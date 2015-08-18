#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#ifndef INCLUDED_MATHX
#include "mathx.h"   // ScaleRad, hypot
#define INCLUDED_MATHX
#endif
#include "geometry.h"
#include "misc.h"
#include "calc.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/



static  void AdvancePoint(
	POINT2D *p,
	int     dir,
	const LINE *l)
{
	const POINT2D select[4] = { /* see notes RIVM-14 */
		{ 1 , 0 },
		{ 0 , 1 },
		{-1 , 0 },
		{ 0 ,-1 }};
	AddPoint(p,select+dir);
	if (dir % 2) /* s->x == 0 */
		p->x = XgivenY(l,p->y);
	else 
		p->y = YgivenX(l,p->x);
}

static void ComputeTangent(
	MAP_REAL8    *result,
	REAL8     viewAngleVal,
  const MAP_REAL8 *dem,
        int ry,
	int cx)
{
	double a,bestM,bestZ,startZ; /* bestM = tan(angle) */
	POINT2D p,bestPoint;
	LINE l;
	/* incr/decr on x/y is chosen on the split of the 45 degrees 
	 * (is M_PI/4) line
	 */
	int dir = (int)(ScaleRad(viewAngleVal-(M_PI/4))/(M_PI/2));
	POSTCOND(dir >= 0 && dir < 4);
	a = ScaleRad((M_PI/2)-viewAngleVal); /* angle with Y > 0 --> x > 0	 */

	p.x=cx+0.5;
	p.y=ry+0.5;
	if ( fabs(fmod(a-(M_PI/2),M_PI)) < 0.0001 ) {
		/* almost 180 or 360 */
		l.parY = TRUE;
		l.xInt = p.x;
	} else {
		l.slope = -tan(a);
		l.parY = FALSE;
		l.yInt = p.y - l.slope * p.x; 
	}

	dem->Get(&startZ,ry,cx, dem);
	bestZ= -REAL8_MAX;
	bestM= -REAL8_MAX;
	bestPoint = p;

	AdvancePoint(&p,dir,&l);
	while(dem->Get(&a, (int)floor(p.y),(int)floor(p.x), dem)) { 
		if (a > bestZ)
		{
			double newM = (a-startZ);
			if (newM > bestM)
			{
			 newM /= 
#ifdef _MSC_VER
       _hypot
#else
       hypot
#endif
       (cx-floor(p.x),ry-floor(p.y))*Side();
			 if (newM > bestM) {
			 	bestM = newM;
				bestZ = a;
				bestPoint = p;
			 }
			}
		}
		AdvancePoint(&p,dir,&l);
	}
	if ( (int)floor(bestPoint.x) == cx &&
	     (int)floor(bestPoint.y) == ry )
		result->PutMV(ry,cx, result);
	else
		result->Put(bestM, ry,cx, result);
}


extern int HorizonTangent(
	MAP_REAL8 *result, /* angle in degrees */
  const MAP_REAL8 *dem, /* */
  const MAP_REAL8 *viewAngle) 
{
	REAL8 	demVal,viewAngleVal;			/* value read in dem.map */
     	int 	nrRows, nrCols, r, c;

     	dem->SetGetTest(GET_MV_TEST, dem);
     	viewAngle->SetGetTest(GET_MV_TEST, viewAngle);
     	nrRows = dem->NrRows(dem);
     	nrCols = dem->NrCols(dem);

	result->PutAllMV(result);

	/* For every cell in the dem map calculate the slope. */
     	for (r = 0; r < nrRows; r++)
	{
		for (c = 0; c < nrCols; c++)
		 if(dem->Get(&demVal, r, c, dem) &&
		    viewAngle->Get(&viewAngleVal, r, c, viewAngle) &&
		    viewAngleVal != -1 )
		 {
			ComputeTangent(result,viewAngleVal,dem,r,c);
		 }
		 else {
		 	result->PutMV(r, c, result);
		}
     	}
     	return 0;
}
