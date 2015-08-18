#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h"	/* sqrt, sqr */
#include "misc.h"
#include "calc.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "delta.h"	/* CalcDeltaXY */

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

/* Calculates the slope for each cell in degrees [0, 360].
 * Assumes a dem map present results in a slope map.
 * Returns 0.
 */
int Slope(
     	MAP_REAL8 *slope,       /* Read-write slope output map  */ 
     	const MAP_REAL8 *dem)	/* Digital Elevation Model */
{
	REAL8 	demVal;			/* value read in dem.map */
     	int 	nrRows, nrCols, r, c;

     	dem->SetGetTest(GET_MV_TEST, dem);
     	nrRows = dem->NrRows(dem);
     	nrCols = dem->NrCols(dem);

	/* For every cell in the dem map calculate the slope. */
     	for (r = 0; r < nrRows; r++)
	{
		for (c = 0; c < nrCols; c++)
		 if(dem->Get(&demVal, r, c, dem)) 
		 {
			REAL8 Dx,Dy,G;
			CalcDeltaXY(&Dx, &Dy,dem, r, c);
			G = sqrt(sqr(Dx)+sqr(Dy)); 
			slope->Put(G, r, c, slope);
		 }
		 else
			slope->PutMV(r, c, slope);
     	}
     	return 0;
}

/* calculate sum of 4 neigbours
 * return 0
 */
int Window4total(
     	MAP_REAL8 *out,       /* Read-write output map  */ 
     	const MAP_REAL8 *in)	/* in */
{
     	int 	nrRows, nrCols, r, c;
     	const int     lddCodes[] = {2,4,6,8};

     	in->SetGetTest(GET_MV_TEST, in);
     	nrRows = in->NrRows(in);
     	nrCols = in->NrCols(in);

     	for (r = 0; r < nrRows; r++)
	for (c = 0; c < nrCols; c++)
	{
	  REAL8 inVal,sum=0;
	  int i,nr=0;
	  for (i=0;i<4; i++)
	  {
	   int nbR=DownStrR(r,lddCodes[i]);
	   int nbC=DownStrC(c,lddCodes[i]);
	   if(in->Get(&inVal, nbR, nbC, in)) 
		 {
		 	sum+=inVal;
		 	nr++;
		 }
	  }
	  if (nr==0)
		out->PutMV(r, c, out);
	  else
		out->Put(sum,r, c, out);
	}
     	return 0;
}
