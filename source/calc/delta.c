#include "stddefx.h" 


/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"

/* global header (opt.) and delta's prototypes "" */
#include "delta.h"

/* headers of this app. modules called */ 
# include "mathx.h" 	/* pow(x, y)  */

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

/* gert a pixel , if a pixel is mv or outside border,
 * then the  average of all pixels in it's 3*3 window are calculated
 */
static REAL8 GetVal(
        const MAP_REAL8 *dem,	/* Digital Elevation Model */
	int r,		/* row cell to calculate */
	int c)		/* column cell to calculate */
{
	int 	i,n=0;
	REAL8 	newVal;		/* value for invalid value */

	 if(dem->Get(&newVal, r, c, dem))
		return newVal;
	newVal=0;
	/* Determine the three closest cells. */
	for(i = 1; i <= NR_LDD_DIR; i++)
	{
		REAL8 v;
		int 	rNext = RNeighbor(r, i);
		int 	cNext = CNeighbor(c, i);
		if (i == LDD_PIT)
			continue;

		if (dem->Get(&v, rNext, cNext, dem))
		{
			newVal += v;
			n++;
		}
	}
	POSTCOND(n >= 1); /* centre cell is always defined */
	return newVal/n;
}

/* See skidmore, 1989 */
void CalcDeltaXY(
		double *dzDx, /* write-only, difference in X */
		double *dzDy, /* write-only, difference in Y */
		const MAP_REAL8 *dem, 	/* Digital elevation map */
		int r,			/* row nr. center cell */
		int c)			/* col nr. center cell */
{
	int jc,ir;
	REAL8 z[3][3];
#define Z(ir,jc)   (z[(ir)+1][(jc)+1])
	REAL8 d = 8 * Side();

	for(ir=0; ir < 3; ir++)
	 for(jc=0; jc < 3; jc++)
	   z[ir][jc]  = GetVal(dem, r-1+ir, c-1+jc);

	*dzDy = ( (Z( 1, 1) + 2*Z( 1, 0) + Z( 1,-1)) -
                  (Z(-1, 1)+  2*Z(-1, 0) + Z(-1,-1) ) )/d;

/*
	*dzDx = ( (Z( 1, 1) + 2*Z( 0, 1) + Z(-1,+1)) -
                  (Z(+1,-1)+  2*Z( 0,-1) + Z(-1,-1) ) )/d;
 */
	*dzDx = ( 
                  (Z(+1,-1)+  2*Z( 0,-1) + Z(-1,-1) )
	         -
	          (Z( 1, 1) + 2*Z( 0, 1) + Z(-1,+1))
                   )/d;
#undef Z
}

/* Zevenbergen grid
 *     Z1 Z2 Z3
 *     Z4 Z5 Z6
 *     Z7 Z8 Z9
 */
void ZevenbergenGrid(
	REAL8 *Z,
	const MAP_REAL8 *dem,
	int r,
	int c)
{
	Z[1] = GetVal(dem, r-1, c-1);
	Z[2] = GetVal(dem, r-1, c  );
	Z[3] = GetVal(dem, r-1, c+1);
	Z[4] = GetVal(dem, r  , c-1);
	Z[5] = GetVal(dem, r  , c  );
	Z[6] = GetVal(dem, r  , c+1);
	Z[7] = GetVal(dem, r+1, c-1);
	Z[8] = GetVal(dem, r+1, c  );
	Z[9] = GetVal(dem, r+1, c+1);
}

