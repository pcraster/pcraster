#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h"    	/* sqrt, pow, atan, M_PI */
#include "misc.h"
#include "calc.h"
#include "delta.h"
#include "app.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "delta.h"		/* CalcDeltaXY */

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

/* The azimuthal direction map is calculated [0, 2pi].
 * Assumes a dem map present returns the directional orient map.
 * Returns 0, and modifies the output orient map.
 */
int Orient(
     	MAP_REAL8 *orient,     	/* Read-write output orient map  */ 
     	const MAP_REAL8 *dem)	/* Digital Elevation Model map */
{
  	REAL8  	demVal;		/* value in the dem.map */
	int 	r, c, nrRows, nrCols;

     	dem->SetGetTest(GET_MV_TEST, dem);
     	nrRows = dem->NrRows(dem);
     	nrCols = dem->NrCols(dem);

	/* For every cell in the dem map calculate the orient. */
    	for(r = 0; r < nrRows; r++)
     	{ 
     	 AppRowProgress(r);
     	 for(c = 0; c < nrCols; c++)
     	 {
		if(dem->Get(&demVal, r, c, dem)) 
		{
			REAL8 Dx,Dy;
			CalcDeltaXY(&Dx,&Dy, dem, r, c);
			if(Dx == 0)  	/* exception value */
			{
				if(Dy == 0) /* Flat -> -1 */
					orient->Put((REAL8) -1, r, c,
							orient);
				if(Dy > 0) /* north: 0 DEGREES */
					orient->Put((REAL8)0, r, c,
							orient);
				if(Dy < 0)  /* south: 180 DEGREES */
					orient->Put(M_PI, r, c, orient); 
			}
			else
			{
				if(Dy == 0)
				{
					REAL8 orientVal = M_PI / (REAL8)2;
					if(Dx < 0) /* west */
					{	 /* 270 DEG */
						orientVal *= 3;
						orient->Put(orientVal,
							r, c, orient);
					} 	
					else 	/* east 90 DEG */
						orient->Put(orientVal, 
						r, c, orient); 
				}
				else
				{
					REAL8 A = atan(Dx / Dy);
					if(Dy < 0)
						A += M_PI; 
					if(Dy > 0 && Dx < 0)
						A +=  2 * M_PI; 
					orient->Put(A, r, c, orient);
				}
			}
		}
		else
			orient->PutMV(r, c, orient);
	 }
	}
     	AppEndRowProgress();
	return 0;
}
