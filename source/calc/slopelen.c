#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h"
#include "misc.h"
#include "calc.h"
#include "app.h"	/* AppProgress, appOutput, appUnitTrue */

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

/* Determines the longest path from cell to ends of catchment.
 * Needs two lists, one to contain the search path, the other to contain
 * the distance from each cell in the search path to the current cell.
 */
static void Longest(
     MAP_REAL8 *out,          	/* read-write output map */
     const MAP_UINT1 *ldd,    	/* ldd map */
     const MAP_REAL8 *friction, /* friction map */
     int cellr,		      	/* row number of cell */
     int cellc)			/* column number of cell */
{
     	REAL8 	fricVal, fricVal2, oldDist, newDist = 0;
     	UINT1 	lddVal;
     	int 	r = cellr;
     	int 	c = cellc;

	/* end of catchment -> slopelength is 0 */
	out->Put((REAL8) 0, r, c, out);

     	while(ldd->Get(&lddVal, r, c, ldd) &&
     	friction->Get(&fricVal, r, c, friction) &&
     	friction->Get(&fricVal2, RNeighbor(r, lddVal),
     		CNeighbor(c, lddVal), friction) &&
     	lddVal != LDD_PIT)
     	{
		REAL8 incrCost;
		REAL8 incrDist;
     		int rNext = RNeighbor(r, lddVal);
     		int cNext = CNeighbor(c, lddVal);
		if(appUnitTrue)
			incrDist = (!Corner(lddVal)) SCALE;
		else
			incrDist = (!Corner(lddVal))? 1: sqrt((REAL8)2);
		incrCost = ((fricVal + fricVal2) / 2);
		newDist += incrDist * incrCost;
		if(out->Get(&oldDist, rNext, cNext, out))
		{
			if(oldDist < newDist)
				out->Put(newDist, rNext, cNext, out);
     		}
     		else
     		{
     			if(ldd->Get(&lddVal, rNext, cNext, ldd))
				out->Put(newDist, rNext, cNext, out);
     		}
     		r = rNext;
     		c = cNext;
     	}
}

/* Determines the length of the longest path to end of catchment.
 * Assumes an UINT1 ldd map present.
 * If there is no pit in the ldd map, then the output map will be
 * filled with missing values.
 * .B
 * If the ldd map contains a cycle the program will not terminate.
 * Returns 0 if succesfull, non-zero otherwise
 */
int Slopelength(
     MAP_REAL8 *out,          	/* Read-write output map  */
     const MAP_UINT1 *ldd,	/* ldd map */
     const MAP_REAL8 *friction)	/* friction map */
{
     	UINT1 	lddVal;
     	int 	r, c, nrRows, nrCols;
     	REAL8	fricVal;

     	ldd->SetGetTest(GET_MV_TEST, ldd);
     	friction->SetGetTest(GET_MV_TEST, friction);
     	out->SetGetTest(GET_MV_TEST, out);

     	nrRows = ldd->NrRows(ldd);
     	nrCols = ldd->NrCols(ldd);

	/* Initialize the output map */
 	out->PutAllMV(out);

	/* For every end cell in the ldd map calculate the longest
 	 * path from cell to end of catchment.
 	 */
     	for(r = 0; r < nrRows; r++)
      	{
		AppRowProgress(r);
      		for(c = 0; c < nrCols; c++)
     		{
        		if( ldd->Get(&lddVal, r, c, ldd) &&
        		    friction->Get(&fricVal, r, c, friction)
        		   )
        		{
        			if (fricVal < 0)
				 return	RetError(1,"slopelength: Domain error on parameters");
				if (NoInput(ldd, r, c))
        	       		  Longest(out, ldd, friction, r, c);
        	        }
     		}
     	}
	AppEndRowProgress();
     	return 0;
}
