#include "stddefx.h" 
/*
 *
 */

/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"

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

/* Gives each cell the value of its downstream element.  
 * When the cell is a pit, it keeps its own input value.
 * Assumes a spatial ldd map and an amount map.
 */
int DownStream(
     MAP_REAL8 *out,			/* write-only output map  */ 
     const MAP_UINT1 *ldd,		/* input ldd  map */ 
     const MAP_REAL8 *amount)		/* input value map */
{
	UINT1 	lddVal, lddNext;   /* ldd value current and next cell */
	REAL8 	amountVal, ownVal;
	int 	r, c;
	int 	nrRows = ldd->NrRows(ldd);
	int 	nrCols = ldd->NrCols(ldd);

	/* Fill out with missing values. This is the initial value. */
	out->PutAllMV(out);

	/* algorithm wants ldd->Get() to return FALSE if a value is a 
	 * missing value and for the amount map just the same. 
	 */
	ldd->SetGetTest(GET_MV_TEST, ldd);
	amount->SetGetTest(GET_MV_TEST, amount);

	/* for every cell check where it flows to */	
	for(r = 0; r < nrRows; r++)
	{
		AppRowProgress(r);
		for(c = 0; c < nrCols; c++)
		{ 	
		      if(ldd->Get(&lddVal, r, c, ldd) &&
	              (amount->Get(&ownVal, r, c, amount)))
		      {	/* determine cell downstream */
			   int rNext = RNeighbor(r, lddVal);
			   int cNext = CNeighbor(c, lddVal);

			   if(amount->Get(&amountVal, rNext, cNext,
						amount)
			   && (ldd->Get(&lddNext, rNext, cNext,
						ldd)))
			   {	
				if(lddVal != LDD_PIT) 
					out->Put(amountVal, r, c, out);
				else
					out->Put(ownVal, r, c, out);
			   }		
		      }
			/* else-> (r,c) keeps MV as output value */
		}
	}
	AppEndRowProgress();
	return 0;
}
