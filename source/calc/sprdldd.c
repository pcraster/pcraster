#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"
#include "mathx.h"

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
/* Spreads down from one point.
 * For this point its first downstream element is taken.
 * This is repeated until a downstream element is found with
 * already a lower cost output value in the outCost map.
 * Returns nothing, just modifies the output maps.
 */
static void SpreadDown(
     	MAP_REAL8 *outCost,		/* write-only output map  */
     	MAP_INT4 *outId,		/* read-write output map */
     	const MAP_UINT1 *ldd, 		/* ldd map	*/
     	const MAP_INT4 *points,		/* points	*/
     	const MAP_REAL8 *cost,		/* initial costs */
     	const MAP_REAL8 *friction,	/* friction */
     	int rowNr,			/* point from which is spread */
     	int colNr)			/* point from which is spread */
{
	REAL8 	costVal1, fricVal1;
	UINT1 	lddVal1;
	INT4 	id1, pointVal1;
     	int 	r = rowNr;
     	int 	c = colNr;
	REAL8	diagonal = Diagonal();
	REAL8	side = Side();

	PRECOND(ldd->GetGetTest(ldd) == GET_MV_TEST);
	PRECOND(outCost->GetGetTest(outCost) == GET_MV_TEST);
	PRECOND(outId->GetGetTest(outId) == GET_MV_TEST);
	PRECOND(friction->GetGetTest(friction) == GET_MV_TEST);
	PRECOND(points->GetGetTest(points) == GET_MV_TEST);
	PRECOND(cost->GetGetTest(cost) == GET_MV_TEST);

     	while(ldd->Get(&lddVal1, r, c, ldd)&&
     	(points->Get(&pointVal1, r, c, points))&&
     	(cost->Get(&costVal1, r, c, cost))&&
	(friction->Get(&fricVal1, r, c, friction))&&
	(lddVal1 != LDD_PIT))
	{
		REAL8 costVal2, fricVal2;
		UINT1 lddVal2;
		INT4 pntVal2, id2;
		int rNext = RNeighbor(r, lddVal1);  /* downstream elt.*/
		int cNext = CNeighbor(c, lddVal1);

		PRECOND(outId->Get(&id1, r, c, outId));

		/* Check on MV in input maps */
		if(friction->Get(&fricVal2, rNext, cNext, friction)&&
		(cost->Get(&costVal2, rNext, cNext, cost)) &&
		(ldd->Get(&lddVal2, rNext, cNext, ldd)) &&
		(outId->Get(&id2, rNext, cNext, outId)) &&
		(outId->Get(&id1, r, c, outId)) &&
		(outCost->Get(&costVal1, r, c, outCost)) &&
		(points->Get(&pntVal2, rNext, cNext, points)))
		{
			REAL8 fricVal = ((fricVal1 + fricVal2) / 2);	
			REAL8 totalcost;

			/* distance (so total cost also) depends on 
			 * neighbor being a corner neighbor or not .
			 */
			if(Corner(lddVal1))
			{
			 	if(appUnitTrue)
					totalcost = fricVal * diagonal;
				else
					totalcost = fricVal * 
							sqrt((REAL8) 2);
			}
			else
			{
			 	if(appUnitTrue)
					totalcost = fricVal * side;	
				else
					totalcost = fricVal;
			}
			totalcost += costVal1; /* cost current cell */

			if((id2 == 0) |
			(outCost->Get(&costVal2, rNext, cNext, outCost) &&
			(totalcost < costVal2)))
			{	
			/* this path is first path or cheaper path */			
				outCost->Put(totalcost, rNext, cNext, outCost);
				outId->Put(id1, rNext, cNext, outId);
				r = rNext;
				c = cNext;
			}	
			else
				return; /* old path was cheaper, stop */
		}
		else
			return;		/* MV in input, stop */	
	}
     	return;
}

/* Spreads along the ldd from each nonzero point in points map.
 * Calculates the costs for each cell along the path from a nonzero
 * point.
 * .B 
 * If the ldd map contains a cycle the program may not terminate.
 * Returns 0.
 */
int SpreadLdd(
     MAP_REAL8 *outCost,		/* write-only output map  */
     MAP_INT4 *outId,			/* read-write output map  */
     const MAP_UINT1 *ldd, 		/* ldd map		*/
     const MAP_INT4 *points,		/* points	*/
     const MAP_REAL8 *cost,		/* initial costs */
     const MAP_REAL8 *friction)		/* friction */
{
     	REAL8 	initCostVal, fricVal;
     	UINT1 	lddVal;
     	INT4 	pntVal;
     	int 	r, c ,nrRows, nrCols;

     	nrRows = ldd->NrRows(ldd);
     	nrCols = ldd->NrCols(ldd);

	/* algorithm wants map->Get() to return FALSE in case of a MV */
	ldd->SetGetTest(GET_MV_TEST, ldd);
	points->SetGetTest(GET_MV_TEST, points);
	friction->SetGetTest(GET_MV_TEST, friction);
	cost->SetGetTest(GET_MV_TEST, cost);
	outId->SetGetTest(GET_MV_TEST, outId);
	outCost->SetGetTest(GET_MV_TEST, outCost);

	/* Fill outIdBuf with 0, this is the initial value */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	{
		if(ldd->Get(&lddVal, r, c, ldd)&&
		(points->Get(&pntVal, r, c, points))&&
		(friction->Get(&fricVal, r, c, friction))&&
		(cost->Get(&initCostVal, r, c, cost)))
		{
			if (fricVal < 0)
			 return	RetError(1,"spreadldd: Domain error on parameters");
			if(pntVal == 0)	
			{	
				outId->Put(0, r, c, outId);
				outCost->PutMV(r, c, outCost);
			}	
			else	
			{	
				outCost->Put(initCostVal, r, c, outCost);
				outId->Put(pntVal, r, c, outId);
			}	
		}	
	   	else
	   	{	
	   		outCost->PutMV(r, c, outCost);
	   		outId->PutMV(r, c, outId);
	   	}
	}	

	/* For every nonzero point in the pointmap perform the 
	 * spread function.
	 */
	for(r = 0; r < nrRows; r++)
	{
	 	AppRowProgress(r);
	 	for(c = 0; c < nrCols; c++)
		{	
			if(outCost->Get(&initCostVal, r, c, outCost))
  				SpreadDown(outCost, outId, ldd, points,
  						cost, friction, r, c);
     				
		}
	}
	AppEndRowProgress();
	return 0;			/* successful terminated */ 
}
