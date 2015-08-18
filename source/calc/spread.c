#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"	/* appUnitTrue, appOutput */
#include "mathx.h"	/* sqrt */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "p_calc_list.h"	/* RemFromList */

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 
static NODE *firstOfList=NULL;		/* first element of list */
static  unsigned char **inList = NULL;

/******************/
/* IMPLEMENTATION */
/******************/

/* Puts element in front of the list.
 * Gets the original list and element to add.
 * Returns new list.
 */
 static NODE* AddToList(NODE *list,	/* write-only original list */
 			int row,	/* row from cell to add */
 			int col)	/* column from cell to add */
 {			
 	NODE *c;
#ifdef NEVER 
 	if(InList(list, row, col)) /* CW not neccessary when 
 	                            * building initial list
 	                            */
 		return list;
# endif
 	if (Set1BitMatrix(inList, row,col)) /* already in list */
 		return list;
 	c = NewNode(row,col);
 	if(c == NULL)
 	{
 		list = FreeList(list);
 		return NULL;		/* allocation error */
 	}
 	/* initialize node, row,col set in NewNode */
 	c->prev = NULL;
 	if(list == NULL)
 		list = firstOfList = c;	/* first element in list */
 	else
 	{
 		firstOfList->prev = c;	/* put in front of 1st element */
 		firstOfList = c;
 	}	
 	return list;
 }

/* Calculates the spread value.
 * The value is determined according to friction and the position of the
 * neighbors. If the neighbors are corner neighbors, the average
 * friction is multiplied with the diagonal, else with the side.
 * Returns the spread value.
 */
static REAL8 CalcSpreadValue(
	INT4 *id,			/* write-only id */
	const MAP_REAL8 *outCost,	/* output costs */
	const MAP_INT4 *outId,		/* id map from spread */
	const MAP_REAL8 *friction,	/* friction map */
	int r,				/* row current cell */
	int c,				/* column curr. cell */
	REAL8 f)			/* friction value (r, c) */
{			
	REAL8 	costs, costVal, fricNxt, minCosts;
	int 	i, rNext, cNext;
	INT4 	newId = 0;		/* id spread point of min. cost */

	minCosts = REAL8_MAX;		/* minimum costs until now */
	FOR_ALL_LDD_NBS(i)
	{
		rNext = RNeighbor(r, i);
		cNext = CNeighbor(c, i);
		if(outCost->Get(&costVal, rNext, cNext, outCost) &&
		(friction->Get(&fricNxt, rNext, cNext, friction)))
		{
			costs = (f + fricNxt) / 2;
 			costs *= (Corner(i) == FALSE) SCALE;
			costs += costVal;	
			if(costs < minCosts)
			{ 	/* cheapest path from neighbor to r,c */	
				minCosts = costs;	
				outId->Get(&newId, rNext, cNext, outId);
				*id = newId;
				POSTCOND(*id != 0);
			}	
		}
	}	
	*id = newId;		/* id from cheapest neighbor */
	return minCosts;	/* minimum costs to get to r, c */	
}				

/* Performs the spread function using the breadth-first strategy. 
 * All neighbors from current cell are checked on getting a cheaper path
 * from current cell. All cells in the coordlist are checked. Every time
 * a neighbor is found which gets a new cost value, its neighbors are
 * checked too.
 * Returns 0 if no error occurs, 1 otherwise. 
 */
static int PerformSpread(
	MAP_REAL8 *outCost, 		/* read-write output costs */
	MAP_INT4 *outId,		/* read-write output id map */
	NODE *coordList,		/* read-write list of cells */
	const MAP_REAL8 *friction)	/* friction map */
{
  extern int com_equalEpsilonFloat(float a, float b);
	int 	rNext, cNext, rowNr, colNr, i;
	REAL8 	f;        	/* friction of current cell */
	REAL8 	s, newS;	/* old & new spreadval. of curr. cell */

	while(coordList != NULL)
	{
	 INT4 id;	 /* id of current cell */
	 rowNr = coordList->rowNr; /* cell from which is spread */
	 colNr = coordList->colNr; /* (source cell ) */
	 coordList = RemFromList(coordList);  /* unable */
	 id = Set0BitMatrix(inList,rowNr,colNr);
	 POSTCOND(id != 0); /* was set */

	 FOR_ALL_LDD_NBS(i)
	 {   /* find new spread value for all neighbors */
	  rNext = RNeighbor(rowNr, i);	
	  cNext = CNeighbor(colNr, i);

	  if(friction->Get(&f, rNext, cNext, friction) &&
	 	outId->Get(&id, rNext, cNext, outId))
	  {
	  	INT4 newId = 0;
	  	if(id != 0)
	  	 outCost->Get(&s, rNext, cNext, outCost); /* already visited */

	 	newS = CalcSpreadValue(&newId, outCost,
	 	       outId, friction, rNext, cNext, f);
#ifdef _MSC_VER
#pragma warning( disable : 4244 )
#endif
/* CW this POSSIBLE_DATA_LOSS is very tricky, do not touch! */
	 	if(newId != 0 &&
        (id == 0 || 
        (AppCastREAL4(newS) < s && !com_equalEpsilonFloat(newS,s))))
#ifdef _MSC_VER
#pragma warning( default: 4244 )
#endif
		{
		/* a cheaper or first route to this cell
		 * found, inspect the neighbors too. 
		 */
			coordList = AddToList(coordList, rNext, cNext);
			if(coordList == NULL)
				return 1;
			outCost->Put(newS, rNext, cNext, outCost); /* new costs */
			outId->Put(newId, rNext, cNext, outId); /* new id */
		}
	   }
	}
      }
   return 0;
}

/* Spreads from each nonzero point in points map.
 * First all cells that have a point value > 0, are
 * put in a list. These cells are the points from which is spread.
 * Returns 1 if an error occurs, 0 otherwise.
 *
 * Note: spread is a closure algorithm, do until no changes.
 */
int Spread(
     MAP_REAL8 *outCost,		/* read-write output map  */
     MAP_INT4 *outId,			/* read-write output map  */
     const MAP_INT4 *points,		/* points to be spread */
     const MAP_REAL8 *cost,		/* initial costs */
     const MAP_REAL8 *friction)		/* friction of each cell */
{
     	NODE 	*coordList = NULL;	/* list with cells to be checked */
     	INT4 	pointVal;		/* value in points map of cell */
     	REAL8 	s, f;	/* s = initial cost, f = friction of cell */
     	int 	r, nrRows = points->NrRows(points);
     	int 	c, nrCols = points->NrCols(points);
     	inList = NewBitMatrix((size_t)nrRows,(size_t)nrCols);

     	if (inList == NULL)
     		return 1;
     	SetAllBitMatrix(inList,nrRows, nrCols, 0); /* not in list */

	/* Fill outCostBuf with MV, this is the initial value */
	outCost->PutAllMV(outCost);

	/* Fill outIdBuf with 0, this is the initial value */
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
		outId->Put(0, r, c, outId);

	/* breadth - first */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	 {
	 	if(points->Get(&pointVal, r, c, points) &&
	 	(friction->Get(&f, r, c, friction)))
	 	{	
	 		if (f < 0) 
			 return	RetError(1,"spread: Domain error on parameters");
	 		if(pointVal != 0)
	 		{  /* put spread points in coordlist */	
	 	            if(! cost->Get(&s, r, c, cost)) 
	 	                goto putMv;

	 		    outCost->Put(s, r, c, outCost);
	 		    outId->Put(pointVal, r, c, outId);
	 		    coordList = AddToList(coordList, r, c);
	 			if(coordList == NULL)
	 				return 1;
	 		}		
	 	}	
	 	else
		{	
putMv:
			outId->PutMV(r, c, outId);	
			outCost->PutMV(r, c, outCost);	
		}	
	}	
	if(PerformSpread(outCost, outId, coordList, friction))
		return 1;
	Free2d((void **)inList, (size_t)nrRows);
	return 0;		/* successful terminated */ 
}
