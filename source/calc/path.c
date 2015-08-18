#include "stddefx.h" 
/*
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

/* Builds path from TRUE point to the pit.
 * Along the path given in the local drain direction, each cell is given
 * TRUE as output value, until the pit is reached. If a MV is detected
 * in one of the input maps, the search for the downstream elements
 * is stopped. The cell itself gets a MV in the output map and the
 * remaining downstream elements will keep a value equal to FALSE.
 * Returns nothing.
 */
 static void BuildPath(
 	MAP_UINT1 *out,			/* write-only output map */
 	const MAP_UINT1 *ldd,		/* ldd map  */	
 	const MAP_UINT1 *points,	/* points map  */	
 	int rPoint,			/* row of nonzero point */
 	int cPoint)			/* column of true point */
 {
	UINT1 	lddVal, pointsVal;
	int	r = rPoint;	/* Point that has TRUE in points.map */
	int 	c = cPoint;

  	PRECOND(ldd->GetGetTest(ldd) == GET_MV_TEST);
  	PRECOND(points->GetGetTest(points) == GET_MV_TEST);

	/* build path until pit is reached or a MV is found */
	while(ldd->Get(&lddVal, r, c, ldd)&&
	points->Get(&pointsVal, r, c, points)&&
	(lddVal != LDD_PIT))
	{
		out->Put(1, r, c, out);
		r = RNeighbor(r, lddVal);
		c = CNeighbor(c, lddVal);
	}	
	if(lddVal == LDD_PIT)
		out->Put(1, r, c, out);	    /* pit */
	else
		out->PutMV(r, c, out);	
}

/* Determines for each TRUE point its downstream path to pit.
 * Assumes a spatial UINT1 points map and a spatial UINT1 ldd map
 * present. If a cell has a MV in one of these maps,
 * it will get a MV in the output map also.
 * .B 
 * If the ldd map contains a cycle the program will not terminate.
 * Returns 0 if termination is successful, non-zero otherwise.
 */
int Path(
     MAP_UINT1 *out,			/* write-only output map  */ 
     const MAP_UINT1 *ldd, 		/* ldd map	*/
     const MAP_UINT1 *points) 		/* points map	*/
{
     	UINT1 	pointVal, lddVal;
     	int 	r, c ,nrRows, nrCols;

     	nrRows = ldd->NrRows(ldd);
     	nrCols = ldd->NrCols(ldd);

	PRECOND(nrRows = points->NrRows(points));
	PRECOND(nrCols = points->NrCols(points));

	/* Fill outBuf with 0, this is the initial value */
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
	{
		out->Put(0, r, c, out);
	}

	/* algorithm wants ldd->Get() and points->Get() to
	 * return FALSE if a value is a missing value
	 */
	ldd->SetGetTest(GET_MV_TEST, ldd);
	points->SetGetTest(GET_MV_TEST, points);

	/* For every true point in the points map do the function   */

	for (r = 0; r < nrRows; r++)
	{
		AppRowProgress(r);
		for (c = 0; c < nrCols; c++)
		{
			if(ldd->Get(&lddVal, r, c, ldd)&&
			points->Get(&pointVal, r, c, points))
			{
				if(pointVal)
					BuildPath(out, ldd, points,
								 r, c);
			}
			else
				out->PutMV(r, c, out);
		}
	}
	AppEndRowProgress();
	return 0;			/* successful terminated */ 
}
