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
# include "p_calc_list.h" 	/* LinkToList, RemFromList, LinkReal */

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

/* Determines all lowest neighbors and gives them output.
 * All lowest neighbors from current cell are put at the beginning of 
 * the list (depth-first) and the stream is added to their output value.
 * Returns list if successful, NULL when memory allocation failed.
 */
static NODE *DoNeighbors(
	MAP_REAL8 * out,		/* read-write output map */
	NODE *list,			/* read-write list 	 */
	const MAP_REAL8 * dem,		/* dem map */
	const MAP_REAL8 * points, 	/* points map */
	int r,				/* current cell row */
	int c,				/* current cell column */
	REAL8 drainVal)			/* value to drain down */
{	
	NODE 	*list2 = NULL;		/* list of lowest neighbors */
	REAL8 	dropMax = 0;		/* maximal drop value */
	REAL8 	dropVal = 0;		/* maximal drop value */
	int 	i, nrPaths = 0;		/* nr of outgoing paths	*/
	REAL8 	demVal, newDem, outVal, pntVal; /* dem value
	 		* and output value of old and new cell and the
	 		* point value of both to check on MV.
	 		*/

  	PRECOND(dem->GetGetTest(dem) == GET_MV_TEST);
  	PRECOND(points->GetGetTest(points) == GET_MV_TEST);
  	PRECOND(out->GetGetTest(out) == GET_MV_TEST);
	PRECOND(dem->Get(&demVal, r, c, dem));
	PRECOND(out->Get(&outVal, r, c, out));

	dem->Get(&demVal, r, c, dem);	/* height original cell */
	out->Get(&outVal, r, c, out);	/* output original cell */

	for(i = 1; i <= NR_LDD_DIR; i++)
	{	/* check all neighbors */
		int 	rNext = RNeighbor(r, i);
		int 	cNext = CNeighbor(c, i);

		if(dem->Get(&newDem, rNext, cNext, dem) &&   /* no MV */
		points->Get(&pntVal, rNext, cNext, points) &&/* no MV */
		(i != LDD_PIT) &&		  /* skip cell itself */
		(0 < (demVal - newDem)))   /* lower than current cell */
		{
			REAL8 dist = (Corner(i) == FALSE) SCALE;

			dropVal = (demVal - newDem) / dist;
			if(dropMax <= dropVal)
			{
				NODE *tmp;
				if(dropMax < dropVal)
				{
				/* all previous found neighbors
				 * were not the lowest -> reset.
				 */
					list2 = FreeList(list2);
					POSTCOND(list2 == NULL);
					nrPaths = 0;
					dropMax = dropVal;
				}	
				nrPaths++;
				tmp = LinkToList(list2, rNext, cNext);
				if(tmp == NULL)
				{
					FreeList(list2);
					FreeList(list);
					return NULL;
				}
				list2 = tmp;
			}
		}
	}
	drainVal /= nrPaths;	/* divide between steepest paths */
	while(list2 != NULL)
	{ 
		PRECOND(out->Get(&outVal, list2->rowNr, list2->colNr,
								out));
		out->Get(&outVal, list2->rowNr, list2->colNr, out);
		outVal += drainVal;
		out->Put(outVal, list2->rowNr, list2->colNr, out);
		list = LinkChkReal(list, list2->rowNr, list2->colNr,
							drainVal);
		if(list == NULL)
		{
			FreeList(list2);
			return NULL;
		}
		list2 = RemFromList(list2);
	}
	POSTCOND(list != NULL); /* result HasLowerNeighbor was TRUE */
	return list;
}	

/* Checks a cell on having a lower neighbor.
 * If the height of a neighbor is less than the height of the
 * current cell, then the cell has a lower neighbor.
 * Returns TRUE if this is the case, FALSE otherwise.
 */
static int HasLowerNeighbor(
		const MAP_REAL8 *dem,  		/* dem.map */
		const MAP_REAL8 *points,	/* points.map */
		int rowNr,	     /* row number of checked cell */
		int colNr)	  /* column number of checked cell */
{		
	REAL8 demVal, newDem; /* heights original cell and neighbor */
	REAL8 pntVal; /* if MV, then not a valid lower neighbor */
	int   i;

  	PRECOND(dem->GetGetTest(dem) == GET_MV_TEST);
	PRECOND(dem->Get(&demVal, rowNr, colNr, dem));

	dem->Get(&demVal, rowNr, colNr, dem);

	for(i = 1; i <= NR_LDD_DIR; i++)
	{ /* Check all neighbors for being lower */
		int rNext = RNeighbor(rowNr, i);
		int cNext = CNeighbor(colNr, i);
		if(dem->Get(&newDem, rNext, cNext, dem)&&
		points->Get(&pntVal, rNext, cNext, points) &&
		(demVal > newDem))
			return TRUE;	/* has lower neighbor */
	}
	return FALSE; 			/* no neighbor is lower */
}

/* Drains down from each nonzero point.
 * If one of the neighbors has a MV it will get a MV as output as well.
 * All of the lowest neighbors get an extra input from current cell.
 * Returns 1 if memory allocation fails, 0 otherwise.
 */
 static REAL8 DoDrain(
 		MAP_REAL8 *out,		 /* read-write output map */
 		const MAP_REAL8 *dem,	 /* dem map  */	
 		const MAP_REAL8 *points, /* points map  */	
 		int r,			 /* current cell row number */
 		int c)			 /* current cell column nr. */
{
	NODE 	*list = NULL;
	REAL8 	pntVal;			/* value in points.map */
	REAL8 	drainVal;		/* total value to drain down */

	PRECOND(points->Get(&pntVal, r, c, points));

	points->Get(&pntVal, r, c, points);
	list = LinkChkReal(list, r, c, pntVal);
	while(list != NULL)
	{
		int rowNr = list->rowNr;
		int colNr = list->colNr;
		drainVal = list->val.Real;
		list = RemFromList(list);
		if(HasLowerNeighbor(dem, points, rowNr, colNr))
		{	
			list = DoNeighbors(out, list, dem, points,
						rowNr, colNr, drainVal);
			if(list == NULL)
	  			return 1;
		}	
	}
	POSTCOND(list == NULL);
	return 0;
}

/* Determines for each nonzero point its steepest downhill path.
 * Assumes a spatial REAL8 points map and a spatial REAL8 dem map
 * present. If a cell has a MV in one of these maps,
 * it will get a MV in the output map also.
 * The amount that drains down is determined by the points map.
 * Returns 0 if termination is successful, 1 otherwise.
 */
int Drain(
     MAP_REAL8 *out,			/* write-only output map  */ 
     const MAP_REAL8 *dem, 		/* dem map	*/
     const MAP_REAL8 *points) 		/* points map	*/
{
     	REAL8 	pointVal, demVal;
     	NODE 	*pointlist = NULL;
     	int 	r, c , nrRows, nrCols, nrPnts = 0;

	AppProgress("\nnumber of points to do:\n");

     	nrRows = dem->NrRows(dem);
     	nrCols = dem->NrCols(dem);

	PRECOND(nrRows == points->NrRows(points));
	PRECOND(nrCols == points->NrCols(points));

	/* Fill outBuf with 0, this is the initial value */
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
		out->Put((REAL8)0, r, c, out);

	/* algorithm wants dem->Get() and points->Get() to
	 * return FALSE if a value is a missing value
	 */
	dem->SetGetTest(GET_MV_TEST, dem);
	points->SetGetTest(GET_MV_TEST, points);
	out->SetGetTest(GET_MV_TEST, out);

	/* For every true point in the points map, put point in list and
 	 * put point value in output map. The latter is necessary when a
 	 * defined point streams into another defined point.
 	 */
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
	{
		if(dem->Get(&demVal, r, c, dem)&&
		  points->Get(&pointVal, r, c, points))
		{
			if(pointVal != 0)
			{	
				NODE *tmp;
				out->Put(pointVal, r, c, out);
				tmp = LinkToList(pointlist, r, c);
				if(tmp == NULL)
				{
					FreeList(pointlist);
					return 1;
				}
				pointlist = tmp;
				nrPnts++;
			}		
		}
		else
			out->PutMV(r, c, out);
	}

	/* For every true point in the points map do the function */
	while(pointlist != NULL)
	{
		r = pointlist->rowNr;
		c = pointlist->colNr;
		if(DoDrain(out, dem, points, r, c))
		{
			FreeList(pointlist);
			return 1;
		}
		pointlist = RemFromList(pointlist);	
		nrPnts--;
		AppProgress("\r%d                   ", nrPnts);

	}		
	AppEndRowProgress();
	return 0;		/* successful terminated */ 
}
