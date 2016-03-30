#include "stddefx.h"

/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"	/* AppProgress */

/* global header (opt.) and test's prototypes "" */

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
/*
static REAL8 VISMIN = (-REAL8_MAX); Initial value visibility plane 
*/
#define VISMIN  (-REAL8_MAX) /* Initial value visibility plane */

/******************/
/* IMPLEMENTATION */
/******************/

/* Calculates for cell(r,c) the visibility, due to neighbor(rNext,cNext).
 * The visibility plane of neighbor is given, the visibility of the
 * current cell depends on its visibility plane being higher or equal
 * to the visibility of the neighbor, that might block the current cell.
 * Returns nothing, but changes the output map.
 */
static void CalcView(
     MAP_UINT1 *out,		/* write-only output map  	     */ 
     REAL8 *currRow,		/* read-write vis. planes curr. row  */
     REAL8 *lastRow,		/* read-write vis. planes last row   */
     const MAP_REAL8 *dem, 	/* dem map (Digital Elevation Model) */
     const MAP_UINT1 *points, 	/* view points map */
     int r,			/* row of point to check */
     int c,			/* column of point to check */
     int rNext,			/* row of cell that might block */
     int cNext,			/* column  "     "     "     "  */
     REAL8 visPlane,		/* visibility plane blocking neighbor */
     int viewr, 		/* row of view point */
     int viewc,			/* column of view point */
     REAL8 height1)		/* height of cell to check */			
 {
	REAL8 	dist1, vis1, height2, viewHght;
	UINT1 	nextPnt; /* point value of possible blocking neighbor */
  int noMV;

	/* calculate distance from (r, c) to view point */
	dist1 = sqrt((REAL8) (pow((REAL8)(r - viewr),
		(REAL8) 2) + pow((REAL8)(c - viewc), (REAL8) 2)));
	
	noMV = dem->Get(&viewHght, viewr, viewc, dem);
  PRECOND(noMV);
  (void)lastRow; // shut up compiler
  (void)noMV; // shut up compiler

	if(dem->Get(&height2, rNext, cNext, dem) &&
	(points->Get(&nextPnt, rNext, cNext, points)))
	{
		REAL4 tmp;
		if(dist1 != 0)
		/* rounding or truncation causes slight differences */	
			vis1 = (height1 - viewHght) / dist1;	
		else
			vis1 = VISMIN;
		if(vis1 == VISMIN ||
		   visPlane <= (tmp = AppCastREAL4(vis1)))
		{	/* visible, output := TRUE */
			out->Put(1, r, c, out);
			currRow[c] = vis1; /* change visibility plane */
		}	
		else   /* not visible, vis. plane remains unchanged */
			currRow[c] = visPlane;
	}
	else	/* MV -> not able to peer through. */
		currRow[c] = REAL8_MAX;
 }

/* Determines visibility for each cell in the first quadrant.
 * (including edges of quadrant). The possible blocking neighbor is on
 * the right side or at the right-below corner or below the cell.
 * Returns 0 if successful, 1 otherwise
 */
 static int First(
     MAP_UINT1 *out,			/* write-only output map  */ 
     int viewr,				/* row of view point */
     int viewc,				/* column of view point */
     const MAP_REAL8 *dem, 		/* dem map	*/
     const MAP_UINT1 *points) 		/* points map	*/
{ 
	int 	r, c, i;
	UINT1 	pntVal;			/* value in points.map */
	REAL8 	viewHght, visPlane;
	REAL8 	*lastRow, *currRow;	/* vis. planes of last and
					 * current row.
					 */

	PRECOND(dem->GetGetTest(dem) == GET_MV_TEST);
	PRECOND(points->GetGetTest(points) == GET_MV_TEST);
	PRECOND(out->GetGetTest(out) == GET_MV_TEST);
	PRECOND(dem->Get(&viewHght, viewr, viewc, dem));

	/* allocate and initialize the vis. planes of last and
	 * current row.
	 */
	lastRow = (REAL8 *)ChkMalloc(sizeof(REAL8) * (viewc + 1));
	if(lastRow == NULL)
		return 1;
	currRow = (REAL8 *)ChkMalloc(sizeof(REAL8) * (viewc + 1));
	if(currRow == NULL)
	{
		free(lastRow);
		return 1;
	}
	for(i = 0; i <= viewc ; i++)
	{
		currRow[i] = VISMIN;	/* lowest value possible */
		lastRow[i] = VISMIN;	/* lowest value possible */
	}

	visPlane = VISMIN;		/* vis. plane of view point */
	dem->Get(&viewHght, viewr, viewc, dem);	/* viewpoint height */

	/* start from view point to left-upper corner */
	for(r = viewr; 0 <= r; r--)
	{
		 for(c = viewc; 0 <= c; c--)
		 {
		/* Calculate possible blocking neighbor & vis. plane */
			int rNext, cNext; /* possible blocking neighbor */
			REAL8 height1;
			if(c != viewc)
			{
				int rc = (r - viewr) / (c - viewc);
				if(rc < 0.5)
				{ /* take neighbor on the right */
					rNext = r;
					cNext = c + 1;
					visPlane = currRow[c + 1];
				}	
				if(rc > 2) 
				{ /* take neighbor below (r, c) */
					rNext = r + 1;	
					cNext = c;
					visPlane = lastRow[c];
				}	
				if(rc <= 2 && 0.5 <= rc)
				{ /* take right-below corner neighbor */
					rNext = r + 1;
					cNext = c + 1;
					visPlane = lastRow[c + 1];
				}	
			}		
			else
			{  /* rc would cause division by 0 */
				if(viewr == r)
				{
					rNext = r;
					cNext = c;
					visPlane = VISMIN;
				}
				else
				{
					rNext = r + 1;
					cNext = c;
					visPlane = lastRow[c];
				}
			}	

			/* Calculate visibility of current cell */
			if(dem->Get(&height1, r, c, dem) &&
			(points->Get(&pntVal, r, c, points)))
			{	
				CalcView(out, currRow, lastRow, dem,
					points, r, c, rNext, cNext, 
					visPlane, viewr, viewc, height1); 
			}	
			else
			{
				out->PutMV(r, c, out);
 				/* not able to peer through a MV */
				currRow[c] = REAL8_MAX;
			}
		}

		/* This row is done, take next row */
		for(i = 0; i <= viewc; i++)
		{ /* last row := current row */
			lastRow[i] = currRow[i];	
			currRow[i] = 0;		/* initialize */
		}
	}
	free(lastRow);		/* deallocate */
	free(currRow);		/* deallocate */
	return 0;
} 

/* Determines visibility for each cell in the second quadrant.
 * The possible blocking neighbor is on the left side or below
 * each cell or at the left-below corner.
 * Returns 0 if successful, 1 otherwise
 */
 static int Second(
     MAP_UINT1 *out,			/* write-only output map  */ 
     int viewr,				/* row of view point */
     int viewc,				/* column of view point */
     const MAP_REAL8 *dem, 		/* dem map	*/
     const MAP_UINT1 *points, 		/* points map	*/
     int nrCols)			/* number of columns */
{ 
	int 	r, c, i;
	UINT1 	pntVal;
	REAL8 	viewHght, visPlane;
	REAL8 	*lastRow, *currRow;    	/* contains visibility planes of
				   	 * last row c.q. of current row. 
				   	 */

	PRECOND(dem->GetGetTest(dem) == GET_MV_TEST);
	PRECOND(points->GetGetTest(points) == GET_MV_TEST);
	PRECOND(out->GetGetTest(out) == GET_MV_TEST);
	PRECOND(dem->Get(&viewHght, viewr, viewc, dem));

	/* allocate and initialize the last and current row */
	lastRow = (REAL8 *)ChkMalloc(sizeof(REAL8) * (nrCols));
	if(lastRow == NULL)
		return 1;
	currRow = (REAL8 *)ChkMalloc(sizeof(REAL8) * (nrCols));
	if(currRow == NULL)
	{
		free(lastRow);
		return 1;
	}
	for(i = 0; i < nrCols; i++)
	{
		currRow[i] = VISMIN;	/* lowest value possible */
		lastRow[i] = VISMIN;	/* lowest value possible */
	}

	visPlane = VISMIN;
	dem->Get(&viewHght, viewr, viewc, dem);

	if(viewc == nrCols - 1)		/* no second quadrant */
	{
		free(currRow);
		free(lastRow);
		return 0;
	}

	/* start from view point to right upper corner */
	for(r = viewr; 0 <= r; r--)
	{
		 for(c = viewc; c < nrCols; c++)
		 {
		/* Calculate possible blocking neighbor & vis. plane */	
			int rNext, cNext; /* possible blocking neigh. */
			REAL8 height1;
			if(c != viewc)
			{
				int rc = (r - viewr) / (c - viewc);
				if(-0.5 < rc)
				{ /* take neighbor on left */
					rNext = r;
					cNext = c - 1;
					visPlane = currRow[c - 1];
				}	
				if(-2 > rc)
				{ /* take neighbor below */
					rNext = r + 1;	
					cNext = c;
					visPlane = lastRow[c];
				}	
				if(rc <= -0.5 && -2 <= rc)
				{ /* take left-below corner neighbor */
					rNext = r + 1;
					cNext = c - 1;
					visPlane = lastRow[c - 1];
				}	
			}		
			else
			{  /* rc would cause division by 0 */
				if(viewr == r)
				{
					rNext = r;
					cNext = c;
					visPlane = VISMIN;
				}
				else
				{
					rNext = r + 1;
					cNext = c;
					visPlane = lastRow[c];
				}
			}	

		/* Determine visibility of the current cell */
			if(dem->Get(&height1, r, c, dem) &&
			(points->Get(&pntVal, r, c, points)))
			{	
				CalcView(out, currRow, lastRow, dem,
					points, r, c, rNext, cNext, 
					visPlane, viewr, viewc, height1); 
			}	
			else
			{
				out->PutMV(r, c, out);
 				/* not able to peer through a MV */
				currRow[c] = REAL8_MAX;
			}
		}

		/* This row is done, take the next row */
		for(i = 0; i < nrCols; i++)
		{	/* last row := current row */
			lastRow[i] = currRow[i];   /* next row */
			currRow[i] = VISMIN;
		}
	}
	free(lastRow);		/* deallocate */
	free(currRow);		/* deallocate */
	return 0;
} 


/* Determines visibility for each cell in the third quadrant.
 * The possible blocking neighbor is on the right side and/or
 * above each cell.
 * Returns 0 if successful, 1 otherwise
 */

 static int Third(
     MAP_UINT1 *out,			/* write-only output map  */ 
     int viewr,				/* row of view point */
     int viewc,				/* column of view point */
     const MAP_REAL8 *dem, 		/* dem map	*/
     const MAP_UINT1 *points, 		/* points map	*/
     int nrRows)			/* nr columns */
{
	int 	r, c, i;
	UINT1 	pntVal;
	REAL8 	viewHght, visPlane;
	REAL8 	*lastRow, *currRow; /* contains visibility planes of
				     * last row c.q. current row 
				     */

	PRECOND(dem->GetGetTest(dem) == GET_MV_TEST);
	PRECOND(points->GetGetTest(points) == GET_MV_TEST);
	PRECOND(out->GetGetTest(out) == GET_MV_TEST);
	PRECOND(dem->Get(&viewHght, viewr, viewc, dem));

	/* allocate and initialize the last and current row */
	lastRow = (REAL8 *)ChkMalloc(sizeof(REAL8) * (viewc + 1));
	if(lastRow == NULL)
		return 1;
	currRow = (REAL8 *)ChkMalloc(sizeof(REAL8) * (viewc + 1));
	if(currRow == NULL)
	{
		free(lastRow);
		return 1;
	}

	for(i = 0; i <= viewc ; i++)
	{
		currRow[i] = VISMIN; 	/* lowest value possible */
		lastRow[i] = VISMIN;	/* lowest value possible */
	}

	visPlane = VISMIN;
	dem->Get(&viewHght, viewr, viewc, dem);

	if(viewr == nrRows - 1)
	{
		free(currRow);
		free(lastRow);
		return 0;	/* no third quadrant */
	}
	if(viewc == 0)	
	{
		free(currRow);
		free(lastRow);
		return 0;	/* no third quadrant */
	}

	/* start from view point to left-below corner */
	for(r = viewr; r < nrRows; r++)
	{
		 for(c = viewc; 0 <= c; c--)
		 {
			int rNext, cNext; /* possible blocking neigh. */
			REAL8 height1;
			if(c != viewc)
			{

		/* Calculate possible blocking neighbor & vis.plane */ 	
				int rc = (r - viewr) / (c - viewc);
				if(rc > -0.5) 
				{ /* take neighbor on the right */
					rNext = r;
					cNext = c + 1;
					visPlane = currRow[c + 1];
				}	
				if(rc < -2) 
				{ /* take neighbor above (r, c) */
					rNext = r - 1;	
					cNext = c;
					visPlane = lastRow[c];
				}	
				if(-2 <= rc && rc <= -0.5)  
				{ /* take right-above corner neighbor */
					rNext = r - 1;
					cNext = c + 1;
					visPlane = lastRow[c + 1];
				}	
			}		
			else
			{  /* rc would cause division by 0 */
				if(viewr == r)
				{
					rNext = r;
					cNext = c;
					visPlane = VISMIN;
				}
				else
				{
					rNext = r - 1;
					cNext = c;
					visPlane = lastRow[c];
				}
			}	

			/* Determine visibility of the current cell */
			if(dem->Get(&height1, r, c, dem) &&
			(points->Get(&pntVal, r, c, points)))
			{	
				CalcView(out, currRow, lastRow, dem,
					points, r, c, rNext, cNext, 
					visPlane, viewr, viewc, height1); 
			}	
			else
			{
				out->PutMV(r, c, out);
 				/* not able to peer through a MV */
				currRow[c] = REAL8_MAX;
			}
		}

		/* This row is done take the next row */
		for(i = 0; i <= viewc; i++)
		{	/* last row := current row */
			lastRow[i] = currRow[i];	/* next row */
			currRow[i] = 0;
		}
	}
	free(lastRow);		/* deallocate */
	free(currRow);		/* deallocate */
	return 0;
} 

/* Determines visibility for each cell in the fourth quadrant.
 * The blocking neighbor eventually is on the left side and / or
 * above each cell.
 * Returns 0 if successful, 1 otherwise
 */

 static int Fourth(
     MAP_UINT1 *out,			/* write-only output map  */ 
     int viewr,				/* row of view point */
     int viewc,				/* column of view point */
     const MAP_REAL8 *dem, 		/* dem map	*/
     const MAP_UINT1 *points, 		/* points map	*/
     int nrRows,			/* nr rows */
     int nrCols)			/* nr columns */
{ 
	int 	r, c, i;
	UINT1 	pntVal;
	REAL8 	viewHght, visPlane;
	REAL8 	*lastRow, *currRow; /* contains visibility planes of
				   * last row c.q. current row 
				   */

	PRECOND(dem->GetGetTest(dem) == GET_MV_TEST);
	PRECOND(points->GetGetTest(points) == GET_MV_TEST);
	PRECOND(out->GetGetTest(out) == GET_MV_TEST);
	PRECOND(dem->Get(&viewHght, viewr, viewc, dem));

	/* allocate and initialize last and current row */
	lastRow = (REAL8 *)ChkMalloc(sizeof(REAL8) * (nrCols));
	if(lastRow == NULL)
		return 1;
	currRow = (REAL8 *)ChkMalloc(sizeof(REAL8) * (nrCols));
	if(currRow == NULL)
	{
		free(lastRow);
		return 1;
	}
	for(i = 0; i < nrCols ; i++)
	{
		currRow[i] = VISMIN;	/* lowest value possible */
		lastRow[i] = VISMIN;	/* lowest value possible */
	}

	visPlane = VISMIN;
	dem->Get(&viewHght, viewr, viewc, dem);

	if(viewr == nrCols - 1)
	{
		free(currRow);
		free(lastRow);
		return 0;	/* no fourth quadrant */
	}
	if(viewc == nrRows - 1)
	{
		free(currRow);
		free(lastRow);
		return 0;	/* no fourth quadrant */
	}

	/* start from view point to right-below corner */
	for(r = viewr; r < nrRows; r++)
	{
		 for(c = viewc; c < nrCols; c++)
		 {
		/* Calculate blocking neighbor & its visibility plane */
			int rNext, cNext; /* possible blocking neigh. */
			REAL8 height1;
			if(c != viewc)
			{
				int rc = (r - viewr) / (c - viewc);
				if(rc < 0.5)
				{ /* take neighbor on the left */
					rNext = r;
					cNext = c - 1;
					visPlane = currRow[c - 1];
				}	
				if(2 < rc)
				{ /* take neighbor above (r, c) */
					rNext = r - 1;	
					cNext = c;
					visPlane = lastRow[c];
				}	
				if(0.5 <= rc && rc <= 2)
				{ /* take left-above corner neighbor */
					rNext = r - 1;
					cNext = c - 1;
					visPlane = lastRow[c - 1];
				}	
			}		
			else
			{  /* rc would cause division by 0 */
				if(viewr == r)
				{
					rNext = r;
					cNext = c;
					visPlane = VISMIN;
				}
				else
				{
					rNext = r - 1;
					cNext = c;
					visPlane = lastRow[c];
				}
			}	

			/* Calculate visibility current cell */
			if(dem->Get(&height1, r, c, dem) &&
			(points->Get(&pntVal, r, c, points)))
			{	
				CalcView(out, currRow, lastRow, dem,
					points, r, c, rNext, cNext, 
					visPlane, viewr, viewc, height1); 
			}	
			else
			{
				out->PutMV(r, c, out);
 				/* not able to peer through a MV */
				currRow[c] = REAL8_MAX;
			}
		}

		/* this row is done, take next row */
		for(i = 0; i < nrCols; i++)
		{	/* last row := current row */
			lastRow[i] = currRow[i];	/* next row */
			currRow[i] = 0;
		}
	}
	free(lastRow);		/* deallocate */
	free(currRow);		/* deallocate */
	return 0;
} 

/* Determines the visibility for each cell in map.
 * Assumes an UINT1 points map and a spatial REAL8 dem map present. If a
 * cell has a MV in one of the maps, it gets a MV in the output map also.
 * Returns 0 if termination is successful, non-zero otherwise 
 */
int View(
     MAP_UINT1 *out,			/* write-only output map  */ 
     const MAP_REAL8 *dem, 		/* Dig. Elevation map	*/
     const MAP_UINT1 *points) 		/* points map	*/
{
     	UINT1 	pointVal;		/* value in points.map */
     	REAL8 	demVal;			/* value in dem map */
     	int 	r, c , nrRows, nrCols, v = 0;

     	nrRows = dem->NrRows(dem);
     	nrCols = dem->NrCols(dem);

	PRECOND(nrRows == points->NrRows(points));
	PRECOND(nrCols == points->NrCols(points));

	/* Fill out with FALSE, this is the initial value */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	 {
		out->Put((UINT1)0, r, c, out);
	 }

	/* algorithm wants dem->Get() to return FALSE in case of MV */
	dem->SetGetTest(GET_MV_TEST, dem);
	points->SetGetTest(GET_MV_TEST, points);
	out->SetGetTest(GET_MV_TEST, out);

	/* For every view point in the points map */
	AppProgress("\nBusy with viewpoint:\n");
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
	{
		if(dem->Get(&demVal, r, c, dem)&&
		(points->Get(&pointVal, r, c, points))&&
		(pointVal))
	   	{
	   		v++;
	   		AppProgress("\r%d          ", v);
			if(First(out, r, c, dem, points))
				return 1;
			if(Second(out, r, c, dem, points, nrCols))
				return 1;
			if(Third(out, r, c, dem, points, nrRows))
				return 1;
			if(Fourth(out, r, c, dem, points, nrRows,
								nrCols))
				return 1;
		}
	}
	AppEndRowProgress();
	return 0;		/* successful terminated */ 
}
