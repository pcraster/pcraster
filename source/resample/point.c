#include "stddefx.h" 



/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include <math.h>

/* global header (opt.) and test's prototypes "" */
#include "point.h"

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/**********************/ 
/* LOCAL DEFINITIONS  */
/**********************/ 

/*******************/ 
/* IMPLEMENTATION  */
/*******************/ 

/* Calculates minimum of the row or column coordinates of four corners. 
 * Returns minimum value of row or column coordinate.
 */
PTYPE MinPoint(
	PTYPE tl,		/* top left */
	PTYPE tr,		/* top right */
	PTYPE br,		/* bottom right */
	PTYPE bl)		/* bottom left */
{	
	PTYPE min1, min2;
	if(tr <= tl)
		min1 = tr;
	else
		min1 = tl;	
	if(br <= bl)
		min2 = br;	
	else
		min2 = bl;	
	if(min1 <= min2)
		return min1;	
	else
		return min2;
}

/* Calculates maximum of the row or column coordinates of four corners. 
 * Returns maximum value of row or column coordinate.
 */
PTYPE MaxPoint(
	PTYPE tl,		/* top left */
	PTYPE tr,		/* top right */
	PTYPE br,		/* bottom right */
	PTYPE bl)		/* bottom left */
{	
	PTYPE max1, max2;
	if(tl <= tr)
		max1 = tr;
	else
		max1 = tl;	
	if(br <= bl)
		max2 = bl;	
	else
		max2 = br;	
	if(max1 <= max2)
		return max2;	
	else
		return max1;
}

/* Put 4 points in a POINT2D structure with first point also as fifth.
 * The top left coordinate as first and fifth, the top right coordinate
 * as second, the bottom right coordinate as third and the bottom left
 * coordinate as fourth. This results in a polygon. The order is
 * important, because a different order might produce another polygon.
 * Returns pointer to point list.
 */
POINT2D * PutInPol(
	PTYPE tlX,		/* top left x */
	PTYPE tlY,		/* top left y */
	PTYPE trX,		/* top right x */
	PTYPE trY,		/* top right y */
	PTYPE brX,		/* bottom right x */
	PTYPE brY,		/* bottom right y */
	PTYPE blX,		/* bottom left x */
	PTYPE blY)		/* bottom left y */
{
	POINT2D * cell = (POINT2D *) ChkMalloc(5 * sizeof(POINT2D));
	if(cell == NULL)
		return NULL;
	cell[0].x = tlX;		/* top left x-coordinate */
	cell[0].y = tlY;		/* top left y-coordinate */
	cell[1].x = trX;		/* top right x-coordinate */
	cell[1].y = trY;		/* top right y-coordinate */
	cell[2].x = brX;		/* bottom right x-coordinate */
	cell[2].y = brY;		/* bottom right y-coordinate */
	cell[3].x = blX;		/* bottom left x-coordinate */
	cell[3].y = blY;		/* bottom left y-coordinate */
	cell[4].x = cell[0].x;		/* first point x again */
	cell[4].y = cell[0].y;		/* first point y again */
	return cell;
}

/* Calculates the area from outputCell that is covered by the inputCell.
 * This is done by intersecting both cells and determining the area of
 * the intersection.
 * Returns the area of overlap.
 */
double CalcArea(const POINT2D *inputCell,		/* polygon input cell */
		const POINT2D *outputCell,	/* polygon output cell */
		BOOL aligned)	/* check whether rectangles aligned */
{
	double 	area;			/* area of the overlap */
	POINT2D 	polygon[9];		/* max. nr of points + 1 */
	int 	inNr;			/* nr corners in overlap */

	/* Intersect the two cells */
	if(aligned)
	    inNr = IntersectAllignedRectangles(polygon, outputCell,
							inputCell);
	else
	    inNr = IntersectRectangles(polygon, outputCell, inputCell);
						
	POSTCOND(polygon[0].x == polygon[inNr].x);
	POSTCOND(polygon[0].y == polygon[inNr].y);

	if(inNr > 0)
		area = AreaOfPolygon(polygon, inNr);
	else
		area = 0;		/* no overlap */
	return area;
}

/* Modifies the raster according overlap of in- and output cell.
 * This is done by determining for each raster pixel of the output
 * cell whether it is a point in the input cell.
 * Returns the modified raster.
 */
 void ModRaster(
 	RASTER *raster,			/* read-write raster */
 	const POINT2D *outputcell, 	/* point output cell */
	const POINT2D *inputcell,  	/* point input cell */
	REAL8 angle)			/* angle of output cell */
 {
 	size_t 	i, j;
 	int 	nrPoints = 4;		/* number of points of a cell */
 	double 	stepY, stepX;		/* stepsizes in raster */
 	double  stepSize = 1 / (double) raster->rasterSize;
 	POINT2D  	in[5], out[5];

	PRECOND(raster != NULL);

	for(i = 0; i < ARRAY_SIZE(in); i++)
	{
		in[i] = inputcell[i];
		out[i] = outputcell[i];
		if(angle != 0)
		{
			out[i] = *RotPoint(out+i, angle);
			in[i] = *RotPoint(in+i, angle);
		}
	}

	/* No modification necessary, cell coverage satisfied */
	if(raster->covered)
		return;

 	/* steps to "walk" through raster */
 	stepY = (out[2].y - out[1].y) * stepSize;
 	stepX = (out[1].x - out[0].x) * stepSize;

 	/* Determine for each subpixel whether it is covered */
 	for(i = 0; i < raster->rasterSize; i++)
 	 for(j = 0; j < raster->rasterSize; j++)
 	 {
 	 	POINT2D point[1];		/* subpixel to check */

	 	if(BitSet(raster->field, (int)((i * raster->rasterSize)+ j)) )
	 		continue;

		/* take center of subpixel -> + 05 * step */
 	 	point[0].x = out[0].x + i * stepX + 0.5 * stepX;
 	 	point[0].y = out[0].y + j * stepY + 0.5 * stepY;

 	 	PRECOND(in[0].x == in[nrPoints].x);
 	 	PRECOND(in[0].y == in[nrPoints].y);
 	 	
 	 	if(PointInPolygon(point, in, nrPoints))
 	 	/* subpixel is covered by input cell */	
	 	{
	 		SetBit1(raster->field, ((int)(i * raster->rasterSize + j)));
			raster->count++;
			if(raster->nrCoverCells <= raster->count)
				raster->covered = TRUE;
 	 	}
 	 }
 }

/* Determine the percentage of the outputcell that is covered.
 * The TRUE values of the raster are counted and the percentage
 * is determined.
 * Returns the percentage of coverage.
 */
double DetNrCoverCells(const RASTER *raster)	/* raster to check */
{
	PRECOND(raster != NULL);
	return raster->count;
}

/* Initializes the raster by putting FALSE in every subpixel.
 * Returns nothing, modifies the raster.
 */
RASTER *InitRaster(RASTER *raster) /* write-only raster to initialize */
{
	size_t i, j;

	PRECOND(raster != NULL);

	for(i = 0; i < raster->rasterSize; i++)
	 for(j = 0; j < raster->rasterSize; j++)
	 	SetBit0(raster->field, (int)((i * raster->rasterSize) + j));	
	raster->covered = FALSE;
	raster->count = 0;
	return raster;
}

/* Creates a new raster
 * Returns pointer to raster, NULL in case of an error.
 */
RASTER *NewRaster(
	size_t nrCoverCells,	/* min. nr. of non-MV cells for non-MV */
	size_t rasterSize)		/* size of raster */
{
    RASTER *raster = (RASTER*)(ChkMalloc(sizeof(RASTER)));
    size_t nrExtraBytes = 0;
    if(raster == NULL)
    	return NULL;
    if((rasterSize * rasterSize) % 8 > 0)
    	nrExtraBytes = 1;
    raster->field = (unsigned char *)ChkMalloc((UINT4) 
    		(rasterSize * rasterSize / 8) + nrExtraBytes);
    if(raster->field == NULL)
    {
    	Free(raster);
    	return NULL;
    }
    raster->rasterSize = rasterSize;
    raster->nrCoverCells =  nrCoverCells;
    raster->covered = FALSE;
    raster->count = 0;
    return raster;
}

/* Deletes and deallocates a given raster
 */
void FreeRaster(RASTER *raster)	/* write-delete raster */
{
	Free(raster->field);
	Free(raster);
	raster = NULL;
}

