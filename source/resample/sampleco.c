#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "csf.h"
#include "csftypes.h"	/* SET_MV_REAL8 */
#include "misc.h"	/* ChkRealloc */
#include "geometry.h"	/* Intersect, PointInPolygon, AreaPolygon */
#include "mathx.h"	/* ceil, floor, ScaleRad */
#include "point.h"	
#include "app.h"	

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "sample.h"	

/***************/
/* EXTERNALS   */
/***************/
extern size_t rasterSize;

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
static RASTER *raster;

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 
typedef struct DATA{
	REAL8 value;		/* value of cell */
	double area;		/* area of overlap in- & outputcell */
}DATA;

/******************/
/* IMPLEMENTATION */
/******************/
/* Calculate weighted mean of directional data sample.
 * Input and result are in radians.
 * Returns weighted mean of sample.
 */
static double WeightDirectionalMean(
	double *totalWeight,	/* write-only total of weights */
	const DATA *list, 	/* array of n samples, in radians */
	size_t n)			/* samples size */
{
	double	tC, tD, tS, tS2, tC2, R, meanIn;
	size_t	i;

	*totalWeight = 0;
	tC = tS = tD = tC2 = tS2 = 0;

	for(i = 0; i < n; i++)
	{
	    tC += (double) list[i].area * cos((double) list[i].value);
	    tS += (double) list[i].area * sin((double) list[i].value);
	    tC2+= (double) list[i].area * cos((double) list[i].value * 2);
	    tS2+= (double) list[i].area * sin((double) list[i].value * 2);
	    *totalWeight += (double) list[i].area;
	}

	if(*totalWeight == 0)
		return 0;		/* no values to take mean */

	tC /= *totalWeight;
	tS /= *totalWeight;
	R = sqrt(tC * tC + tS * tS);
	meanIn = atan2(tS / R, tC / R);

	/* atan2 -> [-PI, PI], this has to be : [0, 2 * PI> */
	meanIn = ScaleRad(meanIn);

	return meanIn;
}

/* Calculates the output value according to areas and values.
 * Determines the weighted directional mean of the values.
 */
 static void CalcDirectionOut(
 	MAP *out,		/* write-only output map */
 	size_t rOut,		/* row of pixel to calculate */
 	size_t cOut,		/* column of pixel */
 	size_t nrList,		/* length of list */
 	const DATA *list,	/* list of values & areas */
 	size_t nrCoverCells,	/* min. nr. of non-MV cells for non MV */
 	size_t nrMaps)		/* number of input maps */
{
	double 	cover;		/* percentage of cell being covered */
	REAL8 	outVal = 0;	/* the calculated output value */
	BOOL 	first = FALSE;	/* initialization of outVal */
	double 	area = 0;	/* The sum of areas */
	REAL8	cellSize = RgetCellSize(out);

	/* Take the weighted sum of the values, weight is the area */
	outVal = (REAL8) WeightDirectionalMean(&area, list, nrList);

	if(area == 0)
		first = TRUE;	/* no cell covers output cell */

	/* Determine whether or not missing value for output pixel */
	if(nrMaps > 1 && nrCoverCells > 0)
		cover = DetNrCoverCells(raster);
	else
		cover = area * rasterSize * rasterSize/ 
			(cellSize * cellSize);

	if(first || ceil(cover) < nrCoverCells || cover == 0)
 		SET_MV_REAL8(&outVal);		/* MV */
	RputCell(out, rOut, cOut, &outVal); 
}

/* Calculates the output value according to areas and values.
 * Determines the weighted mean of the values.
 */
 static void CalcScalarOut(
 	MAP *out,		/* write-only output map */
 	size_t rOut,		/* row of pixel to calculate */
 	size_t cOut,		/* column of pixel */
 	size_t nrList,		/* length of list */
 	const DATA *list,	/* list of values & areas */
 	size_t nrCoverCells,	/* min. nr. non-MV cells for non MV */
 	size_t nrMaps)		/* number of input maps */
{
	size_t 	i;
	double 	cover;		/* percentage of cell being covered */
	REAL8 	outVal = 0;	/* the calculated output value */
	BOOL 	first = FALSE;	/* initialization of outVal */
	double 	area = 0;	/* The sum of areas */
	REAL8	cellSize = RgetCellSize(out);
	extern  int opMax;

	if (opMax == 1)
	{
		 first = TRUE;
	         for(i = 0; i < nrList; i++)	
	          if (list[i].area > 0)
	          {
	          	if (first)
	          	 outVal = list[i].value;
	          	else
		         outVal = MAX(outVal, list[i].value);
		        first = FALSE;
		        area += list[i].area;  	/* sum of areas */
	          }
	}
	else
	{
	 /* Take the weighted sum of the values, weight is the area */
	 for(i = 0; i < nrList; i++)	
	 {
		area += list[i].area;  	/* sum of areas */
		outVal += list[i].value * list[i].area;
	 }	
	 if(area != 0)
		outVal /= area;		/* average */
	 else
		first = TRUE;		/* no cell covers output cell */

	}
	/* Determine whether or not missing value for output pixel */
	if(nrMaps > 1 && nrCoverCells > 0)
		cover = DetNrCoverCells(raster);
	else
		cover = area * rasterSize * rasterSize/ 
			(cellSize * cellSize);
	if(first || ceil(cover) < nrCoverCells || cover == 0)
 		SET_MV_REAL8(&outVal);		/* put mv */
	RputCell(out, rOut, cOut, &outVal); 
}

/* Adds cell to list.
 * Returns 1 in case of an error, 0 otherwise.
 */
static int AddCell(
	DATA **list,		/* read-write list of cells */
	RASTER *raster,		/* read-write raster to modify */
	size_t *nrList,		/* read-write number of items in list */
	const POINT2D *inputCell,	/* polygon of input cell */
	const POINT2D *outputCell,/* polygon of output cell */
	const REAL8 *currRow,	/* current row */
	const MAP *in,		/* input map */
	size_t nrMaps,		/* nr. of input maps */
	size_t c,			/* current column number */
	size_t nrCoverCells,	/* nr. of non-MV cells for coverage */
	BOOL aligned,		/* maps are aligned */
	REAL8 angle)		/* angle of output map */
{
	REAL8 value;
	(*nrList)++;
	if((*list = (DATA *)ChkRealloc(*list, *nrList * sizeof(DATA)))
	   == NULL)
		return 1;			

	value = currRow[c];

	if(!IsMV(in, &value))
	{
		double area;	/* area of overlap */
		area = CalcArea(inputCell, outputCell, aligned);
		if(area != 0 && nrCoverCells > 0 && nrMaps > 1)
			ModRaster(raster, outputCell, inputCell, angle);
		(*list)[*nrList - 1].area = area;
		(*list)[*nrList - 1].value = value;
	}
	else
	{	/* no valid value */
		(*list)[*nrList - 1].area = 0;
		(*list)[*nrList - 1].value = 0;
	}
	return 0;
}

/* Calculates one pixel from output map, given the input maps.
 * Puts all values of input pixels in a list and determines the value of
 * the output pixel according to these values and the overlapping areas.
 * Returns 0 if no error occurs, 1 otherwise.
 */
static int CalcPixel(
	MAP *out,		/* write-only output map */
 	MAP **in,		/* read-only list input maps */
 	size_t nrCoverCells,	/* min. nr. non-MV cells for non-MV */
 	size_t nrMaps,		/* nr. of input maps */
 	double rOut,		/* row number pixel */
 	double cOut,		/* column number pixel */
 	BOOL aligned,		/* maps are aligned */
 	REAL8 angle)		/* angle of output map */
{		
	PTYPE 	tlX, tlY, trX, trY, brX, brY, blX, blY;
	double 	r, c;
	DATA 	*list = NULL;  	/* areas and values of input cells */
   	size_t 	i, nrList = 0;	/* number of items in list */
	POINT2D 	*outputCell;	/* polygon of output cell */
   	size_t 	nr = 4;		/* nr of points of cell */
   	CSF_VS	vs;		/* value scale of first input map */

	if(nrCoverCells > 0 && nrMaps > 1)
		raster = InitRaster(raster); /* initialize the raster */

   	/* Determine the four corners of output pixel */
   	RrowCol2Coords(out, rOut, cOut, &tlX, &tlY); /* top left */
   	RrowCol2Coords(out, rOut, cOut + 1, &trX, &trY); /* top right */
   	RrowCol2Coords(out, rOut + 1, cOut, &blX, &blY); /* bottom left */
   	RrowCol2Coords(out, rOut + 1, cOut + 1, &brX, &brY); /* bottom right */
	outputCell = PutInPol(tlX, tlY, trX, trY, brX, brY, blX, blY);
	if(outputCell == NULL)
		return 1;

	POSTCOND(outputCell[0].x == outputCell[nr].x);
	POSTCOND(outputCell[0].y == outputCell[nr].y);

	/* Get pixel on every input map */
	for(i = 0; i < nrMaps; i++)
	{
		MAP 	*X = in[i];	/* input map number i */
		PTYPE 	tlC, tlR, trC, trR, brC, brR, blC, blR;
		PTYPE 	tlX2, tlY2, trX2, trY2, brX2, brY2, blX2, blY2;
   		double 	leftB, belowB, rightB, upperB;	/* boundaries */

		/* Corners: (tlX, tlY), (trX, trY), (blX, blY) and
		 * (brX, brY). Translate for input map.
		 */
   		Rcoords2RowCol(X, tlX, tlY, &tlC, &tlR); /* top left */
   		Rcoords2RowCol(X, trX, trY, &trC, &trR); /* top right */
   		Rcoords2RowCol(X, blX, blY, &blC, &blR); /* bottom left */
   		Rcoords2RowCol(X, brX, brY, &brC, &brR); /* bottom right */

   		/* Boundaries in the input map */
   		rightB = ceil(MaxPoint(tlR, trR, blR, brR));	
   		belowB = ceil(MaxPoint(tlC, trC, blC, brC));	
   		leftB = floor(MinPoint(tlR, trR, blR, brR));	
   		upperB = floor(MinPoint(tlC, trC, blC, brC));	
   		
		PRECOND(upperB <= belowB);
		PRECOND(leftB <= rightB);

		/* Check all cells between the boundaries */
		for(r = upperB; r < belowB; r++)
		{
			REAL8 *currRow;
			if(0 <= r && r <= RgetNrRows(X))
		 	 currRow = (REAL8 *)CacheGetRow(in, i,  r);

			for(c = leftB; c < rightB; c++)
		 	{  /* Cells that might be in pixel */
   			    POINT2D *inputCell;  /* polygon input cell */

			    if(r < 0 || RgetNrRows(X) <= r || c < 0 ||
			       RgetNrCols(X) <= c)
			    	continue;

   			    /* Top left & right, bottom left & right */
   			    RrowCol2Coords(X, r, c, &tlX2, &tlY2);
   			    RrowCol2Coords(X, r, c+1, &trX2, &trY2);
   			    RrowCol2Coords(X, r+1, c, &blX2, &blY2);
   			    RrowCol2Coords(X, r+1, c+1, &brX2, &brY2);
			    inputCell = PutInPol(tlX2, tlY2, trX2, trY2,
					brX2, brY2, blX2, blY2);
			    if(inputCell == NULL)
				return 1;

			    POSTCOND(inputCell[0].x == inputCell[nr].x);
			    POSTCOND(inputCell[0].y == inputCell[nr].y);

			    /* Add item to list for cell */
			    if(AddCell(&list, raster, &nrList, inputCell,
				  outputCell, currRow, X, nrMaps,
				   (size_t)c, nrCoverCells, aligned, angle))
				      return 1;
			    Free(inputCell);	/* deallocate */
		 	}
		}
	}	 

	/* calculate output value of pixel according value scale */
	vs = RgetValueScale(in[0]);
	if(vs != VS_DIRECTION)
		CalcScalarOut(out, (size_t)rOut, (size_t)cOut, nrList, list, nrCoverCells, nrMaps);
	else
		CalcDirectionOut(out, (size_t) rOut, (size_t) cOut, nrList, list, nrCoverCells, nrMaps);

	Free(outputCell);		/* deallocate */
	Free(list);			/* deallocate */
	return 0;			/* successfully terminated */
}

/* Resamples N input maps into 1 output map.
 * For every pixel in the output map all input maps are scanned.
 * The output value of the pixel is determined according to the
 * cells of the input maps at the pixel location. With an optional
 * percentage a minimum coverage area for a non MV can be given.
 * Returns 0 if no error occurs, 1 otherwise.
 */ 
int SampleCont(
	MAP *out,		/* write-only output map */
 	MAP **in,		/* read-only list input maps */
 	double percentage,	/* min. percentage for non-MV */
 	size_t nrMaps,		/* number of input maps */
 	size_t nrRows,		/* number of rows */
 	size_t nrCols,		/* number of columns */
 	BOOL aligned,		/* maps are aligned */
 	REAL8 angle)		/* angle of output map */
 {		
	double r, c;
	size_t nrCoverCells = (size_t)ceil((percentage / 100) * rasterSize
					* rasterSize);
	InitCache(out, in, nrMaps);
	for(r = 0; r < nrRows; r++) 	
	{

		if(nrMaps > 1 && percentage > 0)
	 		raster = NewRaster(nrCoverCells, rasterSize);

	 	/* Print progress information if wanted */
	 	AppRowProgress((int) r);

	 	for(c = 0; c < nrCols; c++) 	
	 	{   /* For every output cell */
		    if(CalcPixel(out, in, nrCoverCells, nrMaps, r, c, aligned, angle)) 
			return 1;	/* allocation failed */
	 	}
	}

	AppEndRowProgress();
	if(nrMaps > 1 && percentage > 0)
		FreeRaster(raster);
	FreeCache(nrMaps);
	return 0;			/* successfully terminated */
 }
