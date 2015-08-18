#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "csf.h"
#include "misc.h"	/* ChkRealloc */
#include "geometry.h"	/* Intersect, PointInPolygon, AreaPolygon */
#include <math.h>	/* ceil, floor */
#include <string.h>	/* memmove */
#include "point.h"	
#include "table.h"	
#include "app.h"	/* AppRowProgress */	

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "sample.h"	

/***************/
/* EXTERNALS   */
/***************/
extern int opMax; 	/* option for maximum id */
extern size_t rasterSize;

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
static RASTER *raster=NULL;
static size_t nrFast;

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

#define MAXFAST	256

typedef struct DATA{
	INT4 id;		/* id of cell serves as ID */
	double area;		/* area from cell in output cell */
}DATA;

/******************/
/* IMPLEMENTATION */
/******************/

/* Returns id of a given record.
 */
static int ReturnId(DATA *f)	/* record of which the id is wanted */
{
	return (int)(f->id);
}

static void InitRec(DATA *f, int id)
{
	f->id = id;
	f->area = 0;
}

/* Compares a two DATA structures according to their id-field 
 * Returns integer 1, 0 or -1, according to result of equation.
 */
static int CmpStatContVal(DATA *e1,	/* pointer to first id */
			DATA *e2)	/* pointer to second id */
{			
	INT4 tmp;
	tmp = (e1->id) - (e2->id);
	return (int) tmp;	
}


static DATA const *FindMaxArea(
	const DATA *e1,
	const DATA *e2)
{
	int a = CmpDouble(&(e1->area), &(e2->area));
	if ( a == 0)
		a = (int)((e1->id)-(e2->id));
	POSTCOND(a!=0);
	return (a > 0) ? e1 : e2;
}

static DATA const *FindMaxId(
	const DATA *e1,
	const DATA *e2)
{
	const DATA *maxId, *otherId;
	if (e1->id > e2->id)
	 { maxId=e1; otherId=e2;}
	else
	 { maxId=e2; otherId=e1;}
	if (maxId->area == 0)
		return otherId;
	return maxId;
}


/* Calculates the output id according to areas and ids.
 * Determines the id covering the largest area if no option is used.
 * Determines the maximum of the ids in case the option is used.
 * Returns 1 in case of an error, 0 otherwise.
 */
 static int CalcOut(
 	MAP *out,		/* write-only output map */
 	size_t rOut,		/* row of pixel to calculate */
 	size_t cOut,		/* column of pixel */
 	SEARCH_TABLE *table) 	/* list of ids & areas */
{
	size_t	cover = 0;	/* total non-MV cells covering */
	INT4 	outVal;		/* the calculated output id */
	DATA	*record; 
	
  if(opMax == 1){	/* maximum id option */
    /* Maximum id is the output id */
    record = (DATA*)(STsearch(table,(SEARCH_REC)FindMaxId));
  }
  else{	/* id with maximum area wanted */
    record = (DATA*)(STsearch(table,(SEARCH_REC)FindMaxArea));
  }

	outVal = MV_INT4;
	if (record->area != 0)
  {
	  outVal = record->id;
	  /* Determine number of covered subpixels of output pixel */
	  if( raster != NULL)
		cover = DetNrCoverCells(raster);   /* coverage percentage */
	  /* CW CW CW ???????????????
	   * if( cover < nrCoverCells || cover == 0)
	   *	outVal = MV_INT4;
	   */
        }
	RputCell(out, rOut, cOut, &outVal); 
	return 0;
}

/* Adds cell to search table.
 * Returns 1 in case of an error, 0 otherwise.
 */
static int AddCell(
	SEARCH_TABLE *table,	/* read-write search table */
	const POINT2D *inputCell,	/* input cell as polygon */
	const POINT2D *outputCell,/* output cell as polygon */
	INT4 *currRow,		/* current row */
	const MAP *in,		/* input map */
	double r,		/* current row number */
	double c,		/* current column number */
	BOOL aligned,		/* maps are aligned */
	REAL8 angle)		/* angle of output map */
{
	INT4 id = currRow[(int) c];

	PRECOND(0 <= r && r < RgetNrRows(in));
	PRECOND(0 <= c && c < RgetNrCols(in));

	/* Add item to search table for cell */
	if(!IsMV(in, &id))
	{
		DATA *record, key;
		double area;	/* area of overlap */
		key.id = id;

		area = CalcArea(inputCell, outputCell, aligned);
		if(area != 0 && raster != NULL)
			ModRaster(raster, outputCell, inputCell, angle);
		record = (DATA*)(STfindOrInsert(table, &key));
		if(record == NULL)
			return 1;	
		record->area += area;
	}
	return 0;
}

/* Calculates one pixel from output map, given the input maps.
 * Puts all ids of input pixels in a list and determines the id of
 * the output pixel according to these ids and the overlapping areas.
 * Returns 0 if no error occurs, 1 otherwise.
 */
static int CalcPixel(
	MAP *out,		/* write-only output map */
 	MAP **in,		/* read-only list input maps */
 	size_t nrMaps,		/* nr. of input maps */
 	size_t rOut,		/* row number pixel */
 	size_t cOut,		/* column number pixel */
 	BOOL aligned,		/* maps are aligned */
	REAL8 angle)		/* angle of output map */
{
 	SEARCH_TABLE *table=NULL;	/* read-write search table */
	double 	r, c, *leftB, *belowB, *rightB, *upperB;
	PTYPE 	tlX, tlY, trX, trY, brX, brY, blX, blY;    /* corners */
   	size_t 	i;
	POINT2D 	*outputCell=NULL;		/* polygon of output cell */
   	size_t 	nr = 4;			/* nr of points of cell */

	if (raster != NULL)
		raster = InitRaster(raster);	/* initialize raster */

   	/* Initialize table */
	table = STnew(nrFast, sizeof(DATA), (RETURN_ID)ReturnId, 
	              (INIT_REC)InitRec, (QSORT_CMP) CmpStatContVal);
	if(table == NULL)
		goto failure_alloc;

	/* Initialize the boundaries */
	if(((rightB = (double*)(ChkTmpMalloc(nrMaps * sizeof(double))))) == NULL)
		goto failure_alloc;
	if(((leftB = (double*)(ChkTmpMalloc(nrMaps * sizeof(double))))) == NULL)
		goto failure_alloc;
	if(((upperB = (double*)(ChkTmpMalloc(nrMaps * sizeof(double))))) == NULL)
		goto failure_alloc;
	if(((belowB = (double*)(ChkTmpMalloc(nrMaps * sizeof(double))))) == NULL)
		goto failure_alloc;

   	/* Determine the four corners of output pixel */
	/* top left */
   	RrowCol2Coords(out, (double) rOut, (double) cOut, &tlX, &tlY); 

   	/* top right */
   	RrowCol2Coords(out, (double) rOut, (double) cOut + 1, &trX,
   							&trY); 
	/* bottom left */
   	RrowCol2Coords(out, (double) rOut + 1, (double) cOut, &blX,
   							&blY);
	/* bottom right */
   	RrowCol2Coords(out, (double) rOut + 1, (double) cOut + 1,
   							&brX, &brY);

	outputCell = PutInPol(tlX, tlY, trX, trY, brX, brY, blX, blY);
	if(outputCell == NULL)
		goto failure;
		
	POSTCOND(outputCell[0].x == outputCell[nr].x);
	POSTCOND(outputCell[0].y == outputCell[nr].y);

	/* Get pixel on every input map */
	for(i = 0; i < nrMaps; i++)
	{
		MAP 	*X = in[i];
		PTYPE 	tlC, tlR, trC, trR, brC, brR, blC, blR;
		PTYPE 	tlX2, tlY2, trX2, trY2, brX2, brY2, blX2, blY2;
		INT4	*currRow;

		/* Corners: (tlX, tlY), (trX, trY), (blX, blY) and
		 * (brX, brY). Translate for input map.
		 */
   		Rcoords2RowCol(X, tlX, tlY, &tlC, &tlR); /* top left */
   		Rcoords2RowCol(X, trX, trY, &trC, &trR); /* top right */
   		Rcoords2RowCol(X, blX, blY, &blC, &blR); /* bottom left */
   		Rcoords2RowCol(X, brX, brY, &brC, &brR); /* bottom right */

   		/* Boundaries in the input map */
   		rightB[i] = ceil(MaxPoint(tlR, trR, blR, brR));	
   		belowB[i] = ceil(MaxPoint(tlC, trC, blC, brC));	
   		leftB[i] = floor(MinPoint(tlR, trR, blR, brR));	
   		upperB[i] = floor(MinPoint(tlC, trC, blC, brC));	

		PRECOND(upperB[i] <= belowB[i]);
		PRECOND(leftB[i] <= rightB[i]);
		for(r = upperB[i]; r < belowB[i]; r++)
		{ 
		 	if(0 <= r && r < RgetNrRows(X))
		 	 currRow = (INT4 *)CacheGetRow(in, i, r);

		 for(c = leftB[i]; c < rightB[i]; c++)
		 {  /* Cells that might be in pixel */
   			POINT2D 	*inputCell;

			if(r < 0 || RgetNrRows(X) <= r || 
			c < 0 || RgetNrCols(X) <= c)
				continue;

   			/* Top left & right, bottom right & left */
   			RrowCol2Coords(X, r, c, &tlX2, &tlY2);
   			RrowCol2Coords(X, r, c + 1, &trX2, &trY2);
   			RrowCol2Coords(X, r + 1, c + 1, &brX2, &brY2);
   			RrowCol2Coords(X, r + 1, c, &blX2, &blY2);
			inputCell = PutInPol(tlX2, tlY2, trX2, trY2,
					brX2, brY2, blX2, blY2);
			if(inputCell == NULL)
				goto failure;
				
			POSTCOND(inputCell[0].x == inputCell[nr].x);
			POSTCOND(inputCell[0].y == inputCell[nr].y);

			if(AddCell(table, inputCell, 
			    outputCell, currRow, in[i], r, c, aligned, angle))
				return 1;
			Free(inputCell);		/* deallocate */
		}
	      }
	}	 

	if(CalcOut(out, rOut, cOut, table))
		goto failure;

	STfree(table);
	Free(outputCell);
	ChkTmpFree(upperB);
	ChkTmpFree(leftB);
	ChkTmpFree(belowB);		
	ChkTmpFree(rightB);	
	return 0;
failure:
	ChkTmpFree(upperB);
	ChkTmpFree(leftB);
	ChkTmpFree(belowB);		
	ChkTmpFree(rightB);	
failure_alloc:
	Free(outputCell);
	STfree(table);
	return 1;
}

/* Resamples N input maps into 1 output map.
 * For every pixel in the output map all input maps are scanned. The
 * output id of the pixel is determined according to the cells of the
 * input maps at the pixel location. With an optional percentage a
 * minimum coverage area for a non MV can be given.
 * Returns 0 if no error occurs, 1 otherwise.
 */ 
int SampleClass(
	MAP *out,		/* write-only output map */
       	MAP **in,		/* read-only list input maps */
       	double percentage,	/* min. percentage for non-MV */
       	size_t nrMaps,		/* number of input maps */
       	size_t nrRows,		/* number of rows */
       	size_t nrCols,		/* number of columns */
       	BOOL aligned,		/* maps are aligned */
	REAL8 angle)		/* angle of output map */
{
	INT4	maxVal;			/* maximum id input maps */
	size_t	r, c;			/* row and column coordinate */
	size_t nrCoverCells;		/* min. nr. of cells for non-MV */
	size_t i;		/* input map i, nr. of fast list
					 * items. 
					 */

	InitCache(out, in, nrMaps);

	nrCoverCells = ceil((percentage / 100) * rasterSize * rasterSize);

	if(percentage > 0 && nrMaps > 1)
		raster = NewRaster(nrCoverCells, rasterSize);

   	/* Determine maximum id of input maps */
   	RgetMaxVal(in[0], &maxVal);
   	for(i = 1; i < nrMaps; i++)
   	{
		INT4 max;
		RgetMaxVal(in[i], &max);
		maxVal = MAX(max,maxVal);
   	}

   	/* Determine the number of items in the fast list */
   	if(MAXFAST < maxVal)
   		nrFast = MAXFAST;
   	else
   		nrFast = maxVal + 1;



	/* Calculate the id for each pixel */
	for(r = 0; r < nrRows; r++) 	
	{
	 	AppRowProgress((int) r);
	 	for(c = 0; c < nrCols; c++) 	
	 	{
			if(CalcPixel(out, in, nrMaps, r, c, aligned, angle))
				return 1;	/* allocation failed */
	 	}
	}
	AppEndRowProgress();
	if(raster != NULL)
		FreeRaster(raster);
	FreeCache(nrMaps);
	return 0;			/* successfully terminated */
}
