#include "stddefx.h" 

/*
 *
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "csf.h"
#include "misc.h"
#include <string.h>	/* strlen, strcpy */
#include <ctype.h>	/* isdigit */
#include "app.h"	/* appOutput, APP_PROGRESS, AppProgress */

#include "map2asc.h"
/* apps. called */

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

static char valFmt[128],mvStr[128];
static double mvVal;
static BOOL number;
static CSF_VS valueScale;

/******************/
/* IMPLEMENTATION */ 
/******************/

#include "printcol.inc"

/* Scans maps in order according to option.
 * Returns 1 in case of an error, 0 otherwise.
 */
static int ScanMap(
	FILE  *outputFile,		/* write-only output file */
	MAP *inputMap,			/* read-only input maps */
	const char *sep,
	int  nrCellsOnLine, 
	BOOL colWise)
{
 	size_t 	r, c, nrRows, nrCols;
 	
 	nrRows = (int)RgetNrRows(inputMap);
 	nrCols = (int)RgetNrCols(inputMap);

 	if(colWise)
 	{	/* for each column all rows are scanned */
 		size_t cell = 0;
 		for(c = 0; c < nrCols; c++)
 		{
 			AppProgress(
 			 "progress line beneath denotes columns not rows\n");
 			AppRowProgress(c);
 			for(r = 0; r < nrRows; r++)
 			{
 			    	REAL8 val;
 			    	int   res;
 			    	RgetCell(inputMap, r, c, &val);
 			    	res = PutCell(outputFile,&val,
 			    	              valueScale,number,mvVal,
 			    	              valFmt,mvStr,
 			    	               sep,cell==0); 
 			    	if (res != 0)
 			    	{
					PrintError(res,r,c,&val);
					return res;
 			    	}
 				if(++cell == nrCellsOnLine)
 				{
 					fprintf(outputFile, "\n");
 					cell = 0;
 				}
 			}
 			AppEndRowProgress();
 		}
 	}
 	else
 	{	/* for each row all columns are scanned */
 		int cell = 0;
 		REAL8 *currRow = (REAL8 *)Rmalloc(inputMap, nrCols);
 		if(currRow == NULL)
 			return 1;

 		for(r = 0; r < nrRows; r++)
 		{
 			AppRowProgress(r);
 			RgetRow(inputMap, r, currRow);
 			for(c = 0; c < nrCols; c++)
 			{
 			    	int res = PutCell(outputFile,currRow+c,
 			    	                  valueScale,number,mvVal,
 			    	                  valFmt,mvStr,
 			    			  sep,cell==0); 
 			    	if (res != 0)
 			    	{
					PrintError(res,r,c,currRow+c);
					Free(currRow);
					return res;
 			    	}
 				if(++cell == nrCellsOnLine)
 				{
 					fprintf(outputFile, "\n");
 					cell = 0;
 				}
 			}
 		}
 		AppEndRowProgress();
 		Free(currRow);
 	}
 	return 0;
}

/* Prints header in outputFile.
 * Returns 1 in case of an error, 0 otherwise.
 */
 static int PrintAsciiGridHeader(
	FILE  *outputFile,	/* write-only output file */
	MAP *input)		/* read-only input map */
{
	UINT4 nrRows = RgetNrRows(input), nrCols = RgetNrCols(input);
	double x, y;

	/* Calculate XLLCorner and YLLCorner */
	RgetCoords(input, 0, (size_t)nrRows, (size_t)0, &x, &y);

/*
  Als je map files exporteert met -a a optie voor Arc/INFO
    Zet hij er in de header .0000 achetr en dat moet niet

    Het schijnt dat die .00000 een probleem geven

    NCOLS 2700
    NROWS 3250
    XLLCORNER  10000.000000
    YLLCORNER 300000.000000
    CELLSIZE 100.000000
    NODATA_VALUE -9999

    het moet zijn:

    NCOLS 2700
    NROWS 3250
    XLLCORNER  10000
    YLLCORNER 300000
    CELLSIZE     100
    NODATA_VALUE -9999
*/

	/* Print header */
	fprintf(outputFile, "NCOLS %d\n", (int)nrCols);
	fprintf(outputFile, "NROWS %d\n", (int)nrRows);
	fprintf(outputFile, "XLLCORNER %f\n", x);
	fprintf(outputFile, "YLLCORNER %f\n", y);
	fprintf(outputFile, "CELLSIZE %f\n", RgetCellSize(input)); 
	if(fprintf(outputFile, "NODATA_VALUE %s\n", mvStr) < 0) 
		return 1;
	return 0;
}

/* Converts csf input files to an ascii output file.
 * If option -a is used an arc/info ascii file is created, else
 * a plain ascii file is created.
 * Returns 1 in case of an error, 0 otherwise
 */
int Map2Asc(
	MAP        *inputMap,	/* read-only input map    */
	char       *outputFile,	/* write-only output file */
	const char *mv,         /* mv-string              */
	      char *fmt,        /* empty string if not given */
	const char *sep,        /* separator, NULL if not given */
	int         nrCellsOnLine, /* preset to NrCols if not given */
	HEADER      head,    /* write ASCIIGRID header Y/N */
	BOOL        colWise)    /* output columnwise Y/N */
{
	FILE *output = fopen(outputFile, "w");
	if(output == NULL)
	{
		Error("Unable to create '%s'",outputFile);
		goto failure;
	}

	/* Fill buffers for print format */
	valueScale = RgetValueScale(inputMap);
	number = CnvrtDouble(&mvVal, mv);
	MakeFmts( valFmt,mvStr,valueScale,fmt,mv,inputMap);

	/* Create output ascii file */
	switch(head) {
	 case HEAD_NONE: break;
	 case HEAD_ARCINFO:
	       if (PrintAsciiGridHeader(output, inputMap))
		{
			Error("Unable to write to '%s'",outputFile);
			goto failure2;
		}
	       break;	
	 case HEAD_ROWCOL:	
		if(fprintf(output, "%d %d\n",
		 (int)RgetNrRows(inputMap),(int)RgetNrCols(inputMap)) < 0) 
			goto failure2;
		break;	
	 case HEAD_COLROW:	
		if(fprintf(output, "%d %d\n",
		 (int)RgetNrCols(inputMap),(int)RgetNrRows(inputMap)) < 0) 
			goto failure2;
		break;	
	}	
	switch (ScanMap(output, inputMap, sep, nrCellsOnLine, colWise)) {
		case 0: break; /* OK */
		case 1: 
		    Error("Unable to write to '%s'",outputFile);
		    goto failure2;
		case 2:
		    Error("In input map '%s'",MgetFileName(inputMap));
		    goto failure2;
         }

	fclose(output);
	return 0;

failure2:
	fclose(output);
	remove(outputFile);
failure:
	return 1;
}
