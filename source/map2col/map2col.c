#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "csf.h"
#include "misc.h"
#include <string.h>	/* strlen, strcpy */
#include "app.h"	/* appOutput, APP_PROGRESS, AppProgress */

/* apps. called */
#include "map2col.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/
#define POS_X	0
#define POS_Y	1

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/
static REAL8 **recList=NULL;
static size_t nrRecs=0;
static BOOL mvIsNumber;
static double mvValue;
static const char *globSep;
/******************/
/* IMPLEMENTATION */ 
/******************/

#include "printcol.inc"

/* Prints header in outputFile when in non-append mode.
 * Returns 1 in case of an error, 0 otherwise.
 */
 static int PrintHeader(
	FILE  *outputFile,		/* write-only output file */
	const INP_MAP *maps,		/* read-only input maps */
	size_t nrMaps)			/* number of input maps */
{
	size_t i;

	/* print name of program in output file */
	fprintf(outputFile, "map2col\n");
	fprintf(outputFile, "%u\n",(UINT_T)nrMaps);
	for(i = 0; i < nrMaps; i++)
	{
		const char *s = NULL;
		switch(maps[i].type) {
		 case 'x' : s = "x-coordinate"; break;
		 case 'y' : s = "y-coordinate"; break;
		 case 'v' : s = MgetFileName(maps[i].m); break;
		}
		POSTCOND(s!= NULL);
		if(fprintf(outputFile, "%s\n", s) < 0)
			return 1;
	}
	return 0;
}

static void FreeCache(
  INP_MAP    *maps,       /* list of map records */
 size_t nrMaps)             /* array size of maps */
{
	size_t i;
	for (i=0; i < nrMaps; i++)
	if (maps[i].type == 'v')
	  Free(maps[i].r);
}

static int InitMaps(
 INP_MAP    *maps,       /* list of map records */
 size_t nrMaps,             /* array size of maps */
 const char *mv)
{
	size_t i,n;
	for (i=n=0; i < nrMaps; i++)
	{
	 if (maps[i].type != 'v')
	 	maps[i].vs = VS_SCALAR; /* x or y */
	 else
	 {
	  maps[i].vs = RgetValueScale(maps[i].m);
	  maps[i].cacheRow = -1; /* no row read */
	  maps[i].r = (REAL8 *)Rmalloc(maps[i].m,RgetNrCols(maps[i].m));
	  if (maps[i].r == NULL)
	  {
	  	FreeCache(maps,n);
	        return 1;
	  }
	 }
	 MakeFmts(maps[i].valFmt, maps[i].mvStr, 
	           maps[i].vs, maps[i].usrFmt, mv, maps[i].m);
	 n++;
	}
	return 0;
}

static int ReadInputRecords(
	BOOL *geoEas,
  const char *inputFile,
	size_t   xcoord,
	size_t   ycoord,
  const char *mv) 
{
	size_t cols[3];
	size_t nrRecsRead, nrMVvalCol,nrMVcoorCol;
	static const char *locsep;
	cols[0] = xcoord;
	cols[1] = cols[2] = ycoord;
	recList=NULL;
	nrRecs=0;
	if (globSep == NULL)
		locsep = ",";
	else
	        locsep = globSep;
	
	if (AppReadColumnFile(&recList, &nrRecs, 
	          &nrRecsRead, &nrMVvalCol,&nrMVcoorCol,
	          geoEas, inputFile, mv, VS_UNDEFINED,CR_UNDEFINED,
	          cols, locsep[0], FALSE))
        {
		Error("While reading '%s'", inputFile);
		return 1;
	}
	POSTCOND(nrRecsRead == nrRecs);/* all processed */

	AppVerbose("nr. of records read: %u\n", nrRecsRead);
	/* use nrMVvalCol and not nrMVcoorCol, see AppReadColumnFile
	 */
	AppVerbose("nr. of records with mv (x,y): %u\n", nrMVvalCol);

	return 0;
}

static void GetCell(
	REAL8   *v,
	INP_MAP *m,
	int row, int col)
{
	if (m->cacheRow != row)
	{
		RgetRow(m->m, row, m->r);
		m->cacheRow = row;
	}
	COPY_REAL8(v,(m->r)+col);
}

static int PrintValuesLine(
	FILE *out,           /* output file */
	const INP_MAP *maps, /* */
	REAL8 *values,       /* values only changed if vs is DIRECTIONAL */
	size_t nrMaps,          /* size of maps and values array */
	BOOL append)         /* append determines if we write the first col */
{
	size_t i;
	for(i=0; i < nrMaps; i++)
	 if ( PutCell(out, values+i, maps[i].vs,mvIsNumber,mvValue, 
	              maps[i].valFmt, maps[i].mvStr, globSep, (!append) && i == 0) )
	              return 1;
	return fprintf(out, "\n") < 0;
}

static int CopyLine(
	FILE *in,
	FILE *out)
{
	int c;
	while ( (c= fgetc(in)) != '\n' && c != EOF)
		if (fputc(c, out) == EOF)
			break;
	return ferror(in) || ferror(out);
}

static int DoAppendMode(
 const char    *outputFile, 
 const char    *inputFile, 
       INP_MAP *maps,
       size_t     nrMaps, 
       BOOL    geoEas)
{
	FILE *in,*out;
	REAL8 *values;
	size_t   i,c;

	if ( (values = (REAL8 *)ChkMalloc(sizeof(REAL8)*nrMaps)) 
	      == NULL)
	      goto error;

	if ( (in = fopen(inputFile,"r")) == NULL)
	{
		Error("Can't open '%s'",inputFile);
		goto error0;
	}
	if ( (out = fopen(outputFile,"w")) == NULL)
	{
		Error("Can't create '%s'",outputFile);
		goto error1;
	}
	if (geoEas)
	{
		size_t nrCols;
		int getChar;

		/* copy headerline */
		CopyLine(in,out);
		fprintf(out,"\n");

		/* print new nr.cols line 
		 * and skip old nr.cols line
		 */
		nrCols = (size_t)AppDetectColumnFile(&geoEas,inputFile,',');
		fprintf(out,"%u\n",(UINT_T)(nrCols+nrMaps));
		while ( (getChar = fgetc(in)) != '\n' && getChar != EOF) 
				; /* eof-error will be detected later */
		
		/* copy old column description lines */
		for(c=0; c < nrCols; c++)
		{
		 CopyLine(in,out);
		 fprintf(out,"\n");
		}

		/* write new column decription lines */
		for(c=0; c < nrMaps; c++)
			fprintf(out, "map2col append: %s\n", MgetFileName(maps[c].m));
	}
	if (LimitedVersionCheck(-1,-1,-1,-1,(int)nrRecs,-1))
		goto error2;

	for(i = 0; i < nrRecs; i++)
	{
		if (CopyLine(in,out))
		{
		      Error("while copying from %s to %s",
		               inputFile, outputFile);
		      goto error2;
		}
		if (IS_MV_REAL8(recList[i]+POS_X) ||
		    IS_MV_REAL8(recList[i]+POS_Y) )
		     SetMemMV(values,nrMaps,CR_REAL8);
		else 
                 for (c = 0; c < nrMaps; c++)
                 {
                 	int row,col;
                 	if (AppRgetRowCol(maps[c].m,
		                      recList[i][POS_X],
		                      recList[i][POS_Y],
		                      &row,&col))
		        {
		            GetCell(values+c,maps+c, row,col);
		            if (mvIsNumber && (!IS_MV_REAL8(values+c)) 
		                && mvValue == values[c])
		            {
				PrintError(2,row,col,values+c);
	                        ErrorNested("In map '%s'",
	                         MgetFileName(maps[c].m));
	                        goto error2;
		            }
		         }
			else /* outside */
			    SET_MV_REAL8(values+c);
                  } /* eofor maps */
                if (PrintValuesLine(out,maps,values,nrMaps, TRUE))
                {
                 Error("Can't write to '%s'",outputFile);
                 goto error2;
                }
        } /* eofor nrRecs */

	fclose(out);
	fclose(in);
	Free(values);
	return 0;

error2: fclose(out);
        remove(outputFile);
error1: fclose(in);
error0:
	Free(values);
error:
	return 1;
}

static int DoCreateMode(
 const char    *outputFile, 
       INP_MAP *maps,
       size_t     nrMaps, 
       BOOL    geoEas,
       BOOL    colWise,
       BOOL    printMV,
       size_t     xco,
       size_t     yco)
{
	FILE *out;
	REAL8 *values;
        size_t row=0,col;
        size_t nrRows,nrCols;
        MAP  *m;

        while (maps[row].type != 'v') row++;
        m = maps[row].m;

        nrRows = RgetNrRows(m);
        nrCols = RgetNrCols(m);

	if ( (out = fopen(outputFile,"w")) == NULL)
	{
		Error("Can't create '%s'",outputFile);
		goto error;
	}
	if ( (values = (REAL8 *)ChkMalloc(nrMaps * sizeof(REAL8))) == NULL)
		goto error1;
	if(geoEas && PrintHeader(out, maps, nrMaps))
	{
		Error("Can't write to '%s'",outputFile);
		goto error2;
	}

	if (colWise)
	  for(col=0; col < nrCols; col++)
	  for(row=0; row < nrRows; row++)
	  {
		double xout,yout;
		double x,y;
		size_t c;
		BOOL allMv = TRUE;
		AppRgetCoords(m,(int)row,(int)col,&xout,&yout);
		RgetCoords(m,1,row,col,&x,&y);
		values[xco] = xout;
		values[yco] = yout;
                for (c = 0; c < nrMaps; c++)
                {
                 	size_t mrow, mcol;
                 	if (maps[c].type != 'v')
                 		continue;
                 	if (RgetRowCol(maps[c].m,x,y,&mrow,&mcol))
		        {
		            RgetCell(maps[c].m, mrow,mcol, values+c);
		            allMv &= IS_MV_REAL8(values+c);
		            if (mvIsNumber && (!IS_MV_REAL8(values+c)) 
		                && mvValue == values[c])
		            {
				PrintError(2,(int)mrow,(int)mcol,values+c);
	                        ErrorNested("In map '%s'", MgetFileName(maps[c].m));
	                        goto error2;
		            }
		        }
			else /* outside */
			    SET_MV_REAL8(values+c);
                } /* eofor maps */
                if ( (!allMv) || printMV)
		 if (PrintValuesLine(out, maps, values, nrMaps, FALSE))
		 {
                  Error("Can't write to '%s'",outputFile);
                  goto error2;
                 }
	  } /* eofor col,row */
        else /* rowWise */
	  for(row=0; row < nrRows; row++)
	  for(col=0; col < nrCols; col++)
	  {
		double xout,yout;
		double x,y;
		size_t c;
		BOOL allMv = TRUE;
		AppRgetCoords(m,(int)row,(int)col,&xout,&yout);
		RgetCoords(m,1,row,col,&x,&y);
		values[xco] = xout;
		values[yco] = yout;
                for (c = 0; c < nrMaps; c++)
                {
                 	size_t mrow, mcol;
                 	if (maps[c].type != 'v')
                 		continue;
                 	if (RgetRowCol(maps[c].m,x,y,&mrow,&mcol))
		        {
		            GetCell(values+c,maps+c, (int)mrow,(int)mcol);
		            allMv &= IS_MV_REAL8(values+c);
		            if (mvIsNumber && (!IS_MV_REAL8(values+c)) 
		                && mvValue == values[c])
		            {
				PrintError(2,(int)mrow,(int)mcol,values+c);
	                        ErrorNested("In map '%s'", MgetFileName(maps[c].m));
	                        goto error2;
		            }
		        }
			else /* outside */
			    SET_MV_REAL8(values+c);
                } /* eofor maps */
                if ( (!allMv) || printMV)
		 if (PrintValuesLine(out, maps, values, nrMaps, FALSE))
		 {
                  Error("Can't write to '%s'",outputFile);
                  goto error2;
                 }
	  } /* eofor row,col */

	Free(values);
	fclose(out);
	return 0;
error2:
	Free(values);
error1:
	fclose(out);
	remove(outputFile);
error:
	return 1;
}

/* Converts csf input files to a column output file.
 * If option -g is used a GeoEas column file is created, else
 * a plain column file is created (in non-append mode).
 * Returns 1 in case of an error, 0 otherwise
 */

int Map2Col(
 INP_MAP    *maps,       /* list of map records */
 const char *outputFile, /* number of map records */
 size_t nrMaps,             /* array size of maps */
 size_t xcoord,             /* pos. of x column, internal index */
 size_t ycoord,             /* pos. of y column, internal index */
 const char *mv,
 const char *separator,   /* NULL if not used, use this as seperator */
 BOOL  geoEas,            /* geoEas output Y/N */
 BOOL  colWise,           /* order of output   */
 BOOL  printMV,           /* output records even if all input maps are
                           * mv's
                           */
 const char *inputColumnFile)/* NULL if not used  */
{

	mvIsNumber = CnvrtDouble(&mvValue,mv);
	globSep = separator;
	if (InitMaps(maps,nrMaps,  mv))
		goto error;
	if (inputColumnFile != NULL)
	{    /* append mode */
	     if ( ReadInputRecords(&geoEas,inputColumnFile,
		                 xcoord,ycoord,
		                 mv))
		      goto error1;
             if ( DoAppendMode(outputFile, inputColumnFile, maps,nrMaps, geoEas))
             {
	     	AppFreeColumnData(recList, nrRecs);
		goto error1;
             }
	     AppFreeColumnData(recList, nrRecs);
        }
	else
	{	/* Create a new output column file */
             if (DoCreateMode(outputFile, maps, nrMaps, geoEas, 
				colWise, printMV, xcoord, ycoord))
		goto error1;
	}

	FreeCache(maps, nrMaps);
	return 0;

error1:
	FreeCache(maps, nrMaps);
error:
	return 1;
}
