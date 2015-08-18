#include "stddefx.h"

/*
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "csf.h"
#include "misc.h"
#include "app.h"  
#include "asc2map.h"

/* apps. called */
#include "arcgrid.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */
/******************/

/* Prints error message if unexpected end of file is detected.
 * Error message is printed to buffer of ErrorNested.
 */
 static void EndError(
 	UINT4 r,
 	UINT4 c,
 	UINT4 nrRows,
 	UINT4 nrCols)
 {
 	ErrorNested("at line '%d' for cell (row= %d, col= %d)\n" 
 	            "%d cells were read (%d) required", 
 	            LexGetLineNr(), r+1,c+1,
 	            (int)((r*nrCols)+c), (int)(nrRows*nrCols));
 }

static void PrintIllegalVal(
 	UINT4 r, /* row */
 	UINT4 c) /* col */
{
 	ErrorNested("at line '%d' for cell (row=%d,col=%d)",
 	            LexGetLineNr(), (int)r+1, (int)c+1);
}

/* Parses the input file, checks its values and puts values in csf map.
 * Returns 1 in case of an error, 0 otherwise.
 */
static int ScanInputFile(
	MAP *out,	       /* write-only */
	FILE *f,	       /* input file */
	const char *mv,        /* missing value */
	int  sepChar,         /* separator char */
	ASC_TYPE   format,     /* special format */ 
	int        header,
	int        rowHeader)
{
	size_t nrMV=0;
	size_t nrnonMV=0;
	size_t nrDirFlat=0;
	size_t	nrRows = RgetNrRows(out);
	size_t	nrCols = RgetNrCols(out);
	CSF_VS   vs = RgetValueScale(out);
	CSF_CR   cr = RgetCellRepr(out);
	REAL8	*buf = Rmalloc(out, nrCols);
	UINT4 	r, c;
	int     result = 1;
	char    sepBuf[2];

	/* overwritten in arcInfo mode: */
	double 	mvDbl;
	BOOL 	number = CnvrtDouble(&mvDbl, mv);

	if(buf == NULL)
		return 1;

	/* initialize the lexical analyzer */
	sepBuf[0] = (char)sepChar;
	sepBuf[1] = '\0';
	LexInstall(f, sepBuf);

	switch(format) {
	 case ASC_ARCINFO:
	     {
		/* Format is output of arc/infoIIGRID */
		ARC_INFO_GRID_ASCII a;
		if (ReadArcInfoGridAsciiHeader(&a, f))
		{
		 ErrorNested("in ARC/INFO gridascii file");
		 goto error;
		}
		if (a.nrRows != nrRows || a.nrCols != nrCols)
		{
		 ErrorNested("number of columns and rows are not the same as in the clone map\n"
		             "(cols, rows) clone: (%d,%d) gridascii: (%d,%d)",
			     nrCols,nrRows,a.nrCols,a.nrRows);
		 ErrorNested("in ARC/INFO gridascii file");
		 goto error;
		}

		/* optional MV field or default ARC value: -9999 */
		number = TRUE;
		mvDbl = a.mv;
	     }
	     break;
	 case ASC_GENAMAP:
	  {
		size_t rHeader,cHeader;
		if (ReadGenamapAuditHeader(&rHeader,&cHeader,f))
		{
		 ErrorNested("in Genamap AUDIT output file");
		 goto error;
		}
		if (rHeader != nrRows || cHeader != nrCols)
		{
		 ErrorNested("number of rows and columns are not the same as in the clone map\n"
		             "(rows, cols) clone: (%u,%u) Genamap: (%u,%u)",
			     nrRows, nrCols,rHeader,cHeader);
		 ErrorNested("in Genamap AUDIT output file");
		 goto error;
		}
	        rowHeader = 2;
	  }
	  break;
	default:
	         if (header > 0)
	          if ( LexSkipLines(header) != header)
	          {
	           ErrorNested("not enough lines in file to skip '%d' lines of header",
	                        header);
		   goto error;
		  }
	  break;
	}

	for(r = 0; r < nrRows; r++)
	{
	  AppRowProgress((int) r);
	  if (rowHeader > 0) 
	  	LexSkipLines(rowHeader+1);
	  for(c = 0; c < nrCols; c++)
	  {
	    int token = LexGetToken();
	    const   char *v;
	    REAL8   val;

	    /* skip separator */
	    if (token == sepChar)
	        token = LexGetToken();

	    if (LexError(token))
		{
		 EndError(r,c,(UINT4)nrRows,(UINT4)nrCols);
		 goto error;
		}

	    if (token == sepChar)
	    {
	        ErrorNested("empty cell, two consecutive separators '%c' read",
	                    sepChar);
		EndError(r,c,(UINT4)nrRows,(UINT4)nrCols);
		goto error;
	    }

	    v = LexGetTokenValue();
	    switch(token)
	    {
	     case LEX_NUMBER:
		    (void)CnvrtREAL8(&val, v);
		    if(number && val == mvDbl)
		    { /* string mv */
			SET_MV_REAL8(buf+c);
			nrMV++;
		    }
		    else
		    {  
		       if(AppCheckVal(v, vs, cr))
		       {
			PrintIllegalVal(r,c);
			goto error;
		       }
		       if(vs == VS_DIRECTION) 
		       {
		        if (val != -1)
				val = AppInputDirection(val);
			else
				nrDirFlat++;
                       }
		       buf[c] = val;
		       nrnonMV++;
		    }
		    break;
	     default: /* not a number, could be the mv */
		if (!number && StrEq(v, mv))
		{ /* string mv */
			SET_MV_REAL8(buf+c);
			nrMV++;
		}
		else /* some other string */
		{
			ErrorNested("'%s' is an illegal value", v);
			PrintIllegalVal(r,c);
			goto error;
		}
	     } /* eoswitch */

	    } /* eofor col */
	   RputRow(out, (size_t)r, buf);
	} /* eofor row */
	AppEndRowProgress();
	
	AppVerbose("number lines read: %ld \n", LexGetLineNr());
	AppVerbose("number of non-mv cells: %ld\n", nrnonMV);
	AppVerbose("number of mv cells: %ld\n", nrMV);
        if(vs == VS_DIRECTION) 
	 AppVerbose("number of no-direction cells: %ld\n",nrDirFlat);

 	result = 0;
error: 
	free(buf);
	return result;
}

/* Converts an ascii input file to a csf file.
 * Checks values to be valid ones according to the value scale.
 * Gives warning when the MV falls in the domain of the value scale.
 * Returns 1 in case of an error, 0 otherwise
 */
int Asc2Map(
	MAP         *out,		/* write-only output file */
	const char *inputFile,		/* input file */
	const char *mv,        /* missing value */
	int  sepChar,
	ASC_TYPE   t,                   /* special format */ 
	int        header,
	int        rowHeader)
{
	FILE 	*f;

	/* open file */
	f = fopen(inputFile, "r");
	if(f == NULL)
	{
		AppFileOpenError(inputFile);
		return 1;
	}

	if(ScanInputFile(out,  f, mv, sepChar,t,header,rowHeader))
	{
		(void)fclose(f);
		return RetError(1,"while reading '%s'", inputFile);
	}
	(void)fclose(f);
	return 0;
}
