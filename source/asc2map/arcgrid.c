#include "stddefx.h"

/*
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
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

static int NextToken(void)
{
	int c = LexGetToken();
	if (LexError(c))
	{
	  ErrorNested("on line %d", LexGetLineNr());
	  return 1;
	}
	return 0;
}

static int ParseError(
	const char *expect)
{
	ErrorNested("expected header to contain %s at this point but read '%s'",
		   expect,LexGetTokenValue());
	ErrorNested("on line %d", LexGetLineNr());
	return 1;
}
 
static int GetDouble(
	double *v)   /* write-only value */
{
	if (NextToken())
	  return 1;
	if (!CnvrtDouble(v, LexGetTokenValue()))
	        return ParseError("a number");
	return 0;
}

static int GetWord(
	const char *w)
{
	if (NextToken())
	  return 1;
	if (! StrCaseEq(w, LexGetTokenValue()))
	{
	        char buf[64];
	        (void)sprintf(buf,"key word '%s'",w);
	        return ParseError(buf);
	}
	return 0;
}

/* read arc info grid ascii header
 * puts all info in header,writes to
 * ErrorNested in case of error,
 * parsing stops before or jst after the last newline 
 * in the header
 * returns 0 if OK, 1 if format error
 */
int ReadArcInfoGridAsciiHeader(
	ARC_INFO_GRID_ASCII *a, /* write only */
	FILE *f)	        /* input file, left at point where
	                         * first cell can be scanned  */
{
	int val;
	double v;
	rewind(f);
	LexInstall(f, "");

	if (GetWord("NCOLS")) return 1;
	if (GetDouble(&v)) return 1;
	if ( (! CnvrtInt(&val, LexGetTokenValue()))
	   || val <= 0 )
	{
	 ErrorNested("line %d: '%s' is not a legal value for the number of columns",
	  LexGetLineNr(), LexGetTokenValue());
	 return 1;
	}
	a->nrCols = (size_t)val;

	if (GetWord("NROWS")) return 1;
	if (GetDouble(&v)) return 1;
	if ( (! CnvrtInt(&val, LexGetTokenValue()))
	   || val <= 0 )
	{
	 ErrorNested("line %d: '%s' is not a legal value for the number of rows",
	  LexGetLineNr(), LexGetTokenValue());
	 return 1;
	}
	a->nrRows = (size_t)val;

	/* xllcenter | xllcorner */
	if (NextToken()) return 1;
	if (!StrCaseEq(LexGetTokenValue(),"XLLCENTER") &&
	    !StrCaseEq(LexGetTokenValue(),"XLLCORNER") )
	   return ParseError("key word 'XLLCENTER'or 'XLLCORNER'");
	a->xCorner = StrCaseEq(LexGetTokenValue(),"XLLCENTER");
	if (GetDouble(&(a->xLL))) return 1;

	/* yllcenter | yllcorner */
	if (NextToken()) return 1;
	if (!StrCaseEq(LexGetTokenValue(),"YLLCENTER") &&
	    !StrCaseEq(LexGetTokenValue(),"YLLCORNER") )
	   return ParseError("key word 'YLLCENTER'or 'YLLCORNER'");
	a->yCorner = StrCaseEq(LexGetTokenValue(),"YLLCENTER");
	if (GetDouble(&(a->yLL))) return 1;

	if (GetWord("CELLSIZE")) return 1;
	if (GetDouble(&(a->cellSize))) return 1;
	if ( a->cellSize <= 0 )
	{
	 ErrorNested("line %d: '%s' is not a legal value for the cell size",
	  LexGetLineNr(), LexGetTokenValue());
	 return 1;
	}

	/* optional no data value */
	if (NextToken()) return 1;
	if (StrCaseEq(LexGetTokenValue(),"NODATA_VALUE"))
	{
	    a->mvGiven = TRUE;
	    if (GetDouble(&(a->mv))) return 1;
	}
	else
	{
	    /* first number of grid-data: unget */
	    LexUngetToken();
	    a->mvGiven = FALSE;
	    a->mv      = -9999;
	}
	return 0;
}

/* read header of Genamap audit header
 * Genamap can create an ascii raster file
 * by redirecting the AUDIT command.
 * This function recognizes the number of rows and columns
 * in the header.
 * Parsing stops before the new line of the first line.
 * returns 0 if success, 1 in case of failure which means that
 * are not two numbers on the first line denoting the number of
 * rows and columns.
 */
int ReadGenamapAuditHeader(
	size_t   *nrRows,   /* write-only, number of rows */ 
	size_t   *nrCols,   /* write-only, number of columns */
	FILE *f)	 /* input file, left at point where
	                  * first cell can be scanned  */
{
	int val;
	rewind(f);
	LexInstall(f, "");

	do {
	     if (NextToken())
	     	return 1;
	     if (LexGetLineNr() > 1)
	     {
	     	ErrorNested("Line 1 does not contain the number of rows and columns");
	     	return 1;
	     }
	} while ( !CnvrtInt(&val, LexGetTokenValue()) );

	if (*nrRows <= 0)
	{
	 ErrorNested("line %d: '%s' is not a legal value for the number of rows",
	  LexGetLineNr(), LexGetTokenValue());
	 return 1;
	}
	*nrRows = (size_t)val;
	if (NextToken())
	     	return 1;
	if ( (!CnvrtInt(&val, LexGetTokenValue())) || (*nrCols <= 0) )
	{
	 ErrorNested("line %d: '%s' is not a legal value for the number of columns",
	  LexGetLineNr(), LexGetTokenValue());
	 return 1;
	}
	*nrCols = (size_t)val;
	return 0;
}
