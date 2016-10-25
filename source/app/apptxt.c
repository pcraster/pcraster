#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"	/*  StartTimer(), PauseOffTimer() etc. */

/* apps. called */
#include "app.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/

/* Detects the number of columns and type of column file 
 * AppDetectColumnFile first tries to see if it is Geo-EAS by
 * testing if the second line contains ONLY the number of columns.
 * If not the number of tokens, that are not the separator, are
 * counted to determine the number of columns.
 * The test if a file exists should be done before calling this
 * function.
 * Returns number of columns, 0 in case of error which means it can't
 * be column file at all. 
 */
int AppDetectColumnFile(
       BOOL  *geoEas,   		/* write-only boolean */
       const char *fileName,		/* file to read */
       int  sepChar)                   /* separator char */
{
	int  	token, c,nrCols = 0;
	FILE 	*f = fopen(fileName, "r");
	BOOL    somethingOnLine1 = FALSE;
	int     firstNonEmptyLine;
	char    sepBuf[2];
	*geoEas = FALSE;

	if(f == NULL)
	{
		ErrorNested("can not open: %s\n", fileName);
		return 0;
	}
	while( (c = fgetc(f)) != EOF )
	{
		if (c == '\n')
		   break;
		else
		   somethingOnLine1 = TRUE;
	}
	if ( c == EOF )
	{ /* end of file */
	      if (somethingOnLine1) 
	      {
	      	/* there's only 1 line, can't be Geo-EAS */
	      	goto detect_plain;
	      }
	      else
		goto error; /* empty file */
	}
	POSTCOND(c == '\n');

	LexInstall(f, ""); /* actual lineNr are now 1 off */
	token = LexGetToken();
	if (token == LEX_NUMBER)
	{
		int nvarOnLine = LexGetLineNr();
		if (! CnvrtInt(&nrCols,LexGetTokenValue()))
		 /* not a valid integer */
	      	 goto detect_plain;
	      	LexGetToken();
	      	if (LexGetLineNr() > nvarOnLine)
	      	{
	      		*geoEas = TRUE;
	      		(void)fclose(f);
	      		return nrCols;
	      	}
	}

detect_plain:
	rewind(f);
	sepBuf[0] = (char)sepChar;
	sepBuf[1] = '\0';
	LexInstall(f, sepBuf); 
	token = LexGetToken();
	nrCols = 0;
	firstNonEmptyLine = LexGetLineNr();
	while (firstNonEmptyLine == LexGetLineNr())
	{
	 switch(token) {
	  case 0 : firstNonEmptyLine--; break;
	  default  : if (token != sepChar)
	              nrCols++;
	 }
	 token = LexGetToken();
	}
	(void)fclose(f);
	return nrCols ;
error:
	(void)fclose(f);
	return 0;
}

#ifdef NEVER

int main(void)
{
	BOOL geoEas;
	int nrCols = AppDetectColumnFile(&geoEas, "col1");

	(void)printf("*********************\n");
	system(" cat col1 ");
	(void)printf("*********************\n");
	(void)printf("nrCols %d geoEAS %d\n",nrCols,geoEas);
	return 0;
}
#endif /*  NEVER */
