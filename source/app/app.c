#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "app.h"
#include "misc.h"
#include <stdarg.h>	/* va_list, va_arg, va_start and va_end */

/* apps. called */

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/*
 * define FLUSH(stream)	(void)fflush(stream)
 */
#define FLUSH(stream)	

/*********************/
/* LOCAL DEFINITIONS */
/*********************/
static BOOL firstTimeCalled = TRUE;	/* for prefix PROGRESS */

/******************/
/* IMPLEMENTATION */
/******************/

/* Write busy with given row number to stderr and returns */
void AppRowProgress(int r)	/* number of current row */
{
	if(appOutput != APP_PROGRESS)
		return;
	(void)fprintf(stderr, "\rBusy with row: %d", r);
	FLUSH(stderr);
	FLUSH(stdout);
}

/* Write newline to stderr and returns */
void AppEndRowProgress(void)
{
	if(appOutput == APP_PROGRESS)
		(void)fprintf(stderr, "\n");
}

/* Writes progress message to stderr if desired and then returns.
 */
void AppProgress(
	const char *fmt,  	/* Format control */
	... )             	/* Optional arguments */
{
    	va_list marker;		/* parameter list of variable length */

    	if(appOutput != APP_PROGRESS)
    		return;		/* no progress message desired */

    	/* Let marker point to 1st unnamed argument */
    	va_start(marker, VA_START_ARG(fmt));

    	if(firstTimeCalled)
    	{	/* Prefix only the first time */
    		(void)fprintf(stderr, "\nPROGRESS:\n");
		firstTimeCalled = FALSE;
	}

    	(void)vfprintf(stderr, fmt, marker);
	va_end(marker);		/* clean up */

	FLUSH(stderr);
	FLUSH(stdout);
	return;
}

/* Writes verbose message to stderr if desired and then returns.
 */
void AppVerbose(
	const char *fmt,  	/* Format control */
	... )             	/* Optional arguments */
{
    	va_list marker;		/* parameter list of variable length */


    	if(appOutput == APP_NOOUT)
    		return;		/* no progress message desired */

    	/* Let marker point to 1st unnamed argument */
    	va_start(marker, VA_START_ARG(fmt));
    	(void)vfprintf(stderr, fmt, marker);
	va_end(marker);		/* clean up */

	FLUSH(stderr);
	FLUSH(stdout);
	return;
}

/* Prints error message for failed file opening to buffer of ErrorNested.
 */
 void AppFileOpenError(const char *filename)
 {
 	ErrorNested("can not open: %s", filename);
 }

/* Casts REAL8 to REAL4.
 * Necessary when due to rounding errors occur in execution 
 */
REAL4 AppCastREAL4(double newS)		/* value to cast */
{
	REAL4 s = (REAL4)newS;
	return s;
}
