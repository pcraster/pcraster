#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"	/* AppProgress, APP_PROGRESS, appOutput */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

#ifdef DEBUG
/* nr of time tables allocated.	
 * nr of time tables allocated.	
 * Only available in DEBUG mode.
 */
int nrTimeTables=0;
#endif 

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* Determines output map, with given table and timestep.
 * Pixels with and id that is not in the time table will become MV.
 * Returns 0;
 */
int TimeInputSeries(
	MAP_REAL8 *out,		/* map to write */
	const MAP_INT4 *id,	/* id map */
	const TIME_TABLE *t,	/* time table */
	int timeStep)		/* number of iteration */
{
	int r, c, nrRows, nrCols;

	/* initialization */
	nrRows = id->NrRows(id);
	nrCols = id->NrCols(id);
	PRECOND(nrRows == out->NrRows(out));
	PRECOND(nrCols == out->NrCols(out));
	PRECOND(0 <= timeStep && timeStep < t->nrSteps);
	id->SetGetTest(GET_MV_TEST, id);

	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	 {
		INT4 idVal;
		if(!id->Get(&idVal, r, c, id))
			out->PutMV(r, c, out);
		else
		{	/* id is not a MV */
			if(0 < idVal && idVal < t->nrCols /* id is in time table */
			  && (!IS_MV_REAL8(t->vals[timeStep]+idVal)))
			    out->Put(t->vals[timeStep][idVal], r, c, out);
			else
			    out->PutMV(r, c, out);    /* not in table , or MV */
		}
	}
	return 0;
}

/* Reads a TIME_TABLE from a file.
 * Values are checked on being valid according to value scale.
 * In case of an error, a message is printed using ErrorNested.
 * Returns NULL when an error occurs, pointer to time table otherwise.
 *
 * NOTE
 * use FreeTimeTable to free the space allocated in this function
 */
TIME_TABLE *ReadTimeInputTable(
	const char *fileName,		/* name of file to read */
	int nrFirstStepsToSkip,		/* nr. of steps to skip (NOT USED, set to 0) */
	int nrStepsToRead,		/* nr. of steps to read (NOT USED, set to 0) */
	CSF_VS vs)			/* value scale */
{
	TIME_TABLE 	*t;
	BOOL geoEas;
	size_t nrSteps, nrCols;

  appLarge=TRUE;

	/* suppress not used warning */
	PRECOND(nrFirstStepsToSkip >= 0);
	PRECOND(nrStepsToRead >= 0);
  (void)nrFirstStepsToSkip; // shut up compiler
  (void)nrStepsToRead; // shut up compiler

	if((t = NewTimeTable(vs, 0)) == NULL)
		return NULL;
	t->vs = vs;
	if ( AppReadTimeSeriesFile(&(t->vals), &nrSteps, &nrCols, &geoEas,
	                           fileName, "1E31", t->vs, CR_UNDEFINED, ',') )
	 {
	  Error("while reading timeseries '%s'", fileName);
	  Free(t);
	  return NULL;
	 }
	t->nrSteps = (int)nrSteps;
	t->nrCols = (int)nrCols;
	return t;
}

/* Create a time table strucure
 * NewTimeTable only allocates the TIME_TABLE structure and NOT the
 * the space for the values. The vals pointer is set to NULL. Pcrcalc allocates the value space during the
 * first step.
 * returns
 *  NULL if allocation fails
 */
TIME_TABLE *NewTimeTable(
	CSF_VS vs,      /* value scale , VS_UNDEFINED is valid */
	int   nrSteps) /* number of steps */
{
	TIME_TABLE *t;
#ifdef DEBUG
	nrTimeTables++;
#endif 
	t = ChkMalloc(sizeof(TIME_TABLE));
	if (t == NULL)
		return NULL;
	t->vs = vs;
	t->nrSteps = nrSteps;
	t->vals = NULL; /* important, output table are allocated 
	                 * in first time step
	                 * this discerns input and output table
	                 * in makecode.c
	                 */
	return t;
}

/* Free a time table created in NewTimeTable or ReadTimeInputTable
 * FreeTimeTable free the TIME_TABLE structure and the value space if
 * the value space is not NULL
 */
void FreeTimeTable(TIME_TABLE *t) /* free all memory of this table */
{
#ifdef DEBUG
	nrTimeTables--;
#endif 
	if (t->vals != NULL)
		Free2d((void **)t->vals, (size_t)t->nrSteps);
	Free(t);
}
