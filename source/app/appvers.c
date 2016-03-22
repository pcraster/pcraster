#include "stddefx.h" 

/*
 * appvers.c 
 */


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */

/* global header (opt.) and appvers's prototypes "" */
#include "misc.h" 
#include "app.h" 


/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
#define MSG "Evaluation version limitation:"

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* checks if the dataset is allowed for this version
 * LimitedVersionCheck checks all limitations of
 * the evaluation version, Each restriction that is set
 * negative (-1) is not checked
 *
 * returns
 *  0 if dataset is allowed
 *  non zero if dataset is not allowed
 */
int LimitedVersionCheck(
	int nrRows,     /* number of rows limit: 60 */
	int nrCols,     /* number of columns limit: 60 */
	int nrSteps,    /* number of timesteps limit: 360 */
	int nrLookupRecs, /* number of records in lookup tables limit 20 */
	int nrXYZRecs,  /* number of XYZ records in pointfile limit 3600 */
	int nrOps)      /* number of operations, limit not set yet */
{
#ifdef EVAL_VERSION
	int noLimits = 0;
#else
	int noLimits = 1;
#endif
	if (noLimits)
		return 0;
	if (nrRows > 60)
	  return RetError(1,"%s only 60 rows allowed (not '%d')",MSG,nrRows);
	if (nrCols > 60)
	  return RetError(1,"%s only 60 columns allowed (not '%d')",MSG,nrCols);
	if (nrSteps > 360)
	  return RetError(1,"%s only 360 timesteps allowed (not '%d')",MSG,nrSteps);
        if ( (nrRows > 0) &&
	     (nrCols > 0) &&
	     (nrSteps > 0) &&
	     ((nrRows*nrCols*nrSteps) > 240000)
	   )
	  {
	   int i = nrRows*nrCols*nrSteps;
	  return RetError(1,
	   "%s nr.timesteps*nr.cells may not exceed 240000 (not '%d')",MSG,i);
	  }
	if (nrLookupRecs > 20)
	   return RetError(1,"%s only 20 records\\tuples allowed in a table (not '%d')",
	           MSG, nrLookupRecs);
	if (nrXYZRecs > 3600)
	   return RetError(1,"%s only 3600 records\\tuples allowed in point data column file (not '%d')",
	           MSG, nrXYZRecs);
  (void)nrOps; // Shut up compiler
	return 0;
}
