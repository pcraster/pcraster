#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"	/* AppRowProgress */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "accu.h"    	/*  PerformAccu */

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* ARGSUSED */

/* Performs function to return everything.
 */
static REAL8 All(REAL8 amount, REAL8 dummy)
{
	return amount;
}

extern BOOL accuCheckDomain;

 /* Calculates the accumulated amount
 * The amount values of the cells in the catchment are accumulated and
 * this is the output value. Assumes an UINT1 ldd map and a REAL8 
 * amount map to be present. (If the ldd map is unsound and 
 * has no pit, the output map will be filled with missing values.)
 * Writes the new state and the flux for each cell in output maps.
 * Returns 0 if successful, non-zero otherwise 
 */
int Accu(
     MAP_REAL8 *state,		/* Read-write output state map  */ 
     MAP_REAL8 *flux,		/* Read-write output flux map  */ 
     const MAP_UINT1 *ldd, 	/* ldd map */
     const MAP_REAL8 *amount)	/* amount map */
{
	/* use amount also as dummy 
	 */
	accuCheckDomain=TRUE; /* I want it to be true */
	switch(PerformAccu(state, flux, ldd, amount, amount, All)) {
	 case 2: Error("Domain error on parameter");
	 case 1: return 1; /* for both 1 and 2 */
	}
	accuCheckDomain=TRUE;
	return 0;
}
