#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "accu.h"	/* PerformAccu */

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

/* Performs threshold function
 * Returns amount of flux or 0, depending on threshold.
 */
static REAL8 Threshold(
	REAL8 amount,		/* amount to evaluate */
	REAL8 threshold)	/* threshold */
{
	if (threshold < 0)
		return -1;
	if(amount <= threshold)
		return 0;
	return (amount - threshold); /* ret neg if amount < 0 */
}

/* Determines the new state and flux for each cell.
 * The new state is the amount and incoming fluxes minus the transport 
 * threshold if the amount exceeds the threshold or the total amount.
 * Assumes a spatial ldd map and an amount map and a threshold map to
 * be present.
 * Returns 0 if successful, non-zero otherwise 
 */
int AccuTt(
     MAP_REAL8 *state,		/* Read-write output state map  */ 
     MAP_REAL8 *flux,		/* Read-write output flux map  */ 
     const MAP_UINT1 *ldd, 	/* ldd map	*/
     const MAP_REAL8 *amount,	/* amount map 	*/
     const MAP_REAL8 *threshold)/* transport threshold map */
{
    switch(PerformAccu(state, flux, ldd, amount, threshold, Threshold)) {
	 case 2: Error("accuthreshold: Domain error on parameters");
	 case 1: return 1; /* for both 1 and 2 */
	}
	return 0;
}
