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

/* Performs trigger function
 * Returns amount or zero determined by triggerVal.
 */
static REAL8 Trigger(
	REAL8 amount,		/* amount to evaluate */
	REAL8 triggerVal)	/* trigger value */
{
	if (triggerVal < 0)
	        amount = triggerVal; /* return err. neg. value */
	if(triggerVal <= amount)
		return amount;
	return 0;
}

/* Determines the new state and flux for each cell.
 * The new state is the amount and incoming fluxes plus the amount 
 * if the trigger value is exceeded or 0 otherwise.
 * Assumes a sound spatial ldd map, an amount map and a transport 
 * trigger map to be present.
 * Returns 0 if successful, non-zero otherwise 
 */
int AccuTrigger(
     MAP_REAL8 *state,		/* Read-write output state map  */ 
     MAP_REAL8 *flux,		/* Read-write output flux map  */ 
     const MAP_UINT1 *ldd, 	/* ldd map	*/
     const MAP_REAL8 *amount,	/* amount map 	*/
     const MAP_REAL8 *trigger)	/* transport trigger map */
{
    switch(PerformAccu(state, flux, ldd, amount, trigger, Trigger)) {
	 case 2: Error("accutrigger: Domain error on parameters");
	 case 1: return 1; /* for both 1 and 2 */
	}
	return 0;
}
