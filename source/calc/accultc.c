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

/* Performs limited transport capacity function for a cell.
 * Returns flux if or the limited flux due to capacity.
 */
static REAL8 Capacity(
	REAL8 amount,		/* amount to evaluate */
	REAL8 capacity)		/* capacity */
{
	return MIN(capacity, amount);
}

/* Determines the new state and flux for each cell.
 * The new state is the amount and incoming fluxes minus the transport
 * capacity or 0. Assumes a sound spatial ldd map present. Assumes also
 * an amount and a transport capacity map present.
 * Returns 0 if successful, non-zero otherwise 
 */
int AccuLtc(
     MAP_REAL8 *state,		/* Read-write output state map  */ 
     MAP_REAL8 *flux,		/* Read-write output flux map  */ 
     const MAP_UINT1 *ldd, 	/* ldd map	*/
     const MAP_REAL8 *amount,	/* amount map 	*/
     const MAP_REAL8 *transcap)	/* transport capacity map */
{
    switch(PerformAccu(state, flux, ldd, amount, transcap, Capacity)) {
	 case 2: Error("accucapacity: Domain error on parameters");
	 case 1: return 1; /* for both 1 and 2 */
	}
	return 0;
}
