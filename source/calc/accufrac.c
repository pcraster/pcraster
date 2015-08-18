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
#include "accu.h"

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
/* Performs fraction function
 * Return negative value
 * Returns fraction of amount given by val.
 */
static REAL8 Fraction(
	REAL8 amount,		/* amount to evaluate */
	REAL8 fraction)		/* fraction */
{
	REAL8 val;
	if ( 0 > fraction || fraction > 1)
		return -1;
	val = amount * fraction; 
	return val;
}

/* Determines the new state and flux for each cell.
 * The new state is the amount and incoming fluxes minus the transport
 * fraction of the amount. Assumes a sound spatial ldd map present.
 * Assumes also an amount and a fraction map present.
 * Returns 0 if successful, non-zero in case of an error. 
 */
int AccuFraction(
     MAP_REAL8 *state,			/* Read-write state map */ 
     MAP_REAL8 *flux,			/* Read-write flux map */ 
     const MAP_UINT1 *ldd, 		/* ldd map */
     const MAP_REAL8 *amount,		/* amount map */
     const MAP_REAL8 *fracflux)		/* transport fraction map */
{
    switch(PerformAccu(state, flux, ldd, amount, fracflux, Fraction)) {
	 case 2: Error("accufraction: Domain error on parameters");
	 case 1: return 1; /* for both 1 and 2 */
	}
	return 0;
}
