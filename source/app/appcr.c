#include "stddefx.h"

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "csf.h"

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

/* return the default cell representation for a value scale
 * AppDefaultCellRepr returns the default cell repr for
 * a value scale and checks appDouble and appLarge to see if REAL4 
 * must become REAL8 or INT4 must become UINT1
 * returns
 *  a cell representation constant (CR_something)
 */
CSF_CR AppDefaultCellRepr(
       CSF_VS vs) /* value scale */
{
	UINT2 c = RdefaultCellRepr(vs);
	if (c == CR_REAL4 && appDouble)
		c = CR_REAL8;
	if (c == CR_INT4 && !appLarge)
		c = CR_UINT1;
	return c;
}
