#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <math.h> /* sqrt */
#include "app.h"  /* appUnit */

/* global header (opt.) and mapdim's prototypes "" */
#include "api.h"
#include "api_p.h"


/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

/* (LIBRARY_INTERNAL) */
REAL8 testApiArea     = -1;
/* (LIBRARY_INTERNAL) */
REAL8 testApiSide     = -1;
/* (LIBRARY_INTERNAL) */
REAL8 testApiDiagonal = -1;
/* (LIBRARY_INTERNAL) */
int   testApiYproj    =  0;
/* size_t   testApiNrRows   = 0; */
/* size_t   testApiNrCols   = 0; */
/* (LIBRARY_INTERNAL) */
BOOL  testApiInit     = FALSE;

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/

#ifdef DEBUG
/* (LIBRARY_INTERNAL)
 */
int TestApiInitTest(const char *whoCares)
{
	PRECOND(testApiInit);
  (void)whoCares; // Shut up compiler
	return 1;
}
#endif

/* area of a cell depending on appUnitTrue
 */
REAL8 Area(void)
{
	PRECOND(TestApiInitTest("Area"));
	return appUnitTrue ? testApiArea: 1;
}

/* cell length depending on appUnitTrue
 */
REAL8 Side(void)
{
	PRECOND(TestApiInitTest("Side"));
	return appUnitTrue ? testApiSide: 1;
}

/* diagonal cell length depending on appUnitTrue
 */
REAL8 Diagonal(void)
{
	PRECOND(TestApiInitTest("Diagonal"));
	return appUnitTrue ? testApiDiagonal: sqrt((double)2);
}

/* y projection factor
 * y projection factor, used in move function, to translate
 *  the input x and y
 */
REAL8 YProjectionFactor(void)
{
	PRECOND(TestApiInitTest("YProjectionFactor"));
	return appUnitTrue ? testApiYproj : 1;
}
