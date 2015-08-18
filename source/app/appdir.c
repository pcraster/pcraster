#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "app.h"
#include "mathx.h"

/* apps. called */

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


/* Converts degrees to radians or performs mod(2pi) on radians.
 * When opion --radians is set, all radian values are converted to
 * the domain: [0, 2pi>.
 * When the option --degrees is used, all values are converted to
 * radians in the same domain. Note that the flat (-1) IS NOT detected
 * as a special value in this function.
 * Returns new radian value scaled to [0,2PI>.
 */
double AppInputDirection(double val)  /* value to convert */
{
  if(appDirection == APP_DEGREES)
    return Deg2Rad(val);
  POSTCOND(0 <= val && val < M_2PI);
  return val;
}

/* Converts radians to degrees if option --degrees was used.
 * The value -1 is not converted.
 * Returns input value in degrees or radians.
 */
double AppOutputDirection(double rad)    /* value to convert */
{
   if(rad != -1 && appDirection == APP_DEGREES)
     rad = Rad2Deg(rad);
   return rad;
}
