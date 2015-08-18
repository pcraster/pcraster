#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h" 

/* global header (opt.) and dirconv's prototypes "" */


/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
#define MAX_DEG ((long double)360.0)
/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* scales a directional value to the domain [0,endScale>
 * returns the scaled value 
 */
static double ScaleDirectional(double x, double endScale)
{
  int neg = (x < 0);
  x = fabs(x);
  x = fmod(x, endScale);
  neg = neg && x != 0;
#ifdef DEBUG
  {
    double y = neg ? (endScale-x) : x;
    POSTCOND(0 <= y && y < endScale);
  }
#endif
  return neg ? (endScale-x) : x;
}

/* scales a value in degrees to the domain [0,360.0>
 * returns the scaled value 
 */
double ScaleDeg(double x)
{
  return ScaleDirectional(x, MAX_DEG);
}

/* scales a value in radians to the domain [0,2PI>
 * returns the scaled value 
 */
double ScaleRad(double x)
{
  return ScaleDirectional(x, M_2PI);
}

/* convert a radian value to degree value
 * returns scaled result in domain [0,360>
 */
double Rad2Deg(double x) /* value in radians */
{
  x = ScaleRad(x);
#ifdef DEBUG
  {
    double y = ((x)/M_2PI) * MAX_DEG;
    POSTCOND(0 <= y && y < MAX_DEG);
  }
#endif
  return (double)((((long double)(x))/(2*((long double)M_PI))) * MAX_DEG);
}

/* convert a degree value to radian value
 * returns scaled result in domain [0,2PI>
 */
double Deg2Rad(double x) /* value in degrees */
{
  x = ScaleDeg(x);
#ifdef DEBUG
  {
    double y = ((x)/MAX_DEG) * M_2PI;
    POSTCOND(0 <= y && y < M_2PI);
  }
#endif
  return ((x)/MAX_DEG) * M_2PI;
}
