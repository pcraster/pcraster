#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h"
#include "app.h"     /* see  Do_s_2_d() */
#include "csftypes.h"

/* global header (opt.) and sunfunc's prototypes "" */
#include "sunfunc.h"


/* headers of this app. modules called */

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

void Do_pred(
  INT4 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (val[i] != MV_INT4 && val[i] > INT4_MIN)
    val[i]--;
}

void Do_succ(
  INT4 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (val[i] != MV_INT4 && val[i] < INT4_MAX)
    val[i]++;
}

void Do_not(
  UINT1 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (val[i] != MV_UINT1)
    val[i] = (UINT1)(!(val[i]));
}


void Do_sqrt(
  REAL4 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    if (val[i] >= (REAL4)0.0)
      val[i] = (REAL4)sqrt(val[i]);
    else
      SET_MV_REAL4(val+i);
  }
}


/*  trigonometric and other mathematical functions :
     Function     Argument Range     Return Value Range

     acos         -1 to 1            0 to pi
     asin         -1 to 1            -pi/2 to pi/2
     atan         No limit           -pi/2 to pi/2

     cos, sin, and tan trigonometric functions return the cosine,
     sine, and tangent, respectively, of <x>:

     If <x> is large, a partial loss of significance in the result may
     occur in a trigonometric function. In this case, the function
     generates a PLOSS error. If <x> is so large that significance is
     completely lost, the function prints a TLOSS message to stderr and
     returns 0. In both cases, errno is set to ERANGE.

     The exp function returns the exponential function of its floating-
     point argument <x>.  _LHUGE_VAL for expl) on overflow and set errno
     to ERANGE; on underflow, they return 0 but do not set errno.

     The log and log10 functions calculate the natural logarithm and
     base-10 logarithm of <x>, respectively.
     These functions return the logarithm result. If <x> is negative,
     the functions print a DOMAIN error message to stderr, return the
     value -HUGE_VAL (or -_LHUGE_VAL for the long double functions),
     and set errno to EDOM. If <x> is 0, the functions print a SING
     error message to stderr, return the value -HUGE_VAL, and set errno
     to ERANGE.
*/


void Do_acos(
  REAL4 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    if (fabs(val[i]) > (REAL4)1.0)
      SET_MV_REAL4(val+i);
    else
      val[i] = (REAL4)acos((REAL4)val[i]);
  }
}

void Do_asin(
  REAL4 *val,
  size_t n)
{
  size_t i;
  double t;

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    t = val[i];
    if (fabs(t) > (double)1.0)
      SET_MV_REAL4(val+i);
    else
    {
      t = asin(t);
      val[i] = (REAL4)ScaleRad(t);
    }
  }
}


void Do_atan(
  REAL4 *val,
  size_t n)
{
  size_t i;
  double  t;

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    t = atan((REAL4)val[i]);
    val[i] = (REAL4)ScaleRad(t);
  }
}


void Do_tan_d(
  REAL4 *val,
  size_t n)
{
  size_t i;
  REAL4 tan_mv1 = (REAL4)(M_PI*0.5);
  REAL4 tan_mv2 = (REAL4)(M_PI*1.5);

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    if ( (val[i] == -1) ||  tan_mv1 == val[i]
           || tan_mv2 == val[i])
      SET_MV_REAL4(val+i);
    else
      val[i] = (REAL4)tan((REAL4)val[i]);
  }
}

void Do_sin_d(
  REAL4 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    if (val[i] != (REAL4)-1)
      val[i] = (REAL4)sin((REAL4)val[i]);
    else
      SET_MV_REAL4(val+i);
  }
}

void Do_cos_d(
  REAL4 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    if (val[i] != (REAL4)-1)
      val[i] = (REAL4)cos((REAL4)val[i]);
    else
      SET_MV_REAL4(val+i);
  }
}

int Do_s_2_d(REAL4 *v, size_t n)
{
  size_t i;
  double (* f)(double x) =
   (appDirection == APP_RADIANS) ? ScaleRad : Deg2Rad;
  for(i=0;i < n; i++)
   if ( (!IS_MV_REAL4(v+i)) )
    v[i] = (REAL4)f(v[i]);
  return 0;
}

void Do_cos_s(REAL4 *v, size_t n)
{ Do_s_2_d(v,n); Do_cos_d(v,n); }
void Do_sin_s(REAL4 *v, size_t n)
{ Do_s_2_d(v,n); Do_sin_d(v,n); }
void Do_tan_s(REAL4 *v, size_t n)
{ Do_s_2_d(v,n); Do_tan_d(v,n); }

void Do_ln(
  REAL4 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    if (val[i] > (REAL4)0.0)
      val[i] = (REAL4)log((REAL4)val[i]);
    else
      SET_MV_REAL4(val+i);
  }
}


void Do_log10(
  REAL4 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    if (val[i] > (REAL4)0.0)
      val[i] = (REAL4)log10((REAL4)val[i]);
    else
      SET_MV_REAL4(val+i);
  }
}

/* up to 34 is ok, 35 => infinite
 */
void Do_fac(
  REAL4 *val,
  size_t n)
{
  size_t i;

  for(i=0; i < n; i++)
  if (! IS_MV_REAL4(val+i))
  {
    float v = floor(val[i]);
    if (v > (REAL4)0.0 && v <= (REAL4)34.0 && v == val[i])
    {
      float r = 1,n = 1;
      while (n <= v)
      {
        r *= n;
        n += 1;
      }
      val[i] = r;
    }
    else
      SET_MV_REAL4(val+i);
  }
}

#define REAL4_UN_FUNC(funcName, funcOp)\
void Do_##funcName(REAL4 *val, size_t n)\
{\
  size_t i;\
  for(i=0; i < n; i++)\
    if (! IS_MV_REAL4(val+i))\
     val[i] = (REAL4)funcOp((REAL4)val[i]);\
}

/*
  same function as REAL4_UN_FUNC:
void Do_it(REAL4 *val, size_t n)
{
  size_t i;
  for(i=0; i < n; i++)
   if (! IS_MV_REAL4(val+i))
   {
          double newVal;
    newVal = exp((REAL4)val[i]);
    val[i] = (REAL4)newVal;
  }
}
*/

REAL4_UN_FUNC(umin, -)
REAL4_UN_FUNC(uadd, +)
REAL4_UN_FUNC(sqr, sqr)
REAL4_UN_FUNC(rounddown, floor)
REAL4_UN_FUNC(roundoff, Rint)
REAL4_UN_FUNC(roundup, ceil)
REAL4_UN_FUNC(abs, fabs)
REAL4_UN_FUNC(exp, exp)

#undef REAL4_UN_FUNC
