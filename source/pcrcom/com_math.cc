#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Math class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class MathPrivate
{
public:

  MathPrivate()
  {
  }

  ~MathPrivate()
  {
  }

};

} // namespace com
*/

/*! interpolate between 2 values
 *
 * \pre x is in range [x1,x2] or [x2,x1]
 */
double com::interpolate2(
    double x,
    double x1,
    double y1,
    double x2,
    double y2)
{
  if (x1 == x2)
     return y1;
  DEVELOP_PRECOND(limUnordered(x,x1,x2) == x); // is in range x1-x2
  double w1=std::fabs(x1-x);
  double w2=std::fabs(x2-x);
  return ((y1*w2)+(y2*w1))/(w1+w2);
}


/* integer division
* Fdiv() divides x by abs(y), returning an integer value I (in double
* format) such that abs(I)*abs(y) <= x < abs(I+1)*abs(y). I has the
* same sign as x. If y is zero, a runtime error will occur
* returns
*  the integer division value as a floating-point number.
*/
double Fdiv(double x, /* x    */
           double y) /*  y */
{
 double t = floor(fabs(x)/fabs(y)); 
 return x < 0 ? -t : t;
}


//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATH MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MATH MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

extern "C" int com_equalEpsilonFloat(float a, float b)
{
  return com::equal_epsilon<float>(a,b);
}
extern "C" int com_equalEpsilonDouble(double a, double b)
{
  return com::equal_epsilon<double>(a,b);
}

