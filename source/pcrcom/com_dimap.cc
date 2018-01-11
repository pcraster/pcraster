#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_DIMAP
#include "com_dimap.h"
#define INCLUDED_COM_DIMAP
#endif

#ifndef INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#include <boost/math/special_functions/round.hpp>
#define INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#endif


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

const double com::DiMap::logMin = 1.0e-150;



const double com::DiMap::logMax = 1.0e150;



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

/*!
  The double and integer intervals are both set to [0,1].
*/
com::DiMap::DiMap()

  : d_x1(0.0), d_x2(1.0), d_y1(0), d_y2(1), d_cnv(1.0)

{
}



/*!
  \param   i1 First border of integer interval.
  \param   i2 Second border of integer interval.
  \param   d1 First border of double interval.
  \param   d2 Second border of double interval.
  \param   logarithmic Logarithmic mapping, true or false. Defaults to false.

  Constructs a com::DiMap instance with initial integer and double intervals.
*/
com::DiMap::DiMap(int i1, int i2, double d1, double d2, bool logarithmic)

  : d_x1(0.0), d_x2(1.0), d_y1(0), d_y2(1), d_cnv(1.0)

{
  d_log = logarithmic;
  setIntRange(i1,i2);
  setDblRange(d1, d2);
}



com::DiMap::~DiMap()
{
}



/*!
  \param   x Value.
  \return  If \a x lies inside or at the border of the double range.
*/
bool com::DiMap::contains(double x) const
{
  return ( (x >= MIN(d_x1, d_x1)) && (x <= MAX(d_x1, d_x2)));
}



/*!
  \param   x Value.
  \return  If \a x lies inside or at the border of the integer range.
*/
bool com::DiMap::contains(int x) const
{
  return (x >= MIN(d_y1, d_y1)) && (x <= MAX(d_y1, d_y2));
}



/*!
  \param   d1 First border.
  \param   d2 Second border.
  \param   lg Logarithmic (true) or linear (false) scaling. Defaults to false.
*/
void com::DiMap::setDblRange(double d1, double d2, bool lg)
{
  if(lg)
  {
    d_log = true;
    if(d1 < logMin)
      d1 = logMin;
    else if(d1 > logMax)
      d1 = logMax;

    if(d2 < logMin)
      d2 = logMin;
    else if (d2 > logMax)
      d2 = logMax;

    d_x1 = std::log(d1);
    d_x2 = std::log(d2);
  }
  else
  {
    d_log = false;
    d_x1  = d1;
    d_x2  = d2;
  }

  newFactor();
}



/*!
  \param   i1 First border.
  \param   i2 Second border.
*/
void com::DiMap::setIntRange(int i1, int i2)
{
  d_y1 = i1;
  d_y2 = i2;
  newFactor();
}



/*!
  \param  x Value in double interval.
  \return Transformed value in int interval.
  \sa     invTransform(), limTransform()

  linear mapping: round(i1 + (i2 - i1) / (d2 - d1) * (x - d1))

  logarithmic mapping: round(i1 + (i2 - i1) / log(d2 / d1) * log(x / d1))

  The specified value is allowed to lie outside the intervals. If you want to
  limit the returned value, use limTransform().
*/
int com::DiMap::transform(double x) const
{
  if(d_log)
    return (d_y1 + boost::math::iround((std::log(x) - d_x1) * d_cnv));
  else
    return (d_y1 + boost::math::iround((x - d_x1) * d_cnv));
}



/*!
  \param   y Integer value to be transformed.
  \return  Transformed value in double interval.
  \sa      transform(), limTransform()

  linear mapping: d1 + (d2 - d1) / (i2 - i1) * (y - i1)

  logarithmic mapping: d1 + (d2 - d1) / log(i2 / i1) * log(y / i1)
*/
double com::DiMap::invTransform(int y) const
{
  if(d_cnv == 0.0)
  {
    return 0.0;
  }
  else
  {
    if(d_log)
      return std::exp(d_x1 + double(y - d_y1) / d_cnv);
    else
      return (d_x1 + double(y - d_y1) / d_cnv);
  }
}



/*!
  \param   x Value to be transformed.
  \return  Transformed value.
  \sa      transform(), invTransform()

  The function is similar to transform, but limits the input value to the
  nearest border of the map's double interval if it lies outside that interval.
*/
int com::DiMap::limTransform(double x) const
{
  if(x > MAX(d_x1, d_x2))
    x = MAX(d_x1, d_x2);
  else if(x < MIN(d_x1, d_x2))
    x = MIN(d_x1, d_x2);

  return transform(x);
}



/*!
  \param   x Value to be transformed.
  \return  Transformed value.
  \sa

  linear mapping: i1 + (i2 - i1) / (d2 - d1) * (x - d1)
  logarithmic mapping: i1 + (i2 - i1) / log(d2 / d1) * log(x / d1)

  This function is similar to transform(), but makes the integer interval
  appear to be double.
*/
//------------------------------------------------------------
double com::DiMap::xTransform(double x) const
{
  double rv;

  if(d_log) {
    rv = static_cast<double>(d_y1) + (std::log(x) - d_x1) * d_cnv;
  }
  else {
    rv = static_cast<double>(d_y1) + (x - d_x1) * d_cnv;
  }

  return rv;
}



void com::DiMap::newFactor()
{
  if(d_x2 != d_x1)
    d_cnv = static_cast<double>(d_y2 - d_y1) / (d_x2 - d_x1);
  else
    d_cnv = 0.0;
}



double com::DiMap::d1() const
{
  return d_x1;
}



double com::DiMap::d2() const
{
  return d_x2;
}



int com::DiMap::i1() const
{
  return d_y1;
}



int com::DiMap::i2() const
{
  return d_y2;
}



double com::DiMap::scale() const
{
  return d_cnv;
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------

/*!
  \fn      bool com::DiMap::logarithmic() const
  \return  True if the double interval is scaled logarithmically.
*/


