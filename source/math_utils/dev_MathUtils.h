#ifndef INCLUDED_DEV_MATHUTILS
#define INCLUDED_DEV_MATHUTILS



#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

#ifndef INCLUDED_BOOST_TUPLE_TUPLE
#include <boost/tuple/tuple.hpp>
#define INCLUDED_BOOST_TUPLE_TUPLE
#endif

// Project headers.

// Module headers.



namespace dev {

//! Limit a \a value to fit into a specified interval.
/*!
  \param   value Input value.
  \param   low low boundary.
  \param   high high boundary.
  \return  Limited version of \a value.
  \todo    Test.

 If \a value is smaller than low then low is returned.
 If \a value is higher than high than high is returned.
 Else \a value is returned.
*/
template<class T>
inline T limit(
         T const& value,
         T const& low,
         T const& high)
{
  assert(low <= high);

  T result(value);

  if(value < low) {
    result = low;
  }
  else if(value > high) {
    result = high;
  }

  return result;
}



template <class T>
T hypot(T x, T y, T z)
{
  return std::sqrt(x*x + y*y + z*z);
}

} // namespace dev

#endif
