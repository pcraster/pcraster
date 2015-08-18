#include "com_rangemap.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the RangeMap class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RANGEMAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RANGEMAP MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     lower1 Lower border of first range.
  \param     upper1 Upper border of first range.
  \param     lower2 Lower border of second range.
  \param     upper2 Upper border of second range.
*/
template<class T, class U>
com::RangeMap<T, U>::RangeMap(const T& lower1, const T& upper1,
                   const U& lower2, const U& upper2)
{
  setRanges(lower1, upper1, lower2, upper2);
}



//! Destructor.
/*!
*/
template<class T, class U>
com::RangeMap<T, U>::~RangeMap()
{
}



//! Resets the range information.
/*!
  \param     lower1 Lower border of first range.
  \param     upper1 Upper border of first range.
  \param     lower2 Lower border of second range.
  \param     upper2 Upper border of second range.
*/
template<class T, class U>
void com::RangeMap<T, U>::setRanges(const T& lower1, const T& upper1,
                   const U& lower2, const U& upper2)
{
  d_lower1 = lower1;
  d_upper1 = upper1;
  d_lower2 = lower2;
  d_upper2 = upper2;

  if(d_lower1 != d_upper1) {
    d_scale = static_cast<double>(d_upper2 - d_lower2) / (d_upper1 - d_lower1);
  }
  else {
    d_scale = 0.0;
  }
}



//! Returns the mapped value from the first range.
/*!
  \param     value Value from first range.
  \return    Mapped value.

  The returned value is a member of the second range.
*/
template<class T, class U>
U com::RangeMap<T, U>::map(const T& value)
{
  return static_cast<U>(d_lower2 + (value - d_lower1) * d_scale);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------

template class com::RangeMap<double, double>;

