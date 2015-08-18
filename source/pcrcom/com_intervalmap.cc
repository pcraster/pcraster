#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_INTERVALMAP
#include "com_intervalmap.h"
#define INCLUDED_COM_INTERVALMAP
#endif
// Module headers.

/*!
  \file
  This file contains the implementation of the IntervalMap class.
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC INTERVALMAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF INTERVALMAP MEMBERS
//------------------------------------------------------------------------------


/*
com::IntervalMap::IntervalMap()
{
}



com::IntervalMap::~IntervalMap()
{
}
*/

/* NOT IMPLEMENTED
//! Assignment operator.
com::IntervalMap& com::IntervalMap::operator=(const IntervalMap& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
com::IntervalMap::IntervalMap(const IntervalMap& rhs):
  Base(rhs)
{
}
*/

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

template <typename R>
bool com::noOverlap(const std::vector<const Interval<R> *>& v)
{
  IntervalMap<bool,R> m; // TODO actually an IntervalSet<R> suffices
  for(size_t i=0; i<v.size(); ++i)
    if (!m.insertInterval(*v[i]))
      return false;
  return true;

}

namespace com {

template bool noOverlap<float>(const std::vector<const Interval<float> *>& v);
template bool noOverlap<double>(const std::vector<const Interval<double> *>& v);

}
