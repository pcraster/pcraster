#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_VSPATIAL
#include "calc_vspatial.h"
#define INCLUDED_CALC_VSPATIAL
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the VSpatial class.
*/



namespace calc {

//------------------------------------------------------------------------------

/*
class VSpatialPrivate
{
public:

  VSpatialPrivate()
  {
  }

  ~VSpatialPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC VSPATIAL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VSPATIAL MEMBERS
//------------------------------------------------------------------------------

template<typename OutType,
         typename InType >
 VSpatial<OutType,InType>::VSpatial(InType const* data):
   d_data(data)
 {}

template<typename OutType,
         typename InType >
 VSpatial<OutType,InType>::~VSpatial()
 {}




/* NOT IMPLEMENTED
//! Copy constructor.
VSpatial::VSpatial(
         VSpatial const& rhs)

  : Base(rhs)

{
}
*/



/* NOT IMPLEMENTED
//! Assignment operator.
VSpatial& VSpatial::operator=(
         VSpatial const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/

template<typename OutType,
         typename InType >
bool VSpatial<OutType,InType>::isMV(size_t i) const {
   return pcr::isMV(d_data[i]);
}

template<typename OutType,
         typename InType >
OutType VSpatial<OutType,InType>::operator[](size_t i)const  {
   DEVELOP_PRECOND(!pcr::isMV(d_data[i]));
   return d_data[i];
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
template class VSpatial<INT4,UINT1>;
template class VSpatial<INT4,INT4>;
template class VSpatial<double,UINT1>;
template class VSpatial<double,INT4>;
template class VSpatial<double,REAL4>;

} // namespace calc

