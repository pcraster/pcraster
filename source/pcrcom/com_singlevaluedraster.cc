#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SINGLEVALUEDRASTER
#include "com_singlevaluedraster.h"
#define INCLUDED_COM_SINGLEVALUEDRASTER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the SingleValuedRaster class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class SingleValuedRasterPrivate
{
public:

  SingleValuedRasterPrivate()
  {
  }

  ~SingleValuedRasterPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SINGLEVALUEDRASTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SINGLEVALUEDRASTER MEMBERS
//------------------------------------------------------------------------------

template<typename T>
com::SingleValuedRaster<T>& com::SingleValuedRaster<T>::add(
       const IRaster<T>& /* rhs */)
{
  // FIXME
  return *this;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



