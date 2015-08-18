#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_RASTER
#include "com_raster.h"
#define INCLUDED_COM_RASTER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Raster class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class RasterPrivate
{
public:

  RasterPrivate()
  {
  }

  ~RasterPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTER MEMBERS
//------------------------------------------------------------------------------

template<typename T>
com::Raster<T>& com::Raster<T>::add(const IRaster<T>& rhs)
{
  PRECOND(IRaster<T>::nrCells() == rhs.nrCells());

  for(size_t i = 0; i < IRaster<T>::nrCells(); ++i) {
    if(!pcr::isMV(IRaster<T>::element(i))) {
      if(pcr::isMV(rhs.element(i))) {
        pcr::setMV(IRaster<T>::element(i));
      }
      else {
        IRaster<T>::element(i) += rhs.element(i);
      }
    }
  }

  return *this;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



