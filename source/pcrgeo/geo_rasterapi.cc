#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RASTERAPI
#include "geo_rasterapi.h"
#define INCLUDED_GEO_RASTERAPI
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_RASTER
#include "com_raster.h"
#define INCLUDED_COM_RASTER
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the RasterAPI class.
*/



//------------------------------------------------------------------------------

/*
namespace geo {

class RasterAPIPrivate
{
public:

  RasterAPIPrivate()
  {
  }

  ~RasterAPIPrivate()
  {
  }

};

} // namespace geo
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERAPI MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTERAPI MEMBERS
//------------------------------------------------------------------------------

//! ctor
template<typename T>
geo::RasterAPI<T>::RasterAPI(const RasterSpace& rasterSpace)

 : com::IRaster<T>(rasterSpace.nrRows(), rasterSpace.nrCols()),
   d_data(new com::Raster<T>(rasterSpace.nrRows(), rasterSpace.nrCols()))

{
}



//! ctor
/*
 * \warning   This owns \a data and will delete it on destruction.
 */
template<typename T>
geo::RasterAPI<T>::RasterAPI(const RasterSpace& rasterSpace,
         com::IRaster<T>* data)

 : com::IRaster<T>(rasterSpace.nrRows(), rasterSpace.nrCols()),
   d_data(data)

{
}



//! dtor
template<typename T>
geo::RasterAPI<T>::~RasterAPI()
{
  delete d_data;
}



template<typename T>
geo::RasterAPI<T>& geo::RasterAPI<T>::add(const com::IRaster<T>& from)
{
  ensurePointOperationPossible(from.isSpatial());
  d_data->add(from);
  return *this;
}



template<typename T>
geo::RasterAPI<T>& geo::RasterAPI<T>::minus(const com::IRaster<T>& from)
{
  ensurePointOperationPossible(from.isSpatial());
  d_data->minus(from);
  return *this;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



