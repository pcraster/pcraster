#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RASTER
#include "geo_raster.h"
#define INCLUDED_GEO_RASTER
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif



/*!
  \file
   implement Raster template

*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

/*!
  \param     nr Number of rows.
  \param     nc Number of columns.
*/
template<class T>
geo::Raster<T>::Raster(size_t nr, size_t nc, double cellSize,
                       double left, double top, Projection proj)

  : SimpleRaster<T>(nr, nc),
    d_space(nr, nc, cellSize, left, top, proj)

{
}



template<class T>
geo::Raster<T>::Raster(const RasterSpace& rs)

  : SimpleRaster<T>(rs.nrRows(), rs.nrCols()),
    d_space(rs)

{
}



template<class T>
geo::Raster<T>::Raster(const Raster &rhs)

  : SimpleRaster<T>(rhs),
    d_space(rhs.d_space)

{
}



template<class T>
geo::Raster<T>::~Raster()
{
}



template<class T>
const geo::RasterSpace& geo::Raster<T>::space() const
{
  return d_space;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


