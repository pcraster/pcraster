#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_KERNEL
#include "geo_kernel.h"
#define INCLUDED_GEO_KERNEL
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Kernel class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC KERNEL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF KERNEL MEMBERS
//------------------------------------------------------------------------------

//! Constructs a Kernel.
/*!
  \param     radius Radius of the kernel.
  \param     raster Raster to operate kernel on.
*/
template<class T, class U>
geo::Kernel<T, U>::Kernel(size_t radius, const SimpleRaster<T>& raster)

  : d_radius(radius), d_raster(raster)

{
}



//! Destructor.
/*!
*/
template<class T, class U>
geo::Kernel<T, U>::~Kernel()
{
}



//! Returns the size.
/*!
  \return    Size.
  \sa        radius(), nrCells()

  The size of a kernel is 2 * radius + 1.
*/
template<class T, class U>
size_t geo::Kernel<T, U>::size() const
{
  return 2 * d_radius + 1;
}



//! Returns the number of cells.
/*!
  \return    Number of cells.
  \sa        radius(), size()
*/
template<class T, class U>
size_t geo::Kernel<T, U>::nrCells() const
{
  return size() * size();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



