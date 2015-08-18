#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_KERNELSOURCEBUFFER
#include "geo_kernelsourcebuffer.h"
#define INCLUDED_GEO_KERNELSOURCEBUFFER
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the KernelSourceBuffer class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC KERNELAREA MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF KERNELAREA MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     raster Raster to operate kernel on.
  \param     radius Radius of the kernel.
  \param     border Border of the kernel cells.
  \warning   \a raster must be big enough for a kernel of size
             2 * \a radius + 1 and \a border.

  The buffer will be filled with the first 2 * \a radius + 1 rows. Call
  advanceRow() to move the buffer down.
*/
template<class T, class U>
geo::KernelSourceBuffer<T, U>::KernelSourceBuffer(const SimpleRaster<T>& raster,
                   size_t radius, size_t border)

  : SimpleRaster<U>(2 * radius + 1, raster.nrCols() - 2 * border),
    d_raster(raster), d_radius(radius), d_border(border), d_row(border + radius)

{
  PRECOND(radius > 0);
  PRECOND(d_raster.nrRows() >= 2 * d_radius + 1 + 2 * d_border);
  PRECOND(d_raster.nrCols() >= 2 * d_radius + 1 + 2 * d_border);
  PRECOND(static_cast<int>(d_raster.nrRows()) -
                   2 * static_cast<int>(d_border) -
                   2 * static_cast<int>(d_radius) > 0);

  // yepyep: The pure virtual version of update gets called and not the overide.
  // yepyep: Moved this call to the specialisation. Why can't we call a pure
  // yepyep: virtual function from the base's constructor? Link problem?
  // update(0, 0, nrCols() - 1, 2 * d_radius + 1);
}



//! Destructor.
/*!
*/
template<class T, class U>
geo::KernelSourceBuffer<T, U>::~KernelSourceBuffer()
{
}



template<class T, class U>
size_t geo::KernelSourceBuffer<T, U>::border() const
{
  return d_border;
}



// Sets the current position of the buffer to \a row.
/*
  \param     row Position of the upper row of the buffer.
  \warning   You can't position the buffer to row raster.nrRows() - 2 * radius
             + 1 since there would be not enough space for the kernel to
             operate.

  Nothing happens if the \a row equals the current position of the buffer.
  Otherwise update(size_t, size_t, size_t, size_t) is called to update the new
  cells in the buffer.
*/
/*
void geo::KernelSourceBuffer<T>::setRow(size_t row)
{
  PRECOND(row <= d_raster.nrRows() - 2 * d_radius + 1);

  if(row != d_row) {


  }
}
*/



//! Advances the area one row. Returns true if new data became available.
/*!
  \return    true if new data became available.
  \warning   Nothing happens if there's no more space in the raster to advance
             to.

  If the buffer is already at the bottom of the raster, than false is returned.
*/
template<class T, class U>
bool geo::KernelSourceBuffer<T, U>::advanceRow()
{
  // Test if the next position is valid.
  if(d_row + 1 + d_radius > d_raster.nrRows() - 1 - d_border) {
    return false;
  }
  else {
    // Move current values up.
    (void)std::memcpy(static_cast<void*>(this->begin()),
         static_cast<const void*>(this->begin() + this->nrCols()),
         (this->end() - (this->begin() + this->nrCols())) * sizeof(U));
    // std::copy(begin() + nrCols(), end(), begin());
    ++d_row;

    // Ask for an update of the new values.
    update(d_border, d_row + d_radius, this->nrCols(),
                   d_row + d_radius, 0, 2 * d_radius);
    return true;
  }
}



//! Returns the current position of the buffer in the raster.
/*!
  \return    Position of the buffer.
  \sa        raster()

  The returned row is the index of the row in the middle of the buffer. It is
  the index of the row which the kernel has to process / has processed. The
  buffer extends to the back and the front by radius cells.
*/
template<class T, class U>
size_t geo::KernelSourceBuffer<T, U>::row() const
{
  return d_row;
}



//! Returns the raster this buffer operates on.
/*!
  \return    Raster.
  \sa        row()
*/
template<class T, class U>
const geo::SimpleRaster<T>& geo::KernelSourceBuffer<T, U>::raster() const
{
  return d_raster;
}



//! Returns the number of rows to process with the kernel.
/*!
  \return    Number of rows to process.
*/
template<class T, class U>
size_t geo::KernelSourceBuffer<T, U>::nrRowsToProcess() const
{
  return d_raster.nrRows() - 2 * d_border - 2 * d_radius;
}



//! Returns the kernel radius.
/*!
  \return    Kernel radius.
*/
template<class T, class U>
size_t geo::KernelSourceBuffer<T, U>::kernelRadius() const
{
  return d_radius;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

