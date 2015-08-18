#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_KERNELENGINE
#include "geo_kernelengine.h"
#define INCLUDED_GEO_KERNELENGINE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the KernelEngine class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC KERNELENGINE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF KERNELENGINE MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     sourceBuffer Buffer for kernel to operate on.
  \param     targetRaster Raster to fill with new values.
  \param     kernel Classifier.
  \warning   The source raster of the buffer must have the same number of rows
             and columns as the target raster. The buffer must be positioned on
             the back of the source raster. There must be enough space in the
             source raster for the kernel to operate on.
*/
template<class T, class U, class V>
geo::KernelEngine<T, U, V>::KernelEngine(KernelSourceBuffer<T, U>& sourceBuffer,
                   SimpleRaster<V>& targetRaster, Kernel<U, V>& kernel)

  : com::LabeledProgressTracked<com::ProgressBar>(), 
    d_sourceBuffer(sourceBuffer), d_targetRaster(targetRaster), d_kernel(kernel)

{
  PRECOND(sourceBuffer.row() == sourceBuffer.border() + d_kernel.radius());
  PRECOND(sourceBuffer.raster().nrRows() == targetRaster.nrRows());
  PRECOND(sourceBuffer.raster().nrCols() == targetRaster.nrCols());
  PRECOND(sourceBuffer.raster().nrRows() >= kernel.size());
  PRECOND(sourceBuffer.raster().nrCols() >= kernel.size());
  // PRECOND(sourceBuffer == kernel.raster());

  // The border of the target raster cannot be processed by the kernel. This
  // border is border + kernel radius cells wide. The cells in this region
  // are assigned the value of Kernel::borderCellValue().

  V borderValue = d_kernel.borderCellValue();

  for(size_t b = 0; b < sourceBuffer.border() + d_kernel.radius(); ++b) {

    // Top and bottom row(s).
    for(size_t c = 0; c < targetRaster.nrCols(); ++c) {
      targetRaster.cell(0 + b, c) = borderValue;
      targetRaster.cell(targetRaster.nrRows() - 1 - b, c) = borderValue;
    }

    // Left and right col(s).
    for(size_t r = 0; r < targetRaster.nrRows(); ++r) {
      targetRaster.cell(r, 0 + b) = borderValue;
      targetRaster.cell(r, targetRaster.nrCols() - 1 - b) = borderValue;
    }
  }
}



//! Destructor.
/*!
*/
template<class T, class U, class V>
geo::KernelEngine<T, U, V>::~KernelEngine()
{
}



template<class T, class U, class V>
geo::KernelSourceBuffer<T, U>& geo::KernelEngine<T, U, V>::sourceBuffer()
{
  return d_sourceBuffer;
}



template<class T, class U, class V>
geo::SimpleRaster<V>& geo::KernelEngine<T, U, V>::targetRaster()
{
  return d_targetRaster;
}



template<class T, class U, class V>
const geo::Kernel<U, V>& geo::KernelEngine<T, U, V>::kernel() const
{
  return d_kernel;
}



//! Operates the kernel on the values in the source buffer.
/*!
  The buffer is moved across the source raster and the kernel is processed
  on all cells in the buffer. In the end the target raster is filled with kernel
  generated values.

  Since this function can take a long time to finish it notifies finished
  actions to the com::ProgressTracked base.
*/
template<class T, class U, class V>
void geo::KernelEngine<T, U, V>::run()
{

  com::ProgressTracked<com::LabeledProgressTracker<com::ProgressBar> >::
         init(d_sourceBuffer.nrRowsToProcess());

  // Buffer is already filled with values of the back of the raster.
  for(size_t i = d_kernel.radius();
                   i < d_kernel.raster().nrCols() - d_kernel.radius();
                   ++i) {
    d_targetRaster.cell(d_sourceBuffer.row(), d_sourceBuffer.border() + i) =
                   d_kernel.process(d_kernel.radius(), i);
  }

  finishedStep();

  // Move buffer to the front.
  while(d_sourceBuffer.advanceRow()) {

    for(size_t i = d_kernel.radius();
                   i < d_kernel.raster().nrCols() - d_kernel.radius();
                   ++i) {
      d_targetRaster.cell(d_sourceBuffer.row(), d_sourceBuffer.border() + i) =
                   d_kernel.process(d_kernel.radius(), i);
    }

    finishedStep();
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



