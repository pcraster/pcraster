#ifndef INCLUDED_GEO_KERNELSOURCEBUFFER
#define INCLUDED_GEO_KERNELSOURCEBUFFER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif



namespace geo {
  // KernelSourceBuffer declarations.
}



namespace geo {



//! The KernelSourceBuffer class buffers the input for kernels.
/*!
  This class is useful for reclassification kernels which need neighbourhood
  information to operate. If you want to create a kernel which needs to
  preprocess information (eg: scan the neighbourhood) before it can actually
  operate, this class is for you.

  The easiest way to implement such a kernel is to iterate over all cells in the
  raster, collecting all needed information from neighbouring cells and to
  classify the current cell based on this information. The big drawback of this
  approach is that a lot of information is read more than once.

  The goal of this class is to make operating such a kernel more efficient by
  remembering part of the state of the neighbourhood while the kernel advances
  to another area of the raster.

  The width of a KernelSourceBuffer equals the number of columns of the raster
  the kernel operates minus 2 * border. The height of the area equals
  2 * radius + 1 of the kernel. By calling advanceRows() on a
  KernelSourceBuffer object the area will move down to a new row. You get a
  chance to update the new cells in the area by implementing
  update(size_t, size_t, size_t, size_t, size_t, size_t).

  This class uses the notion of a border of a kernel. This is the zone around
  kernel cells which is queried while updating the source buffer and before
  operating the kernel on the buffer. In case of a neighbourhood operation,
  border is the radius of the neighbourhood to visit.

  You can savely operate the kernel in processKernel(size_t, size_t, size_t,
  size_t). All surrounding cells will have meaningfull values.
*/
// T: value type of original source raster.
// U: value type of source raster for kernel operation.
template<class T, class U>
class KernelSourceBuffer: public SimpleRaster<U>
{

private:

  //! Raster for kernel to process.
  const SimpleRaster<T>& d_raster;

  //! Radius of the kernel.
  size_t           d_radius;

  //! Border of kernel cells.
  size_t           d_border;

  //! Current kernel row.
  size_t           d_row;

  //! Assignment operator. NOT IMPLEMENTED.
  KernelSourceBuffer& operator=        (const KernelSourceBuffer&);

  //! Copy constructor. NOT IMPLEMENTED.
                   KernelSourceBuffer  (const KernelSourceBuffer&);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   KernelSourceBuffer  (const SimpleRaster<T>& raster,
                                        size_t radius,
                                        size_t border = 0);

  virtual          ~KernelSourceBuffer ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

/*
  void             setRow              (size_t row);
*/

  bool             advanceRow          ();

  //! Updates cells of the KernelSourceBuffer.
  /*!
    \param     left Left cell of rectangle to update.
    \param     top Top cell of rectangle to update.
    \param     right Right cell of rectangle to update.
    \param     bottom Bottom cell of rectangle to update.
    \param     bufferLeft Left cell in buffer where results can be placed.
    \param     bufferTop Top cell in buffer where results can be placed.

    All coordinates are relative to the input raster of the constructor. To
    be able to place an updated value into the buffer the \a bufferLeft and
    \a bufferTop arguments can be used.

    It is save to query cells which are border() cells outside of the cells to
    update.
  */
  virtual void     update              (size_t left,
                                        size_t top,
                                        size_t right,
                                        size_t bottom,
                                        size_t bufferLeft,
                                        size_t bufferTop) = 0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           border              () const;

  size_t           row                 () const;

  size_t           nrRowsToProcess     () const;

  const SimpleRaster<T>& raster        () const;

  size_t           kernelRadius        () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
