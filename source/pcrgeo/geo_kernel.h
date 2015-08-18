#ifndef INCLUDED_GEO_KERNEL
#define INCLUDED_GEO_KERNEL



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
  // Kernel declarations.
}



namespace geo {



//! The Kernel class is a base class for kernels.
/*!
  A kernel classifies the centre cell of a square area based on information of
  all cells in the area.
*/
// T: value type of source raster for kernel.
// U: value type of result of kernel.
template<class T, class U>
class Kernel
{

private:

  //! Radius.
  size_t           d_radius;

  //! Raster to operate kernel on.
  const SimpleRaster<T>& d_raster;

  //! Assignment operator. NOT IMPLEMENTED.
  Kernel&          operator=           (const Kernel&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Kernel              (const Kernel&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Kernel              (size_t radius,
                                        const SimpleRaster<T>& raster);

  virtual          ~Kernel             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           radius              () const;

  size_t           size                () const;

  size_t           nrCells             () const;

  const SimpleRaster<T>& raster        () const;

  //! Processes the kernel on the raster for position \a row, \a col.
  /*!
    \param     row Row coordinate.
    \param     col Col coordinate.
    \return    New value, result of kernel operation.
  */
  virtual U        process             (size_t row,
                                        size_t col) const = 0;

  virtual U        borderCellValue     () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

template<class T, class U>
inline const SimpleRaster<T>& geo::Kernel<T, U>::raster() const
{
  return d_raster;
}

//! Returns the radius.
/*!
  \return    Radius.
  \sa        size(), nrCells()
*/
template<class T, class U>
inline size_t geo::Kernel<T, U>::radius() const
{
  return d_radius;
}

//! Returns the default border cell value.
/*!
  \return    Border cell value.

  The target raster value type must have a default constructor.
*/
template<class T, class U>
inline U geo::Kernel<T, U>::borderCellValue() const
{
  return U();
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
