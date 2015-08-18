#ifndef INCLUDED_GEO_RASTERAPI
#define INCLUDED_GEO_RASTERAPI

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_IRASTER
#include "com_iraster.h"
#define INCLUDED_COM_IRASTER
#endif

#ifndef INCLUDED_COM_RASTER
#include "com_raster.h"
#define INCLUDED_COM_RASTER
#endif

// Module headers.
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif



namespace geo {
  // RasterAPI declarations.
}



namespace geo {

//! new interface
/*!
*/
template<typename T>
class RasterAPI : public com::IRaster<T>
{

private:

  RasterSpace      d_rasterSpace;

  //! Cells.
  com::IRaster<T>* d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  RasterAPI&       operator=           (const RasterAPI&);

  //! Copy constructor. NOT IMPLEMENTED.
                   RasterAPI           (const RasterAPI&);

  //! check if <i>this op= arg</i> is possible
  /*!
   * \pre !d_data->isSpatial() && argIsSpatial
   */
  void             ensurePointOperationPossible(bool argIsSpatial) {
    if(!d_data->isSpatial()) {
      PRECOND(argIsSpatial);
      com::Raster<T>* data = new com::Raster<T>(*d_data);
      delete d_data;
      d_data = data;
    }
  }

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RasterAPI           (const RasterSpace& rasterSpace);

                   RasterAPI           (const RasterSpace& rasterSpace,
                                        com::IRaster<T>* data);

   virtual         ~RasterAPI          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  RasterAPI<T>&    add                 (const com::IRaster<T>& from);

  RasterAPI<T>&    minus               (const com::IRaster<T>& from);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  /*!
    \return true if raster is represented by different values, false if
            represented by a single value.
   */
  bool             isSpatial           () const
  { return d_data->isMultiValued(); }

  T&               operator[]          (size_t index)
  { return (*d_data)[index]; }

  const T&         operator[]          (size_t index) const
  { return (*d_data)[index]; }

  bool             isMultiValued       ()const
  { return d_data->isMultiValued(index); }

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
