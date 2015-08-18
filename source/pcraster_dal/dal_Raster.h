#ifndef INCLUDED_DAL_RASTER
#define INCLUDED_DAL_RASTER




// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_MATRIX
#include "dal_Matrix.h"
#define INCLUDED_DAL_MATRIX
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif



namespace dal {
  // Raster declarations.
}



namespace dal {



//! This is the Dataset class for spatial raster data.
/*!
  It is assumed by the Raster class that y coordinates increase from south
  to north.

  \todo  Add meta info to the properties of the the raster: number of bands
         in the dataset, interpretation of the values (RGB, grey levels,
         attribute values, reflections, ...).
*/
class PCR_DAL_DECL Raster: public Matrix
{

  friend class RasterTest;

private:

  //! Dimensional properties of the raster.
  RasterDimensions _dimensions;

  /// //! Cell size.
  /// double           d_cellSize;

  /// //! Western-most x-coordinate.
  /// double           d_west;

  /// //! Northern-most y-coordinate.
  /// double           d_north;

protected:

                   Raster              (DatasetType datasetType,
                                        size_t nrRows,
                                        size_t nrCols,
                                        double cellSize,
                                        double west,
                                        double north,
                                        TypeId typeId);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Raster              (size_t nrRows,
                                        size_t nrCols,
                                        double cellSize,
                                        double west,
                                        double north,
                                        TypeId typeId);

                   Raster              (RasterDimensions const& dimensions,
                                        TypeId typeId);

                   Raster              (Raster const& rhs);

  virtual          ~Raster             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Raster&          operator=           (Raster const& rhs);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  RasterDimensions const& dimensions   () const;

  double           cellSize            () const;

  double           north               () const;

  double           south               () const;

  double           west                () const;

  double           east                () const;

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



} // namespace dal

#endif
