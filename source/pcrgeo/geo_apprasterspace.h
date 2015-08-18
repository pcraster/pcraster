#ifndef INCLUDED_GEO_APPRASTERSPACE
#define INCLUDED_GEO_APPRASTERSPACE

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

namespace geo {

//! Transform coordinates in raster space adhering to PCRaster global settings
/*!

   The global settings settings such --unittrue, --unitcell, --coorul, etc.
   adjust the way coordinate conversions are handled. This class describes
   raster space that implement these conversions.

   Code is copied from lib/app
*/

class AppRasterSpace
{
  // default copy constructor is ok

private:
  /* we do not derive, but embed, we do not want to have anything
           altered after the space is configured
   */
  const RasterSpace d_rs;
  const bool        d_trueCoords;
    double          d_xInc,d_yInc;

private:
  //! Assignment operator. NOT IMPLEMENTED.
  AppRasterSpace& operator= (const AppRasterSpace &);
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   AppRasterSpace      (const RasterSpace& rs);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  double cellSize() const;

  double cellArea() const;

  void getCoords( size_t row, size_t col, double &x, double &y) const;
};

//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

} // namespace geo

#endif
