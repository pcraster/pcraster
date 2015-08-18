#ifndef INCLUDED_AG_DIRECTIONALRASTERDRAWER
#define INCLUDED_AG_DIRECTIONALRASTERDRAWER



// External headers.

// Project headers.

// Module headers.
#include "ag_RangeDrawProps.h"
#include "ag_RasterDrawer.h"



namespace ag {
  // DirectionalRasterDrawer declarations.
  class Raster;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class DirectionalRasterDrawer: public RasterDrawer
{

  friend class DirectionalRasterDrawerTest;

private:

  Raster const*    _raster;

  RangeDrawProps   _properties;

  void             draw                (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DirectionalRasterDrawer(
                                        Raster const* raster,
                                        dal::SpaceDimensions const& dimensions,
                                        RangeDrawProps const& properties);

  /* virtual */    ~DirectionalRasterDrawer();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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

} // namespace ag

#endif
