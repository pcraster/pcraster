#ifndef INCLUDED_AG_BOOLEANRASTERDRAWER
#define INCLUDED_AG_BOOLEANRASTERDRAWER

#include "ag_BooleanDrawProps.h"
#include "ag_RasterDrawer.h"



namespace ag {
  // BooleanRasterDrawer declarations.
  class Raster;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class BooleanRasterDrawer: public RasterDrawer
{

  friend class BooleanRasterDrawerTest;

private:

  Raster const*    _raster;

  BooleanDrawProps _properties;

  void             draw                (QPainter& painter,
                                        QRect const& indices,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const override;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BooleanRasterDrawer (Raster const* raster,
                                        dal::SpaceDimensions const& dimensions,
                                        BooleanDrawProps const& properties);

  /* virtual */    ~BooleanRasterDrawer() override;

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
