#ifndef INCLUDED_AG_SCALARRASTERDRAWER
#define INCLUDED_AG_SCALARRASTERDRAWER



// External headers.

// Project headers.

// Module headers.
#include "ag_RangeDrawProps.h"
#include "ag_RasterDrawer.h"



namespace ag {
  // ScalarRasterDrawer declarations.
  class Raster;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class ScalarRasterDrawer: public RasterDrawer
{

  friend class ScalarRasterDrawerTest;

private:

  Raster const*    _raster;

  RangeDrawProps   _properties;

  void             drawSingleColour    (QPainter& painter,
                                        QRect const& indices,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const;

  void             drawMultipleColours (QPainter& painter,
                                        QRect const& indices,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const;

  void             drawColourFill      (QPainter& painter,
                                        QRect const& indices,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const;

  void             drawContours        (QPainter& painter,
                                        QRect const& indices,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const;

  void             draw                (QPainter& painter,
                                        QRect const& indices,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const override;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ScalarRasterDrawer  (Raster const* raster,
                                        dal::SpaceDimensions const& dimensions,
                                        RangeDrawProps const& properties);

  /* virtual */    ~ScalarRasterDrawer() override;

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
