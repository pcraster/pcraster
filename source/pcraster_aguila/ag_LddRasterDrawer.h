#ifndef INCLUDED_AG_LDDRASTERDRAWER
#define INCLUDED_AG_LDDRASTERDRAWER



// External headers.

// Project headers.

// Module headers.
#include "ag_LddDrawProps.h"
#include "ag_RasterDrawer.h"



namespace ag {
  // LddRasterDrawer declarations.
  class Raster;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class LddRasterDrawer: public RasterDrawer
{

  friend class LddRasterDrawerTest;

private:

  Raster const*    _raster;

  LddDrawProps     _properties;

  QColor           _penColour;

  void             draw                (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LddRasterDrawer     (Raster const* raster,
                                        dal::SpaceDimensions const& dimensions,
                                        LddDrawProps const& properties,
                                        QColor const& penColour);

  /* virtual */    ~LddRasterDrawer    ();

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
