#ifndef INCLUDED_AG_ORDINALRASTERDRAWER
#define INCLUDED_AG_ORDINALRASTERDRAWER



// External headers.
#include <boost/noncopyable.hpp>

// Project headers.

// Module headers.
#include "ag_OrdinalDrawProps.h"
#include "ag_RasterDrawer.h"



namespace ag {
  // OrdinalRasterDrawer declarations.
  class Raster;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class OrdinalRasterDrawer: public RasterDrawer
{

  friend class OrdinalRasterDrawerTest;

private:

  Raster const*    _raster;

  OrdinalDrawProps _properties;

  void             draw                (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OrdinalRasterDrawer (Raster const* raster,
                                        dal::SpaceDimensions const& dimensions,
                                        OrdinalDrawProps const& properties);

  /* virtual */    ~OrdinalRasterDrawer              ();

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
