#ifndef INCLUDED_AG_NOMINALRASTERDRAWER
#define INCLUDED_AG_NOMINALRASTERDRAWER



// External headers.

// Project headers.

// Module headers.
#include "ag_NominalDrawProps.h"
#include "ag_RasterDrawer.h"



namespace ag {
  // NominalRasterDrawer declarations.
  class Raster;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class NominalRasterDrawer: public RasterDrawer
{

  friend class NominalRasterDrawerTest;

private:

  Raster const*    _raster;

  NominalDrawProps _properties;

  void             draw                (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   NominalRasterDrawer (Raster const* raster,
                                        dal::SpaceDimensions const& dimensions,
                                        NominalDrawProps const& properties);

  /* virtual */    ~NominalRasterDrawer();

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
