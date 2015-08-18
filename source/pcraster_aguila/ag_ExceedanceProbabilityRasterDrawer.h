#ifndef INCLUDED_AG_EXCEEDANCEPROBABILITYRASTERDRAWER
#define INCLUDED_AG_EXCEEDANCEPROBABILITYRASTERDRAWER



// External headers.

// Project headers.

// Module headers.
#include "ag_RangeDrawProps.h"
#include "ag_RasterDrawer.h"



namespace ag {
  // ExceedanceProbabilityRasterDrawer declarations.
  class Raster;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
  \todo      This class is a copy of ScalarRasterDrawer. Obviously, we need
             to refactor them. In a hurry now...
*/
class ExceedanceProbabilityRasterDrawer: public RasterDrawer
{

  friend class ExceedanceProbabilityRasterDrawerTest;

private:

  Raster const*    _raster;

  RangeDrawProps   _properties;

  void             drawSingleColour    (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

  void             drawMultipleColours (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

  void             drawColourFill      (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

  void             drawContours        (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

  void             draw                (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ExceedanceProbabilityRasterDrawer(
                                        Raster const* raster,
                                        dal::SpaceDimensions const& dimensions,
                                        RangeDrawProps const& properties);

  /* virtual */    ~ExceedanceProbabilityRasterDrawer              ();

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
