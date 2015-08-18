#ifndef INCLUDED_AG_FEATURELAYERDRAWER
#define INCLUDED_AG_FEATURELAYERDRAWER



// External headers.
#include <set>

// Project headers.

// Module headers.
#include "ag_MapDrawer.h"



class QPainterPath;
class OGRLineString;
class OGRMultiLineString;
class OGRMultiPoint;
class OGRMultiPolygon;
class OGRPoint;
class OGRPolygon;
namespace ag {
  // FeatureLayerDrawer declarations.
  class FeatureLayer;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class FeatureLayerDrawer: public MapDrawer
{

  friend class FeatureLayerDrawerTest;

private:

  FeatureLayer const& d_layer;

  void             drawPoint           (QPainter& painter,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper,
                                        long int featureId,
                                        OGRPoint const& point) const;

  void             drawLine            (QPainter& painter,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper,
                                        long int featureId,
                                        OGRLineString const& line) const;

  void             drawPolygon         (QPainter& painter,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper,
                                        long int featureId,
                                        OGRPolygon const& polygon) const;

  void             drawMultiPoint      (QPainter& painter,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper,
                                        long int featureId,
                                        OGRMultiPoint const& multiPoint) const;

  void             drawMultiLine       (QPainter& painter,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper,
                                        long int featureId,
                                        OGRMultiLineString const& multiLine) const;

  void             drawMultiPolygon    (QPainter& painter,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper,
                                        long int featureId,
                                        OGRMultiPolygon const& multiPolygon) const;

  void             draw                (QPainter& painter,
                                        std::set<long int> const& featureIds,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

  void             draw                (QPainter& painter,
                                        long int featureId,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

  virtual void     draw                (QPainter& painter,
                                        long int featureId,
                                        QPainterPath const& path) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FeatureLayerDrawer  (FeatureLayer const* layer,
                                        dal::SpaceDimensions const& dimensions);

  virtual          ~FeatureLayerDrawer ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             draw                (QPainter& painter,
                                        QRectF const& dirtyMapAreaInPixels,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

  FeatureLayer const& layer            () const;

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
