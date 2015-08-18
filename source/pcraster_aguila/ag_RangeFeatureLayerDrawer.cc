#include "ag_RangeFeatureLayerDrawer.h"

// External headers.
#include <QPainter>

// Project headers.

// Module headers.
#include "ag_FeatureLayer.h"
#include "ag_RangeDrawProps.h"


/*!
  \file
  This file contains the implementation of the RangeFeatureLayerDrawer class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RANGEFEATURELAYERDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RANGEFEATURELAYERDRAWER MEMBERS
//------------------------------------------------------------------------------

RangeFeatureLayerDrawer::RangeFeatureLayerDrawer(
         FeatureLayer const* layer,
         dal::SpaceDimensions const& dimensions,
         RangeDrawProps const& drawProperties)

  : FeatureLayerDrawer(layer, dimensions),
    _drawProperties(drawProperties)

{
}



RangeFeatureLayerDrawer::~RangeFeatureLayerDrawer()
{
}



void RangeFeatureLayerDrawer::draw(
         QPainter& painter,
         long int featureId,
         QPainterPath const& path) const
{
  REAL4 value;
  layer().value<REAL4>(featureId, value);

  painter.setBrush(pcr::isMV(value)
         ? Qt::transparent
         : _drawProperties.colour(value));

  painter.drawPath(path);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

