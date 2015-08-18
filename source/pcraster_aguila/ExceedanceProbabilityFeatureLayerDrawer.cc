#include "ExceedanceProbabilityFeatureLayerDrawer.h"

// External headers.
#include <QPainter>

// Project headers.

// Module headers.
#include "ag_FeatureLayer.h"
#include "ag_RangeDrawProps.h"



/*!
  \file
  This file contains the implementation of the ExceedanceProbabilityFeatureLayerDrawer class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC EXCEEDANCEPROBABILITYFEATURELAYERDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF EXCEEDANCEPROBABILITYFEATURELAYERDRAWER MEMBERS
//------------------------------------------------------------------------------

ExceedanceProbabilityFeatureLayerDrawer::ExceedanceProbabilityFeatureLayerDrawer(
         FeatureLayer const* layer,
         dal::SpaceDimensions const& dimensions,
         RangeDrawProps const& drawProperties)

  : FeatureLayerDrawer(layer, dimensions),
    _drawProperties(drawProperties)

{
}



ExceedanceProbabilityFeatureLayerDrawer::~ExceedanceProbabilityFeatureLayerDrawer()
{
}



void ExceedanceProbabilityFeatureLayerDrawer::draw(
         QPainter& painter,
         long int featureId,
         QPainterPath const& path) const
{
  REAL4 value;
  layer().value<REAL4>(featureId, value);

  painter.setBrush(pcr::isMV(value)
         ? Qt::transparent
         : _drawProperties.colour(REAL4(1.0) - value));

  painter.drawPath(path);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

