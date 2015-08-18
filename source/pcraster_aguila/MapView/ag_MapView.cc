#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_AG_MAPVIEW
#include "ag_MapView.h"
#define INCLUDED_AG_MAPVIEW
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the MapView class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MAPVIEW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MAPVIEW MEMBERS
//------------------------------------------------------------------------------

MapView::MapView(
         Alignment alignment,
         QWidget* parent)

  : BufferedWidget(alignment, parent)

{
}



MapView::~MapView()
{
}



bool MapView::extentIsValid() const
{
  return d_extent.isValid();
}



// QPointF MapView::translation() const
// {
//   assert(scaleIsValid());
// 
//   // Position upper left corner of the map in the upper left corner of the
//   // widget.
//   QPointF result(-extent().topLeft());
// 
//   if(alignment() == Center) {
//     // Add a translation to center the map in the widget.
//     // result += QPointF(
//     //      ((width() / scale()) - extent().width()) / 2.0,
//     //      ((height() / scale()) - extent().height()) / 2.0);
// 
//     result += QPointF(
//          (width() - (scale() * extent().width())) / (2.0 * scale()),
//          (height() - (scale() * extent().height())) / (2.0 * scale()));
// 
//     // result += QPointF(anchor() / scale());
//     // result -= QPointF(extent().width() / 2.0, extent().height() / 2.0);
//   }
// 
//   // Take translation because of zooming and panning into account.
//   result += d_offset;
// 
//   return result;
// }



QRectF const& MapView::extent() const
{
  return d_extent;
}



//! Resets the extent of the map.
/*!
  \param     extent New extent to use.

  This function must be called once before a map can be drawn.
*/
void MapView::initializeExtent(
         QRectF const& extent)
{
  d_extent = extent;
}



void MapView::updateBuffer(
         QRect const& area)
{
}



void MapView::initializeWidget()
{
  d_worldTransform.reset();

  // Put the origin of the widget in the center.
  d_worldTransform.translate(width() / 2, height() / 2);

  if(extentIsValid()) {
    double widthScale = width() / d_extent.width();
    double heightScale = height() / d_extent.height();
    double scale;

    if(widthScale < heightScale) {
      scale = widthScale;
    }
    else {
      scale = heightScale;
    }

    // Scale the coordinates so the map fits in the widget.
    d_worldTransform.scale(scale, scale);

    // // Position center of the map in the center of the widget.
    // d_worldTransform.translate(
    //      -extent().left() - (extent().width() / 2.0),
    //      -extent().top() - (extent().height() / 2.0));

    // if(alignment() == Center) {
    //   // Add a translation to center the map in the widget.
    //   // result += QPointF(
    //   //      ((width() / scale()) - extent().width()) / 2.0,
    //   //      ((height() / scale()) - extent().height()) / 2.0);

    //   result += QPointF(
    //        (width() - (scale() * extent().width())) / (2.0 * scale()),
    //        (height() - (scale() * extent().height())) / (2.0 * scale()));

    //   // result += QPointF(anchor() / scale());
    //   // result -= QPointF(extent().width() / 2.0, extent().height() / 2.0);
    // }

    // // Take translation because of zooming and panning into account.
    // result += d_offset;

    setDirty(true);
  }
}



// void MapView::zoomTo(
//          QPoint const& position,
//          qreal factor)
// {
//   // Position the map coordinate under position in the center of the view.
//   // Zoom by factor.
// 
//   QPointF translation(d_worldTransform.m31(), d_worldTransform.m32());
// 
// 
// 
//   qreal scale(this->scale());
// 
//   d_worldTransform.reset();
//   d_worldTransform.scale(factor * scale, factor * scale);
//   d_worldTransform.translate(translation.x(), translation.y());
// 
// 
// }



void MapView::zoomBy(
         double factor)
{
  d_worldTransform.scale(factor, factor);
  setDirty(true);
}



void MapView::focusOn(
         QPoint const& position)
{
  QPointF translation(
         (width() / 2.0) - position.x(),
         (height() / 2.0) - position.y());
  translation /= scale();

  if(!translation.isNull()) {
    d_worldTransform.translate(translation.x(), translation.y());
    setDirty(true);
  }
}



double MapView::scale() const
{
  assert(dal::comparable(
         worldTransform().m11(), worldTransform().m22()));

  return worldTransform().m11();
}



QTransform const& MapView::worldTransform() const
{
  return d_worldTransform;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

