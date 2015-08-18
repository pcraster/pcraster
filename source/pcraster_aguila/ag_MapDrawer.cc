#include "ag_MapDrawer.h"

// External headers.
#include <QPainter>
#include <QRect>

// Project headers.
#include "dal_MathUtils.h"
#include "dal_SpaceDimensions.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the MapDrawer class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MAPDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MAPDRAWER MEMBERS
//------------------------------------------------------------------------------

MapDrawer::MapDrawer(
         dal::SpaceDimensions const& overallDimensions,
         dal::SpaceDimensions const& attributeDimensions)

  : _overallDimensions(overallDimensions),
    _attributeDimensions(attributeDimensions)

{
}



MapDrawer::~MapDrawer()
{
}



QRectF MapDrawer::envelopeInPixels(
         QPointF const& anchor,
         double zoom,
         QPointF const& offset,
         double scale) const
{
  double width  = zoom * _overallDimensions.longitudinalExtent() / scale;
  double height = zoom * _overallDimensions.latitudinalExtent() / scale;
  double left   = anchor.x() + (zoom * offset.x() / scale) - (0.5 * width);
  double top    = anchor.y() + (zoom * offset.y() / scale) - (0.5 * height);

  return QRectF(left, top, width, height);
}



boost::tuple<QwtScaleMap, QwtScaleMap> MapDrawer::mappers(
         QRectF const& envelopeInPixels) const
{
  // map -> pixels
  QwtScaleMap xMapper, yMapper;
  xMapper.setScaleInterval(_overallDimensions.west(), _overallDimensions.east());
  xMapper.setPaintInterval(envelopeInPixels.left(), envelopeInPixels.right());
  yMapper.setScaleInterval(_overallDimensions.north(), _overallDimensions.south());
  yMapper.setPaintInterval(envelopeInPixels.top(), envelopeInPixels.bottom());

  return boost::make_tuple(xMapper, yMapper);
}



double MapDrawer::scale(
         QwtScaleMap const& mapper) const
{
  return mapper.pDist() / mapper.sDist();
}



void MapDrawer::draw(
         QPainter& painter,
         QRectF const& dirtyScreenArea,
         QPointF const& anchor,
         double zoom,
         QPointF const& offset,
         double scale)
{
  if(IS_NAN(zoom) || IS_INFINITE(zoom) || IS_NAN(scale) || IS_INFINITE(scale)) {
    return;
  }

  if(!(dirtyScreenArea.isValid() && !dal::comparable<double>(scale, 0.0))) {
    return;
  }

  // Which pixels are potentially drawn by the dataset.
  QRectF envelopeInPixels = this->envelopeInPixels(anchor, zoom, offset, scale);

  // If this fails, then return here. The zoom level is such that no pixels
  // are drawn.
  assert(envelopeInPixels.isValid());

  // Mappers for translating between real world coordinates and screen
  // coordinates.
  QwtScaleMap xMapper, yMapper;
  boost::tie(xMapper, yMapper) = mappers(envelopeInPixels);

  // Actual area of the screen that possibly contains stuff to draw.
  // QRect dirtyMapAreaInPixels = envelopeInPixels & dirtyScreenArea;
  QRectF dirtyMapAreaInPixels = envelopeInPixels & dirtyScreenArea;

  if(dirtyMapAreaInPixels.isEmpty()) {
    return;
  }

  draw(painter, dirtyMapAreaInPixels, xMapper, yMapper);
}



void MapDrawer::drawOutline(
         QPainter& painter,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  double westScreen = xMapper.transform(_attributeDimensions.west());
  double eastScreen = xMapper.transform(_attributeDimensions.east());
  double northScreen = yMapper.transform(_attributeDimensions.north());
  double southScreen = yMapper.transform(_attributeDimensions.south());

  painter.setRenderHint(QPainter::Antialiasing);
  painter.setPen(Qt::black);
  painter.setBrush(Qt::NoBrush);

  painter.drawRect(westScreen, northScreen, eastScreen - westScreen + 1,
         southScreen - northScreen + 1);
  painter.drawLine(westScreen, northScreen, eastScreen, southScreen);
  painter.drawLine(eastScreen, northScreen, westScreen, southScreen);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

