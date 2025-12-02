#include "ag_MapDrawer.h"
#include "dal_MathUtils.h"
#include "dal_SpaceDimensions.h"

#include <QPainter>
#include <QRect>

#include <cmath>

/*!
  \file
  This file contains the implementation of the MapDrawer class.
*/


namespace ag
{

// Code that is private to this module.
namespace detail
{

}  // namespace detail

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MAPDRAWER MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF MAPDRAWER MEMBERS
//------------------------------------------------------------------------------

MapDrawer::MapDrawer(dal::SpaceDimensions const &overallDimensions,
                     dal::SpaceDimensions const &attributeDimensions)

    : _overallDimensions(overallDimensions), _attributeDimensions(attributeDimensions)

{
}

MapDrawer::~MapDrawer()
{
}

QRectF MapDrawer::envelopeInPixels(QPointF const &anchor, double zoom, QPointF const &offset,
                                   double scale) const
{
  double const width = zoom * _overallDimensions.longitudinalExtent() / scale;
  double const height = zoom * _overallDimensions.latitudinalExtent() / scale;
  double const left = anchor.x() + (zoom * offset.x() / scale) - (0.5 * width);
  double const top = anchor.y() + (zoom * offset.y() / scale) - (0.5 * height);

  return {left, top, width, height};
}

std::tuple<QTransform, QTransform> MapDrawer::mappers(QRectF const &envelopeInPixels) const
{


  double const x = (_overallDimensions.east() - _overallDimensions.west()) /
                   (envelopeInPixels.right() - envelopeInPixels.left());
  double const y = -1.0 * std::fabs((_overallDimensions.south() - _overallDimensions.north()) /
                                    (envelopeInPixels.bottom() - envelopeInPixels.top()));
  double const dx = (-1 * x * (envelopeInPixels.left())) + _overallDimensions.west();
  double const dy = (-1 * y * (envelopeInPixels.top())) + _overallDimensions.north();

  QTransform const screen_to_world = QTransform(x, 0.0, 0.0, 0.0, y, 0.0, dx, dy, 1.0);
  QTransform const world_to_screen = screen_to_world.inverted();

  return std::make_tuple(world_to_screen, screen_to_world);
}

double MapDrawer::scale(QTransform const &mapper) const
{
  if (mapper.m11() > 0) {
    return mapper.m11();
  } else {
    assert(mapper.m22() > 0);
    return mapper.m22();
  }
}

void MapDrawer::draw(QPainter &painter, QRectF const &dirtyScreenArea, QPointF const &anchor,
                     double zoom, QPointF const &offset, double scale)
{
  if (std::isnan(zoom) || std::isinf(zoom) || std::isnan(scale) || std::isinf(scale)) {
    return;
  }

  if (!(dirtyScreenArea.isValid() && !dal::comparable<double>(scale, 0.0))) {
    return;
  }

  // Which pixels are potentially drawn by the dataset.
  QRectF const envelopeInPixels = this->envelopeInPixels(anchor, zoom, offset, scale);

  // If this fails, then return here. The zoom level is such that no pixels
  // are drawn.
  assert(envelopeInPixels.isValid());

  // Mappers for translating between real world coordinates and screen
  // coordinates.
  QTransform world_to_screen;
  QTransform screen_to_world;
  std::tie(world_to_screen, screen_to_world) = mappers(envelopeInPixels);

  // Actual area of the screen that possibly contains stuff to draw.
  // QRect dirtyMapAreaInPixels = envelopeInPixels & dirtyScreenArea;
  QRectF const dirtyMapAreaInPixels = envelopeInPixels & dirtyScreenArea;

  if (dirtyMapAreaInPixels.isEmpty()) {
    return;
  }

  draw(painter, dirtyMapAreaInPixels, world_to_screen, screen_to_world);
}

// void MapDrawer::drawOutline(
//          QPainter& painter,
//          QwtScaleMap const& xMapper,
//          QwtScaleMap const& yMapper) const
// {
//   double westScreen = xMapper.transform(_attributeDimensions.west());
//   double eastScreen = xMapper.transform(_attributeDimensions.east());
//   double northScreen = yMapper.transform(_attributeDimensions.north());
//   double southScreen = yMapper.transform(_attributeDimensions.south());
//
//   painter.setRenderHint(QPainter::Antialiasing);
//   painter.setPen(Qt::black);
//   painter.setBrush(Qt::NoBrush);
//
//   painter.drawRect(westScreen, northScreen, eastScreen - westScreen + 1,
//          southScreen - northScreen + 1);
//   painter.drawLine(westScreen, northScreen, eastScreen, southScreen);
//   painter.drawLine(eastScreen, northScreen, westScreen, southScreen);
// }


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

}  // namespace ag
