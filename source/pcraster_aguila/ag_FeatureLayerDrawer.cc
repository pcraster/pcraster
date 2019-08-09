#include "ag_FeatureLayer.h"

// External headers.
#include <QPainter>
#include <ogr_core.h>
#include <ogr_feature.h>
#include <ogr_geometry.h>

// Project headers.
#include "dal_SpaceDimensions.h"

// Module headers.
#include "ag_FeatureLayerDrawer.h"



/*!
  \file
  This file contains the implementation of the FeatureLayerDrawer class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FEATURELAYERDRAWER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FEATURELAYERDRAWER MEMBERS
//------------------------------------------------------------------------------

FeatureLayerDrawer::FeatureLayerDrawer(
         FeatureLayer const* layer,
         dal::SpaceDimensions const& dimensions)

  : MapDrawer(dimensions, layer->dimensions()),
    d_layer(*layer)

{
}



FeatureLayerDrawer::~FeatureLayerDrawer()
{
}



void FeatureLayerDrawer::drawPoint(
         QPainter& painter,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world,
         long int /* featureId */,
         OGRPoint const& point) const
{
  QPointF p = QPointF(point.getX(), point.getY());
  qreal x = world_to_screen.map(p).x();
  qreal y = world_to_screen.map(p).y();

  painter.drawEllipse(x - 2, y - 2, 5, 5);
}



void FeatureLayerDrawer::drawLine(
         QPainter& painter,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world,
         long int /* featureId */,
         OGRLineString const& line) const
{
  QVector<QLineF> lines;

  QPointF p = QPointF(line.getX(0), line.getY(0));
  qreal x1 = world_to_screen.map(p).x();
  qreal y1 = world_to_screen.map(p).y();

  qreal x2, y2;

  for(int i = 1; i < line.getNumPoints(); ++i) {
    p = QPointF(line.getX(i), line.getY(i));
    x2 = world_to_screen.map(p).x();
    y2 = world_to_screen.map(p).y();

    lines.push_back(QLineF(x1, y1, x2, y2));

    x1 = x2;
    y1 = y2;
  }

  painter.drawLines(lines);
}



void FeatureLayerDrawer::drawPolygon(
         QPainter& painter,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world,
         long int featureId,
         OGRPolygon const& polygon) const
{
  // No exterior ring if the polygon is empty.
  if(polygon.getExteriorRing()) {
    QPainterPath path;
    OGRLinearRing const& exteriorRing(*polygon.getExteriorRing());
    QPolygonF ring(exteriorRing.getNumPoints());

    QPointF p;
    for(int i = 0; i < exteriorRing.getNumPoints(); ++i) {
      ring[i] = world_to_screen.map(QPointF(exteriorRing.getX(i), exteriorRing.getY(i)));
    }

    path.addPolygon(ring);
    QPolygonF hole;

    for(int i = 0; i < polygon.getNumInteriorRings(); ++i) {
      OGRLinearRing const& interiorRing(*polygon.getInteriorRing(i));
      hole.resize(interiorRing.getNumPoints());

      for(int j = 0; j < interiorRing.getNumPoints(); ++j) {
        hole[j] = world_to_screen.map(QPointF(interiorRing.getX(j), interiorRing.getY(j)));
      }

      path.addPolygon(hole);
    }

    draw(painter, featureId, path);
  }
}



void FeatureLayerDrawer::draw(
         QPainter& painter,
         long int /* featureId */,
         QPainterPath const& path) const
{
  painter.drawPath(path);
}



void FeatureLayerDrawer::draw(
         QPainter& painter,
         QRectF const& dirtyMapAreaInPixels,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world) const
{
  if(!(d_layer.isRead() && !d_layer.isEmpty())) {
    return;
  }

  // Area of the feature layer that possibly contains features to draw.
  QRectF dirtyMapAreaInWorldCoordinates(
    screen_to_world.map( QPointF(dirtyMapAreaInPixels.left(), dirtyMapAreaInPixels.top()) ),
    screen_to_world.map( QPointF(dirtyMapAreaInPixels.right(), dirtyMapAreaInPixels.bottom()) )
  );

  double west, north, east, south;
  west = dirtyMapAreaInWorldCoordinates.left();
  east = dirtyMapAreaInWorldCoordinates.right();
  north = dirtyMapAreaInWorldCoordinates.top();
  south = dirtyMapAreaInWorldCoordinates.bottom();

  using Box = dal::FeatureLayerGeometries::Box;
  using Point = dal::FeatureLayerGeometries::Point;

  // Extent the box a little bit to allow geometries that are positioned
  // very close to the box' edges to be selected. Otherwise these geometries
  // might not be selected, possibly due to floating point rounding
  // errors.
  Box box(
    Point(
        west - std::abs(0.01 * west),
        south - std::abs(0.01 * south)
    ),
    Point(
        east + std::abs(0.01 * east),
        north + std::abs(0.01 * north)
    )
  );

  std::set<long int> featureIds;
  d_layer.featureIds(box, std::inserter(featureIds, featureIds.begin()));

  // Default painter settings.
  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(Qt::black);
  painter.setBrush(Qt::NoBrush);

  draw(painter, featureIds, world_to_screen, screen_to_world);
}



void FeatureLayerDrawer::draw(
         QPainter& painter,
         std::set<long int> const& featureIds,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world) const
{
  for(long int featureId : featureIds) {
    draw(painter, featureId, world_to_screen, screen_to_world);
  }
}



void FeatureLayerDrawer::draw(
         QPainter& painter,
         long int featureId,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world) const
{
  OGRGeometry const& geometry = d_layer.geometry(featureId);

  switch(geometry.getGeometryType()) {
    case wkbPoint:
    case wkbPoint25D: {
      OGRPoint const& point(dynamic_cast<OGRPoint const&>(geometry));
      drawPoint(painter, world_to_screen, screen_to_world, featureId, point);
      break;
    }
    case wkbLineString:
    case wkbLineString25D: {
      OGRLineString const& line(
            dynamic_cast<OGRLineString const&>(geometry));
      drawLine(painter, world_to_screen, screen_to_world, featureId, line);
      break;
    }
    case wkbPolygon:
    case wkbPolygon25D: {
      OGRPolygon const& polygon(
            dynamic_cast<OGRPolygon const&>(geometry));
      drawPolygon(painter, world_to_screen, screen_to_world, featureId, polygon);
      break;
    }
    case wkbMultiPoint:
    case wkbMultiPoint25D: {
      OGRMultiPoint const& multiPoint(
            dynamic_cast<OGRMultiPoint const&>(geometry));
      drawMultiPoint(painter, world_to_screen, screen_to_world, featureId, multiPoint);
      break;
    }
    case wkbMultiLineString:
    case wkbMultiLineString25D: {
      OGRMultiLineString const& multiLine(
            dynamic_cast<OGRMultiLineString const&>(geometry));
      drawMultiLine(painter, world_to_screen, screen_to_world, featureId, multiLine);
      break;
    }
    case wkbMultiPolygon:
    case wkbMultiPolygon25D: {
      OGRMultiPolygon const& multiPolygon(
            dynamic_cast<OGRMultiPolygon const&>(geometry));
      drawMultiPolygon(painter, world_to_screen, screen_to_world, featureId, multiPolygon);
      break;
    }
    case wkbGeometryCollection:
    case wkbGeometryCollection25D:
    case wkbLinearRing:
    case wkbNone:
    case wkbUnknown:
    default: {
      break;
    }
  }
}



void FeatureLayerDrawer::drawMultiPoint(
         QPainter& painter,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world,
         long int featureId,
         OGRMultiPoint const& multiPoint) const
{
  int nrGeometries = multiPoint.getNumGeometries();

  for(int i = 0; i < nrGeometries; ++i) {
    OGRPoint const& point(dynamic_cast<OGRPoint const&>(
        *multiPoint.getGeometryRef(i)));
    drawPoint(painter, world_to_screen, screen_to_world, featureId, point);
  }
}



void FeatureLayerDrawer::drawMultiLine(
         QPainter& painter,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world,
         long int featureId,
         OGRMultiLineString const& multiLine) const
{
  int nrGeometries = multiLine.getNumGeometries();

  for(int i = 0; i < nrGeometries; ++i) {
    OGRLineString const& line(dynamic_cast<OGRLineString const&>(
        *multiLine.getGeometryRef(i)));
    drawLine(painter, world_to_screen, screen_to_world, featureId, line);
  }
}



void FeatureLayerDrawer::drawMultiPolygon(
         QPainter& painter,
         QTransform const& world_to_screen,
         QTransform const& screen_to_world,
         long int featureId,
         OGRMultiPolygon const& multiPolygon) const
{
  int nrGeometries = multiPolygon.getNumGeometries();

  for(int i = 0; i < nrGeometries; ++i) {
    OGRPolygon const& polygon(dynamic_cast<OGRPolygon const&>(
        *multiPolygon.getGeometryRef(i)));
    drawPolygon(painter, world_to_screen, screen_to_world, featureId, polygon);
  }
}



FeatureLayer const& FeatureLayerDrawer::layer() const
{
  return d_layer;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

