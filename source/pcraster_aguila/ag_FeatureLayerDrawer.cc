#include "ag_FeatureLayer.h"

// External headers.
#include <QPainter>
#include <qwt_scale_map.h>
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
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper,
         long int /* featureId */,
         OGRPoint const& point) const
{
  qreal x = xMapper.transform(point.getX());
  qreal y = yMapper.transform(point.getY());

  painter.drawEllipse(x - 2, y - 2, 5, 5);
}



void FeatureLayerDrawer::drawLine(
         QPainter& painter,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper,
         long int /* featureId */,
         OGRLineString const& line) const
{
  QVector<QLineF> lines;

  qreal x1 = xMapper.transform(line.getX(0));
  qreal y1 = yMapper.transform(line.getY(0));
  qreal x2, y2;

  for(int i = 1; i < line.getNumPoints(); ++i) {
    x2 = xMapper.transform(line.getX(i));
    y2 = yMapper.transform(line.getY(i));

    lines.push_back(QLineF(x1, y1, x2, y2));

    x1 = x2;
    y1 = y2;
  }

  painter.drawLines(lines);
}



void FeatureLayerDrawer::drawPolygon(
         QPainter& painter,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper,
         long int featureId,
         OGRPolygon const& polygon) const
{
  // No exterior ring if the polygon is empty.
  if(polygon.getExteriorRing()) {
    QPainterPath path;
    OGRLinearRing const& exteriorRing(*polygon.getExteriorRing());
    QPolygonF ring(exteriorRing.getNumPoints());

    for(int i = 0; i < exteriorRing.getNumPoints(); ++i) {
      ring[i] = QPointF(
        xMapper.transform(exteriorRing.getX(i)),
        yMapper.transform(exteriorRing.getY(i)));
    }

    path.addPolygon(ring);
    QPolygonF hole;

    for(int i = 0; i < polygon.getNumInteriorRings(); ++i) {
      OGRLinearRing const& interiorRing(*polygon.getInteriorRing(i));
      hole.resize(interiorRing.getNumPoints());

      for(int j = 0; j < interiorRing.getNumPoints(); ++j) {
        hole[j] = QPointF(
           xMapper.transform(interiorRing.getX(j)),
           yMapper.transform(interiorRing.getY(j)));
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
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  if(!(d_layer.isRead() && !d_layer.isEmpty())) {
    return;
  }

  // Area of the feature layer that possibly contains features to draw.
  double west, north, east, south;
  west = xMapper.invTransform(dirtyMapAreaInPixels.left());
  east = xMapper.invTransform(dirtyMapAreaInPixels.right());
  north = yMapper.invTransform(dirtyMapAreaInPixels.top());
  south = yMapper.invTransform(dirtyMapAreaInPixels.bottom());

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

  draw(painter, featureIds, xMapper, yMapper);
}



void FeatureLayerDrawer::draw(
         QPainter& painter,
         std::set<long int> const& featureIds,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  BOOST_FOREACH(long int featureId, featureIds) {
    draw(painter, featureId, xMapper, yMapper);
  }
}



void FeatureLayerDrawer::draw(
         QPainter& painter,
         long int featureId,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper) const
{
  OGRGeometry const& geometry = d_layer.geometry(featureId);

  switch(geometry.getGeometryType()) {
    case wkbPoint:
    case wkbPoint25D: {
      OGRPoint const& point(dynamic_cast<OGRPoint const&>(geometry));
      drawPoint(painter, xMapper, yMapper, featureId, point);
      break;
    }
    case wkbLineString:
    case wkbLineString25D: {
      OGRLineString const& line(
            dynamic_cast<OGRLineString const&>(geometry));
      drawLine(painter, xMapper, yMapper, featureId, line);
      break;
    }
    case wkbPolygon:
    case wkbPolygon25D: {
      OGRPolygon const& polygon(
            dynamic_cast<OGRPolygon const&>(geometry));
      drawPolygon(painter, xMapper, yMapper, featureId, polygon);
      break;
    }
    case wkbMultiPoint:
    case wkbMultiPoint25D: {
      OGRMultiPoint const& multiPoint(
            dynamic_cast<OGRMultiPoint const&>(geometry));
      drawMultiPoint(painter, xMapper, yMapper, featureId, multiPoint);
      break;
    }
    case wkbMultiLineString:
    case wkbMultiLineString25D: {
      OGRMultiLineString const& multiLine(
            dynamic_cast<OGRMultiLineString const&>(geometry));
      drawMultiLine(painter, xMapper, yMapper, featureId, multiLine);
      break;
    }
    case wkbMultiPolygon:
    case wkbMultiPolygon25D: {
      OGRMultiPolygon const& multiPolygon(
            dynamic_cast<OGRMultiPolygon const&>(geometry));
      drawMultiPolygon(painter, xMapper, yMapper, featureId, multiPolygon);
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
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper,
         long int featureId,
         OGRMultiPoint const& multiPoint) const
{
  int nrGeometries = multiPoint.getNumGeometries();

  for(int i = 0; i < nrGeometries; ++i) {
    OGRPoint const& point(dynamic_cast<OGRPoint const&>(
        *multiPoint.getGeometryRef(i)));
    drawPoint(painter, xMapper, yMapper, featureId, point);
  }
}



void FeatureLayerDrawer::drawMultiLine(
         QPainter& painter,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper,
         long int featureId,
         OGRMultiLineString const& multiLine) const
{
  int nrGeometries = multiLine.getNumGeometries();

  for(int i = 0; i < nrGeometries; ++i) {
    OGRLineString const& line(dynamic_cast<OGRLineString const&>(
        *multiLine.getGeometryRef(i)));
    drawLine(painter, xMapper, yMapper, featureId, line);
  }
}



void FeatureLayerDrawer::drawMultiPolygon(
         QPainter& painter,
         QwtScaleMap const& xMapper,
         QwtScaleMap const& yMapper,
         long int featureId,
         OGRMultiPolygon const& multiPolygon) const
{
  int nrGeometries = multiPolygon.getNumGeometries();

  for(int i = 0; i < nrGeometries; ++i) {
    OGRPolygon const& polygon(dynamic_cast<OGRPolygon const&>(
        *multiPolygon.getGeometryRef(i)));
    drawPolygon(painter, xMapper, yMapper, featureId, polygon);
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

