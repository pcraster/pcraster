#ifndef INCLUDED_DAL_FEATURELAYERGEOMETRIES
#include "dal_FeatureLayerGeometries.h"
#define INCLUDED_DAL_FEATURELAYERGEOMETRIES
#endif

// External headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_OGR_GEOMETRY
#include <ogr_geometry.h>
#define INCLUDED_OGR_GEOMETRY
#endif

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the FeatureLayerGeometries class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FEATURELAYERGEOMETRIES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FEATURELAYERGEOMETRIES MEMBERS
//------------------------------------------------------------------------------

FeatureLayerGeometries::FeatureLayerGeometries(
         double west,
         double north,
         double east,
         double south)

  : _envelope(west, north, east, south)

{
}



FeatureLayerGeometries::~FeatureLayerGeometries()
{
  typedef std::map<FeatureId, OGRGeometry*>::value_type value_type;

  BOOST_FOREACH(value_type pair, _geometryByFeatureId) {
    OGRGeometryFactory::destroyGeometry(pair.second);
  }
}



void FeatureLayerGeometries::insert(
         FeatureId featureId,
         OGRGeometry* geometry)
{
  assert(featureId != OGRNullFID);
  assert(_geometryByFeatureId.find(featureId) == _geometryByFeatureId.end());
  assert(_featureIdByGeometry.find(geometry) == _featureIdByGeometry.end());

  _geometryByFeatureId.insert(std::make_pair(featureId, geometry));
  _featureIdByGeometry.insert(std::make_pair(geometry, featureId));

  OGREnvelope ogrEnvelope;
  geometry->getEnvelope(&ogrEnvelope);

  Box box(
    Point(ogrEnvelope.MinX, ogrEnvelope.MinY),
    Point(ogrEnvelope.MaxX, ogrEnvelope.MaxY));
  _geometryByLocation2.insert(std::make_pair(box, featureId));
}



SpaceDimensions const& FeatureLayerGeometries::envelope() const
{
  return _envelope;
}



size_t FeatureLayerGeometries::size() const
{
  assert(_geometryByFeatureId.size() == _featureIdByGeometry.size());
  assert(_featureIdByGeometry.size() == _geometryByLocation2.size());

  return _featureIdByGeometry.size();
}



OGRGeometry const& FeatureLayerGeometries::geometry(
         FeatureId featureId) const
{
  std::map<FeatureId, OGRGeometry*>::const_iterator it =
         _geometryByFeatureId.find(featureId);

  assert(it != _geometryByFeatureId.end());

  return *((*it).second);
}



FeatureId FeatureLayerGeometries::featureId(
         OGRGeometry const* geometry) const
{
  std::map<OGRGeometry*, FeatureId>::const_iterator it =
         _featureIdByGeometry.find(const_cast<OGRGeometry*>(geometry));
  assert(it != _featureIdByGeometry.end());

  return (*it).second;
}



FeatureId FeatureLayerGeometries::featureId(
         double x,
         double y) const
{
  FeatureId result = OGRNullFID;

  OGRGeometry const* geometry = this->geometry(x, y);

  if(geometry) {
    result = featureId(geometry);
  }

  return result;
}



OGRGeometry const* FeatureLayerGeometries::geometry(
         double x,
         double y) const
{
  Point point(x, y);
  Box box(point, point);

  std::vector<Value> values;
  _geometryByLocation2.query(boost::geometry::index::intersects(box),
      std::back_inserter(values));

  OGRGeometry const* result = 0;

  if(!values.empty()) {
    OGRPoint point;
    point.setX(x);
    point.setY(y);
    OGRGeometry const* geometry;

    for(auto const& value: values) {
      geometry = _geometryByFeatureId.at(value.second);
      if(geometry->Contains(&point)) {
        result = geometry;
        break;
      }
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

