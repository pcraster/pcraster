#ifndef INCLUDED_DAL_FEATURELAYERGEOMETRIES
#include "dal_FeatureLayerGeometries.h"
#define INCLUDED_DAL_FEATURELAYERGEOMETRIES
#endif

// External headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_GEOS_GEOM_ENVELOPE
#include <geos/geom/Envelope.h>
#define INCLUDED_GEOS_GEOM_ENVELOPE
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
  typedef std::map<long int, OGRGeometry*>::value_type value_type;

  BOOST_FOREACH(value_type pair, _geometryByFeatureId) {
    OGRGeometryFactory::destroyGeometry(pair.second);
  }
}



void FeatureLayerGeometries::insert(
         long int featureId,
         OGRGeometry* geometry)
{
  assert(featureId != OGRNullFID);
  assert(_geometryByFeatureId.find(featureId) == _geometryByFeatureId.end());
  assert(_featureIdByGeometry.find(geometry) == _featureIdByGeometry.end());

  _geometryByFeatureId.insert(std::make_pair(featureId, geometry));
  _featureIdByGeometry.insert(std::make_pair(geometry, featureId));

  OGREnvelope ogrEnvelope;
  geometry->getEnvelope(&ogrEnvelope);

  geos::geom::Envelope envelope(ogrEnvelope.MinX, ogrEnvelope.MaxX,
         ogrEnvelope.MinY, ogrEnvelope.MaxY);

  _geometryByLocation.insert(&envelope, geometry);
}



SpaceDimensions const& FeatureLayerGeometries::envelope() const
{
  return _envelope;
}



size_t FeatureLayerGeometries::size() const
{
  assert(_geometryByFeatureId.size() == _featureIdByGeometry.size());
  assert(_featureIdByGeometry.size() == static_cast<size_t>(
         const_cast<geos::index::quadtree::Quadtree&>(
              _geometryByLocation).size()));

  return _featureIdByGeometry.size();
}



OGRGeometry const& FeatureLayerGeometries::geometry(
         long int featureId) const
{
  std::map<long int, OGRGeometry*>::const_iterator it =
         _geometryByFeatureId.find(featureId);

  assert(it != _geometryByFeatureId.end());

  return *((*it).second);
}



long int FeatureLayerGeometries::featureId(
         OGRGeometry const* geometry) const
{
  std::map<OGRGeometry*, long int>::const_iterator it =
         _featureIdByGeometry.find(const_cast<OGRGeometry*>(geometry));
  assert(it != _featureIdByGeometry.end());

  return (*it).second;
}



long int FeatureLayerGeometries::featureId(
         double x,
         double y) const
{
  long int result = OGRNullFID;

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
  geos::geom::Envelope envelope;
  envelope.init(geos::geom::Coordinate(x, y));

  std::vector<void*> items;

  const_cast<geos::index::quadtree::Quadtree&>(_geometryByLocation).query(
         &envelope, items);

  OGRGeometry const* result = 0;

  if(!items.empty()) {
    OGRPoint point;
    point.setX(x);
    point.setY(y);
    OGRGeometry const* geometry;

    BOOST_FOREACH(void* item, items) {
      geometry = static_cast<OGRGeometry const*>(item);

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

