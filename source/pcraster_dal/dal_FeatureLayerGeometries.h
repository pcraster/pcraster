#ifndef INCLUDED_DAL_FEATURELAYERGEOMETRIES
#define INCLUDED_DAL_FEATURELAYERGEOMETRIES



// External headers.

#include <boost/geometry/geometries/box.hpp>
#include <boost/geometry/geometries/point.hpp>
#include <boost/geometry/strategies/strategies.hpp>
#include <boost/geometry/index/rtree.hpp>

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_SPACEDIMENSIONS
#include "dal_SpaceDimensions.h"
#define INCLUDED_DAL_SPACEDIMENSIONS
#endif

#include <map>


class OGRGeometry;
namespace dal {
  // FeatureLayerGeometries declarations.
}



namespace dal {

using FeatureId = long int;


//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class FeatureLayerGeometries
{

  friend class FeatureLayerGeometriesTest;

public:

  using Point = boost::geometry::model::point<double, 2,
    boost::geometry::cs::cartesian>;
  using Box = boost::geometry::model::box<Point>;

private:

  using Value = std::pair<Box, FeatureId>;
  using RTree = boost::geometry::index::rtree<Value,
    boost::geometry::index::quadratic<16>>;

  //! Properties of the spatial extent occupied by the features.
  SpaceDimensions  _envelope;

  //! Geometry of all features in the layer, indexed by feature id.
  std::map<FeatureId, OGRGeometry*> _geometryByFeatureId;

  std::map<OGRGeometry*, FeatureId> _featureIdByGeometry;

  //! Spatial index of the features in the layer.
  RTree            _geometryByLocation2;

  FeatureId        featureId           (OGRGeometry const* geometry) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FeatureLayerGeometries(
                                        double west,
                                        double north,
                                        double east,
                                        double south);

                   FeatureLayerGeometries(FeatureLayerGeometries const& other) = delete;

  FeatureLayerGeometries&  operator=   (FeatureLayerGeometries const& other) = delete;

  /* virtual */    ~FeatureLayerGeometries();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             insert              (FeatureId featureId,
                                        OGRGeometry* geometry);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  SpaceDimensions const& envelope      () const;

  size_t           size                () const;

  OGRGeometry const& geometry          (FeatureId featureId) const;

  OGRGeometry const* geometry          (double x,
                                        double y) const;

  FeatureId         featureId           (double x,
                                        double y) const;

  template<class InsertIterator>
  void             featureIds          (InsertIterator inserter) const;

  template<class InsertIterator>
  void             featureIds          (Box const& box,
                                        InsertIterator inserter) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class InsertIterator>
void FeatureLayerGeometries::featureIds(
         InsertIterator inserter) const
{
  typedef std::map<OGRGeometry*, FeatureId>::value_type value_type;

  for(value_type pair : _featureIdByGeometry) {
    *inserter = pair.second;
    ++inserter;
  }
}



template<class InsertIterator>
void FeatureLayerGeometries::featureIds(
         Box const& box,
         InsertIterator inserter) const
{
  std::vector<Value> values;
  _geometryByLocation2.query(boost::geometry::index::intersects(box),
      std::back_inserter(values));

  for(auto const& value: values) {
    *inserter = value.second;
    ++inserter;
  }
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
