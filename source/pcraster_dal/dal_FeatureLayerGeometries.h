#ifndef INCLUDED_DAL_FEATURELAYERGEOMETRIES
#define INCLUDED_DAL_FEATURELAYERGEOMETRIES



// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.
#ifndef INCLUDED_GEOS_INDEX_QUADTREE_QUADTREE
#include <geos/index/quadtree/Quadtree.h>
#define INCLUDED_GEOS_INDEX_QUADTREE_QUADTREE
#endif

// Module headers.
#ifndef INCLUDED_DAL_SPACEDIMENSIONS
#include "dal_SpaceDimensions.h"
#define INCLUDED_DAL_SPACEDIMENSIONS
#endif



class OGRGeometry;
namespace dal {
  // FeatureLayerGeometries declarations.
}



namespace dal {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class FeatureLayerGeometries: private boost::noncopyable
{

  friend class FeatureLayerGeometriesTest;

private:

  //! Properties of the spatial extent occupied by the features.
  SpaceDimensions  _envelope;

  //! Geometry of all features in the layer, indexed by feature id.
  std::map<long int, OGRGeometry*> _geometryByFeatureId;

  std::map<OGRGeometry*, long int> _featureIdByGeometry;

  //! Spatial index of the features in the layer.
  geos::index::quadtree::Quadtree _geometryByLocation;

  long int         featureId           (OGRGeometry const* geometry) const;

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

  /* virtual */    ~FeatureLayerGeometries();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             insert              (long int featureId,
                                        OGRGeometry* geometry);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  SpaceDimensions const& envelope      () const;

  size_t           size                () const;

  OGRGeometry const& geometry          (long int featureId) const;

  OGRGeometry const* geometry          (double x,
                                        double y) const;

  long int         featureId           (double x,
                                        double y) const;

  template<class InsertIterator>
  void             featureIds          (InsertIterator inserter) const;

  template<class InsertIterator>
  void             featureIds          (geos::geom::Envelope const& envelope,
                                        InsertIterator inserter) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class InsertIterator>
void FeatureLayerGeometries::featureIds(
         InsertIterator inserter) const
{
  typedef std::map<OGRGeometry*, long int>::value_type value_type;

  BOOST_FOREACH(value_type pair, _featureIdByGeometry) {
    *inserter = pair.second;
    ++inserter;
  }
}



template<class InsertIterator>
void FeatureLayerGeometries::featureIds(
         geos::geom::Envelope const& envelope,
         InsertIterator inserter) const
{
  std::vector<void*> items;
  const_cast<geos::index::quadtree::Quadtree&>(_geometryByLocation).query(
         &envelope, items);

  BOOST_FOREACH(void* item, items) {
    std::map<OGRGeometry*, long int>::const_iterator it =
         _featureIdByGeometry.find(static_cast<OGRGeometry*>(item));
    assert(it != _featureIdByGeometry.end());
    *inserter = (*it).second;
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
