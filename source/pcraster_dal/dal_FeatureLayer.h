#ifndef INCLUDED_DAL_FEATURELAYER
#define INCLUDED_DAL_FEATURELAYER




// Library headers.
#ifndef INCLUDED_BOOST_ANY
#include <boost/any.hpp>
#define INCLUDED_BOOST_ANY
#endif

// PCRaster library headers.
#ifndef INCLUDED_OGR_CORE
#include <ogr_core.h>
#define INCLUDED_OGR_CORE
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASET
#include "dal_Dataset.h"
#define INCLUDED_DAL_DATASET
#endif

#ifndef INCLUDED_DAL_FEATURELAYERGEOMETRIES
#include "dal_FeatureLayerGeometries.h"
#define INCLUDED_DAL_FEATURELAYERGEOMETRIES
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif



class OGRGeometry;
namespace dal {
  // FeatureLayer declarations.
}



namespace dal {

//! Collection of features of possibly different geometry types.
/*!
  A feature layer is a collection of features. A feature is a combination of a
  geometry and a *single* attribute. So, instead of other feature layer
  definitions, this layer stores attribute values for only one attribute.

  \todo      A single layer can contain multiple attributes. See if we can
             store the geometry somewhere global and reference it from here.
             Store geometry information by name. Per name, the geometry can
             be assumed to be constant. Geometry stuff can be put in a
             separate class and put in a lookup table (map) indexed by name
             here. The destructor should decrement the reference count and
             should remove the geometry from memory if no feature layer uses
             the info anymore. Geometries?
*/
class PCR_DAL_DECL FeatureLayer: public Dataset

{

  friend class FeatureLayerTest;

private:

  FeatureLayerGeometries* _geometries;

  //! Name of the attribute.
  std::string      _name;

  //! Type id of attribute values.
  TypeId           _typeId;

  //! Table with fid and attribute values.
  dal::Table       _values;

  //! Translation table for feature id's to attribute value table index.
  std::map<long int, size_t> _valueIdByFeatureId;

  //! Minimum attribute value. If empty() then !hasExtremes().
  boost::any       _min;

  //! Maximum attribute value. If empty() then !hasExtremes().
  boost::any       _max;

  template<typename T>
  void             calculateExtremes   ();

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FeatureLayer        (FeatureLayerGeometries* geometries);

                   FeatureLayer        (FeatureLayerGeometries* geometries,
                                        std::string const& name,
                                        TypeId typeId);

                   FeatureLayer        (FeatureLayer const& rhs);

  FeatureLayer&    operator=           (FeatureLayer const& rhs);

  virtual          ~FeatureLayer       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setTypeId           (TypeId typeId);

  void             insert              (long int featureId,
                                        OGRGeometry* geometry);

  template<typename T>
  void             insert              (long int featureId,
                                        OGRGeometry* geometry,
                                        T const& value);

  template<typename T>
  void             replace             (long int featureId,
                                        T const& value);

  template<typename T>
  void             setValue            (long int featureId,
                                        T const& value);

  void             setAllMV            ();

  /// template<typename T>
  /// void             setValues           (Array<T> const& values);

  void             calculateExtremes   ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  SpaceDimensions const& dimensions    () const;

  std::string const& name              () const;

  TypeId           typeId              () const;

  size_t           nrGeometries        () const;

  template<class InsertIterator>
  void             featureIds         (InsertIterator inserter) const;

  template<class InsertIterator>
  void             featureIds          (FeatureLayerGeometries::Box const& box,
                                        InsertIterator inserter) const;

  OGRGeometry const& geometry          (long int featureId) const;

  OGRGeometry const* geometry          (double x,
                                        double y) const;

  bool             hasAttribute        () const;

  long int         featureId           (double x,
                                        double y) const;

  bool             hasValues           () const;

  bool             hasExtremes         () const;

  template<typename T>
  T                min                 () const;

  template<typename T>
  T                max                 () const;

  Table const&     values              () const;

  Table&           values              ();

  template<typename T>
  PCR_DAL_DECL void value              (double x,
                                        double y,
                                        T& result) const;

  template<typename T>
  PCR_DAL_DECL void value              (SpatialCoordinate const& address,
                                        T& result) const;

  template<typename T>
  PCR_DAL_DECL void value              (long int featureId,
                                        T& result) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class InsertIterator>
void FeatureLayer::featureIds(
         InsertIterator inserter) const
{
  _geometries->featureIds<InsertIterator>(inserter);
}



template<class InsertIterator>
void FeatureLayer::featureIds(
         FeatureLayerGeometries::Box const& box,
         InsertIterator inserter) const
{
  _geometries->featureIds<InsertIterator>(box, inserter);
}






//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
