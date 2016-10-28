#ifndef INCLUDED_DAL_FEATURELAYER
#include "dal_FeatureLayer.h"
#define INCLUDED_DAL_FEATURELAYER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

#ifndef INCLUDED_DAL_LIBRARY
#include "dal_Library.h"
#define INCLUDED_DAL_LIBRARY
#endif

#ifndef INCLUDED_DAL_TYPE
#include "dal_Type.h"
#define INCLUDED_DAL_TYPE
#endif



/*!
  \file
  This file contains the implementation of the FeatureLayer class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FEATURELAYER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FEATURELAYER MEMBERS
//------------------------------------------------------------------------------

FeatureLayer::FeatureLayer(
         FeatureLayerGeometries* geometries)

  : Dataset(FEATURE),
    _geometries(geometries),
    _name(),
    _typeId(TI_NR_TYPES)

{
  assert(_geometries);
  assert(!hasAttribute());

  Client::library().geometriesCache().incrementUseCount(_geometries);
}



FeatureLayer::FeatureLayer(
         FeatureLayerGeometries* geometries,
         std::string const& name,
         TypeId typeId)

  : Dataset(FEATURE),
    _geometries(geometries),
    _name(name),
    _typeId(typeId)

{
  assert(_geometries);
  assert(!_name.empty());
  assert(_typeId != TI_NR_TYPES);
  assert(hasAttribute());

  Client::library().geometriesCache().incrementUseCount(_geometries);

  // Configure attribute table.
  _values.appendCol(_name, _typeId);
}



FeatureLayer::FeatureLayer(
         FeatureLayer const& rhs)

  : Dataset(rhs),
    _geometries(rhs._geometries),
    _name(rhs._name),
    _typeId(rhs._typeId),
    _values(rhs._values),
    _valueIdByFeatureId(rhs._valueIdByFeatureId),
    _min(rhs._min),
    _max(rhs._max)

{
  assert(_geometries);
  Client::library().geometriesCache().incrementUseCount(_geometries);
}



FeatureLayer& FeatureLayer::operator=(
         FeatureLayer const& rhs)
{
  if(&rhs != this) {
    Dataset::operator=(rhs);
    _geometries = rhs._geometries;
    assert(_geometries);
    Client::library().geometriesCache().incrementUseCount(_geometries);
    _name = rhs._name;
    _typeId = rhs._typeId;
    _values = rhs._values;
    _valueIdByFeatureId = rhs._valueIdByFeatureId;
    _min = rhs._min;
    _max = rhs._max;
  }

  return *this;
}



//! Destructor.
/*!
  All added geometries are destructed.
*/
FeatureLayer::~FeatureLayer()
{
  Client::library().geometriesCache().decrementUseCount(_geometries);
}



/*!
  \overload
*/
void FeatureLayer::insert(
         long int featureId,
         OGRGeometry* geometry)
{
  assert(_geometries);
  _geometries->insert(featureId, geometry);
}



SpaceDimensions const& FeatureLayer::dimensions() const
{
  return _geometries->envelope();
}



std::string const& FeatureLayer::name() const
{
  return _name;
}



size_t FeatureLayer::nrGeometries() const
{
  return _geometries->size();
}



OGRGeometry const& FeatureLayer::geometry(
         long int featureId) const
{
  return _geometries->geometry(featureId);
}



TypeId FeatureLayer::typeId() const
{
  return _typeId;
}



bool FeatureLayer::hasAttribute() const
{
  return _typeId != TI_NR_TYPES;
}



bool FeatureLayer::hasValues() const
{
  assert(_values.nrRecs() == size_t(0) ||
         _values.nrRecs() == _geometries->size());

  return _values.nrRecs() > size_t(0);
}



//! Return whether the layer's extreme values are set.
/*!
  \return    true or false

  If false is returned than this could mean these things:
  - Extremes have not been calculated yet (using calculateExtremes()).
  - Feature layer does not contain non-missing-value attribute values:
    - No attribute values at all: geometry-only layer.
    - Zero attribute values.
    - Only missing values.
*/
bool FeatureLayer::hasExtremes() const
{
  assert((_min.empty() && _max.empty()) || (!_min.empty() && !_max.empty()));
  return !_min.empty();
}



void FeatureLayer::setTypeId(
         TypeId typeId)
{
  if(typeId != _typeId) {
    assert(!hasValues());
    assert(typeId != TI_NR_TYPES);

    _typeId = typeId;

    _values.erase(0);
    _values.appendCol(_name, _typeId);
  }
}



void FeatureLayer::calculateExtremes()
{
  if(hasAttribute()) {
    switch(_typeId) {
      case TI_REAL4: {
        calculateExtremes<REAL4>();
        break;
      }

      default: {
        assert(false);
        break;
      }
    }
  }
}



Table const& FeatureLayer::values() const
{
  return _values;
}



Table& FeatureLayer::values()
{
  return _values;
}



OGRGeometry const* FeatureLayer::geometry(
         double x,
         double y) const
{
  return _geometries->geometry(x, y);
}



//! Returns the id of the feature at \a x, \a y.
/*!
  \param     x X-coordinate of feature.
  \param     y Y-coordinate of feature.
  \return    Feature id or OGRNullFID if there is no such feature, or if there
             are more than one feature at \a x, \a y.
*/
long int FeatureLayer::featureId(
         double x,
         double y) const
{
  return _geometries->featureId(x, y);
}



void FeatureLayer::setAllMV()
{
  assert(_values.nrCols() == 1);

  _values.setAllMV();
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   It is assumed that the geometry lies within the envelope()
             configured in the constructor.
  \sa        .
*/
template<typename T>
void FeatureLayer::insert(
         long int featureId,
         OGRGeometry* geometry,
         T const& value)
{
  assert(featureId != OGRNullFID);

  // FEATURE optimize
  insert(featureId, geometry);
  Array<T>& col(_values.col<T>(0));
  col.push_back(value);
  assert(_valueIdByFeatureId.find(featureId) == _valueIdByFeatureId.end());
  _valueIdByFeatureId[featureId] = col.size() - 1;
}



template<typename T>
void FeatureLayer::replace(
         long int featureId,
         T const& value)
{
  assert(featureId != OGRNullFID);
  assert(_valueIdByFeatureId.find(featureId) != _valueIdByFeatureId.end());

  size_t valueId = _valueIdByFeatureId[featureId];
  Array<T>& col(_values.col<T>(0));
  col[valueId] = value;
}



template<typename T>
void FeatureLayer::setValue(
         long int featureId,
         T const& value)
{
  assert(featureId != OGRNullFID);

  Array<T>& col(_values.col<T>(0));
  col.push_back(value);
  assert(_valueIdByFeatureId.find(featureId) == _valueIdByFeatureId.end());
  _valueIdByFeatureId[featureId] = col.size() - 1;
}



template<typename T>
void FeatureLayer::value(
         double x,
         double y,
         T& result) const
{
  assert(hasAttribute());

  long int featureId = this->featureId(x, y);

  if(featureId == OGRNullFID) {
    pcr::setMV(result);
  }
  else {
    this->value(featureId, result);
  }
}



template<typename T>
void FeatureLayer::value(
         SpatialCoordinate const& address,
         T& result) const
{
  value(address.x(), address.y(), result);
}



template<typename T>
void FeatureLayer::value(
         long int featureId,
         T& result) const
{
  assert(featureId != OGRNullFID);

  std::map<long int, size_t>::const_iterator it =
         _valueIdByFeatureId.find(featureId);
  assert(it != _valueIdByFeatureId.end());
  size_t valueId = (*it).second;
  assert(valueId < _values.nrRecs());
  result = _values.cell<T>(valueId, 0);
}



template<typename T>
T FeatureLayer::min() const
{
  assert(!_min.empty());
  return boost::any_cast<T>(_min);
}



template<typename T>
T FeatureLayer::max() const
{
  assert(!_max.empty());
  return boost::any_cast<T>(_max);
}



template<typename T>
void FeatureLayer::calculateExtremes()
{
  Array<T> const& col(_values.col<T>(0));

  if(!col.empty()) {
    T min, max;
    size_t i;

    for(i = 0; i < col.size(); ++i) {
      if(!pcr::isMV(col[i])) {
        min = col[i];
        max = min;
        break;
      }
    }

    if(i < col.size()) {
      // There is at least one non-MV value in the array, so the min and max
      // are valid.

      for(++i; i < col.size(); ++i) {
        if(!pcr::isMV(col[i])) {
          min = std::min(min, col[i]);
          max = std::max(max, col[i]);
        }
      }

      _min = min;
      _max = max;
    }
  }
}



#define InstantiateTemplateMembers(type) \
template void FeatureLayer::insert<type>(long int, OGRGeometry*, type const&); \
template void FeatureLayer::replace<type>(long int, type const&); \
template void FeatureLayer::setValue<type>(long int, type const&); \
template PCR_DAL_DECL void FeatureLayer::value<type>(double, double, type&) const; \
template PCR_DAL_DECL void FeatureLayer::value<type>(SpatialCoordinate const&, type&) const; \
template PCR_DAL_DECL void FeatureLayer::value<type>(long int, type&) const; \
template type FeatureLayer::min<type>() const; \
template type FeatureLayer::max<type>() const; \
template void FeatureLayer::calculateExtremes<type>();

InstantiateTemplateMembers(UINT1)
InstantiateTemplateMembers(UINT2)
InstantiateTemplateMembers(UINT4)
InstantiateTemplateMembers(INT1)
InstantiateTemplateMembers(INT2)
InstantiateTemplateMembers(INT4)
InstantiateTemplateMembers(REAL4)
InstantiateTemplateMembers(REAL8)
InstantiateTemplateMembers(std::string)



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

