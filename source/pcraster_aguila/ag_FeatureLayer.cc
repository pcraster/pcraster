#include "ag_FeatureLayer.h"

// External headers.

// Project headers.
#include "dal_FeatureDriver.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the FeatureLayer class.
*/



namespace ag {

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
         std::string const& name,
         dal::DataSpace const& space)

  : SpatialDataset(name, space),
    d_layer(0),
    d_valueScale(VS_NOTDETERMINED)

{
  std::auto_ptr<dal::FeatureLayer> layer(
         dataSource().open<dal::FeatureLayer>());
  assert(layer.get());

  assert(!layer->properties().hasValue(DAL_CSF_VALUESCALE));

  if(!layer->hasAttribute()) {
    // No attribute values only geometry.
    // Value scale is undefined, not relevant.
    d_valueScale = VS_UNDEFINED;
  }
  else {
    d_valueScale = dal::typeIdToValueScale(layer->typeId());

    dal::TypeId useTypeId;

    switch(d_valueScale) {
      case VS_BOOLEAN: {
        useTypeId = dal::TI_UINT1;
        break;
      }
      case VS_NOMINAL:
      case VS_ORDINAL: {
        useTypeId = dal::TI_INT4;
        break;
      }
      case VS_SCALAR: {
        useTypeId = dal::TI_REAL4;
        break;
      }
      default: {
        assert(false);
        useTypeId = dal::TI_REAL4;
        break;
      }
    }

    layer->setTypeId(useTypeId);

    dal::FeatureDriver* driver =
           dynamic_cast<dal::FeatureDriver*>(dataSource().reader());
    assert(driver);

    boost::any min, max;

    if(driver->extremes(min, max, layer->typeId(), dataSource().name(),
           dataSource().enclosingDataSpace())) {
      setExtremes(min, max);
    }

    /// if(dataSource().dataSpace().hasCumProbabilities()) {
    ///   dataSource().setMissingDataStrategy(dal::Interpolate);
    /// }
    /// else if(dataSource().dataSpace().hasTime()) {
    ///   dataSource().setMissingDataStrategy(dal::UsePrevious);
    /// }
  }

  d_layer = layer.release();

  /// FEATURE mappers don't work any more given new SpatialCoordinate stuff
  /// dal::DataSpace const& space(dataSource().dataSpace());
  /// size_t id = space.indexOf(dal::Space);

  /// if(id != space.rank()) {
  ///   // Map row indices to the coordinates of the center of the cell.
  ///   dal::Dimension dimension(space.dimension(id));
  ///   size_t first = dimension.value<size_t>(0);

  ///   localToWorldMapper().setMapper(id,
  ///          new dal::SpaceStepCoordinateMapper(first,
  ///          d_dimensions.north() - (0.5 * d_dimensions.cellSize()),
  ///          -d_dimensions.cellSize() /* , dal::SetToMissingValue */));

  ///   // Map col indices to the coordinates of the center of the cell.
  ///   ++id;
  ///   dimension = space.dimension(id);
  ///   first = dimension.value<size_t>(0);
  ///   localToWorldMapper().setMapper(id,
  ///          new dal::SpaceStepCoordinateMapper(first,
  ///          d_dimensions.west() + (0.5 * d_dimensions.cellSize()),
  ///          d_dimensions.cellSize() /* , dal::SetToMissingValue */));
  /// }

  assert(d_layer);
}



FeatureLayer::~FeatureLayer()
{
  assert(d_layer);
  delete d_layer;
}



void FeatureLayer::read(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address)
{
  assert(d_layer);

  dal::DataSpaceAddress localAddress(this->localAddress(space, address));
  assert(dataSource().dataSpace().rank() == localAddress.size());

  if(isRead(localAddress)) {
    setAddressRead(localAddress);
  }
  else {
    dal::DataSpaceAddress localAddressWithoutSpace(
         dataSource().dataSpace().eraseCoordinates(localAddress, dal::Space));

    if(!dataSource().enclosingDataSpace().contains(localAddressWithoutSpace)) {
      setAddressRead(dataSource().dataSpace().address());
      assert(!isRead());
      assert(!isRead(localAddress));
      assert(!isRead(addressRead()));
    }
    else {
      // dataSource().read(*d_layer, localAddressWithoutSpace);

      if(!hasSelectedValue()) {
        dataSource().read(*d_layer, localAddressWithoutSpace);
      }
      else if(d_layer->hasAttribute()) {
        assert(d_layer->typeId() == dal::TI_REAL4);
        assert(dataSource().dataSpace().hasCumProbabilities());

        localAddressWithoutSpace.unsetCoordinate(
              dataSource().dataSpace().indexOf(dal::CumulativeProbabilities));

        dataSource().read(*d_layer, selectedValue(), localAddressWithoutSpace);
      }

      setAddressRead(localAddress);
      assert(isRead(addressRead()));
    }
  }
}



bool FeatureLayer::isRead() const
{
  bool result = false;

  if(addressRead().size() == dataSource().dataSpace().size()) {
    dal::DataSpaceAddress addressReadWithoutSpace(
         dataSource().dataSpace().eraseCoordinates(addressRead(), dal::Space));
    dal::DataSpace const& space(dataSource().enclosingDataSpace());

    result = space.isValid(addressReadWithoutSpace);
  }

  return result;
}



bool FeatureLayer::isRead(
         dal::DataSpaceAddress const& address) const
{
  bool result = false;

  if(isRead()) {
    dal::DataSpaceAddress addressWithoutSpace(
         dataSource().dataSpace().eraseCoordinates(address, dal::Space));
    dal::DataSpaceAddress addressReadWithoutSpace(
         dataSource().dataSpace().eraseCoordinates(addressRead(), dal::Space));
    dal::DataSpace const& space(dataSource().enclosingDataSpace());

    if(space.hasScenarios()) {
      // <hack>
      // Discard scenario setting of address. Make it equal to the scenario
      // in the currently read address.
      size_t index = space.indexOf(dal::Scenarios);
      addressWithoutSpace.setCoordinate<std::string>(index,
         addressReadWithoutSpace.coordinate<std::string>(index));
      // </hack>
    }

    result = space.equal(addressReadWithoutSpace, addressWithoutSpace);
  }

  return result;
}



//! Return the value scale of the attribute values, or VS_UNDEFINED if the layer only contains geometry.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
CSF_VS FeatureLayer::valueScale() const
{
  assert(d_valueScale != VS_NOTDETERMINED);

  return d_valueScale;
}



bool FeatureLayer::isEmpty() const
{
  assert(d_layer);

  return d_layer->nrGeometries() == 0;
}



size_t FeatureLayer::size() const
{
  assert(d_layer);

  return d_layer->nrGeometries();
}



dal::SpaceDimensions const& FeatureLayer::dimensions() const
{
  return d_layer->dimensions();
}



OGRGeometry const& FeatureLayer::geometry(
         long int featureId) const
{
  return d_layer->geometry(featureId);
}



/// FeatureLayer::const_iterator FeatureLayer::begin() const
/// {
///   return d_layer->begin();
/// }
/// 
/// 
/// 
/// FeatureLayer::const_iterator FeatureLayer::end() const
/// {
///   return d_layer->end();
/// }



bool FeatureLayer::hasAttribute() const
{
  return d_layer->hasAttribute();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

