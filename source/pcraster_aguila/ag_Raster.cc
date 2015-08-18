#include "ag_Raster.h"

// Library headers.
#include <cassert>
#include <boost/format.hpp>

// PCRaster library headers.
#include "dal_RasterDriver.h"
#include "dal_Table.h"
#include "dal_Utils.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the Raster class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTER MEMBERS
//------------------------------------------------------------------------------

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
Raster::Raster(
         std::string const& name,
         dal::DataSpace const& dataSpace)

  : RasterDataset(name, dataSpace),
    d_valueScale(VS_NOTDETERMINED) // ,

{
  std::auto_ptr<dal::Raster> raster(dataSource().open<dal::Raster>());
  assert(raster.get());

  if(raster->properties().hasValue(DAL_CSF_VALUESCALE)) {
    d_valueScale = raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE);
  }
  else {
    d_valueScale = dal::typeIdToValueScale(raster->typeId());
  }

  dal::TypeId useTypeId;

  switch(d_valueScale) {
    case VS_BOOLEAN:
    case VS_LDD: {
      useTypeId = dal::TI_UINT1;
      break;
    }
    case VS_CLASSIFIED:
      // CSF version 1
      d_valueScale = VS_NOMINAL;
      // fall trough
    case VS_NOMINAL:
    case VS_ORDINAL: {
      useTypeId = dal::TI_INT4;
      break;
    }
    case VS_CONTINUOUS:
      // CSF version 1
      d_valueScale = VS_SCALAR;
      // fall through
    case VS_SCALAR:
    case VS_DIRECTION: {
      useTypeId = dal::TI_REAL4;
      break;
    }
    default: {
      assert(false);
      useTypeId = dal::TI_REAL4;
      break;
    }
  }

  d_raster = raster.release();
  d_dimensions = dal::RasterDimensions(d_raster->nrRows(), d_raster->nrCols(),
         d_raster->cellSize(), d_raster->west(), d_raster->north());

  d_raster->setTypeId(useTypeId);
  d_raster->createCells();

  boost::any min, max;
  dal::RasterDriver* driver =
         dynamic_cast<dal::RasterDriver*>(dataSource().reader());
  assert(driver);

  if(driver->extremes(min, max, d_raster->typeId(), dataSource().name(),
         dataSource().enclosingDataSpace())) {
    setExtremes(min, max);
  }

  /// if(dataSource().dataSpace().hasCumProbabilities()) {
  ///   dataSource().setMissingDataStrategy(dal::Interpolate);
  /// }
  /// else if(dataSource().dataSpace().hasTime()) {
  ///   dataSource().setMissingDataStrategy(dal::UsePrevious);
  /// }

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
}



Raster::~Raster()
{
  // delete d_dataSpaceAddressMapper;
  delete d_raster;
}



dal::RasterDimensions const& Raster::dimensions() const
{
  return d_dimensions;
}



CSF_VS Raster::valueScale() const
{
  assert(d_valueScale != VS_NOTDETERMINED);

  return d_valueScale;
}



dal::TypeId Raster::typeId() const
{
  return d_raster->typeId();
}



// void Raster::setDataSpaceAddressMapper(
//          dal::DataSpaceAddressMapper* mapper)
// {
//   delete d_dataSpaceAddressMapper;
// 
//   d_dataSpaceAddressMapper = mapper;
// }



void Raster::read(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address)
{
  assert(d_raster);

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
      if(!hasSelectedValue()) {
        dataSource().read(*d_raster, localAddressWithoutSpace);
      }
      else {
        assert(d_raster->typeId() == dal::TI_REAL4);
        assert(dataSource().dataSpace().hasCumProbabilities());

        localAddressWithoutSpace.unsetCoordinate(
              dataSource().dataSpace().indexOf(dal::CumulativeProbabilities));

        dataSource().read(*d_raster, selectedValue(), localAddressWithoutSpace);
      }

      setAddressRead(localAddress);
      assert(isRead(addressRead()));
    }
  }
}



//! Checks whether some address is read.
/*!
  \param     .
  \return    .
  \exception .
  \warning   Returns true for empty addresses (but only when the data space of
             the source is also empty).
  \sa        .
*/
bool Raster::isRead() const
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



bool Raster::isRead(
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



// //!
// /*!
//   \param     .
//   \return    .
//   \exception .
//   \warning   .
//   \sa        .
// */
// bool Raster::allMV() const
// {
//   return !hasExtremes();
// }



// bool Raster::hasExtremes() const
// {
//   return !d_min.empty() && !d_max.empty();
// }



/// dal::DataSource const& Raster::source() const
/// {
///   return d_source;
/// }



bool Raster::hasLegend() const
{
  assert(d_raster);

  return d_raster->properties().hasValue(DAL_LEGEND);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Returns a default constructed table when no legend is available.
*/
dal::Table Raster::legend() const
{
  assert(d_raster);

  dal::Table result;

  if(hasLegend()) {
    dal::Table rasterLegend = d_raster->properties().value<dal::Table>(
        DAL_LEGEND);
    assert(rasterLegend.nrCols() == 2);
    assert(rasterLegend.typeId(0) == dal::TI_INT4);
    assert(rasterLegend.typeId(1) == dal::TI_STRING);

    // The legend stored may contain a label for a subset of the values in
    // the raster. We expand the legend for those values that are not present
    // in the legend yet.

    // Determine raster value extent.
    INT4 min, max;
    assert(d_raster->typeId() == dal::TI_UINT1 ||
      d_raster->typeId() == dal::TI_INT4);
    if(d_raster->typeId() == dal::TI_UINT1) {
      min = static_cast<INT4>(d_raster->min<UINT1>());
      max = static_cast<INT4>(d_raster->max<UINT1>());
    }
    else if(typeId() == dal::TI_INT4) {
      min = d_raster->min<INT4>();
      max = d_raster->max<INT4>();
    }

    // Merge legend value extent with raster value extent to determine range
    // of values that will be part of the legend. The legend read from the
    // raster may contain values that are not present in the raster. It may
    // also lack values that are present in the raster.
    if(rasterLegend.nrRecs() > 0) {
      dal::Array<INT4> const& values(rasterLegend.col<INT4>(0));
      min = std::min(min, values[0]);
      max = std::max(max, values[rasterLegend.nrRecs() - 1]);
    }

    std::vector<dal::TypeId> typeIds;
    typeIds.push_back(dal::TI_INT4);
    typeIds.push_back(dal::TI_STRING);
    result.init(typeIds);
    result.createCols();
    result.resize((max - min) + 1);
    result.setTitle(rasterLegend.title());

    dal::Array<INT4>& values(result.col<INT4>(0));
    dal::Array<std::string>& labels(result.col<std::string>(1));

    // Fill result legend with default labels.
    INT4 value = min;
    for(size_t i = 0; i < result.nrRecs(); ++i) {
      values[i] = value++;
      labels[i] = (boost::format("%1%") % value).str();
    }

    // Overwrite the default labels with the ones read from the raster.
    std::string label;
    for(size_t i = 0; i < rasterLegend.nrRecs(); ++i) {
      value = rasterLegend.col<INT4>(0)[i];
      label = rasterLegend.col<std::string>(1)[i];
      assert(indexOf(values, value) < values.size());
      labels[indexOf(values, value)] = label;
    }
  }

  return result;
}



bool Raster::isMV(
         size_t row,
         size_t col) const
{
  bool result = true;

  switch(typeId()) {
    case dal::TI_INT1: {
      result = pcr::isMV(cell<INT1>(row, col));
      break;
    }
    case dal::TI_INT2: {
      result = pcr::isMV(cell<INT2>(row, col));
      break;
    }
    case dal::TI_INT4: {
      result = pcr::isMV(cell<INT4>(row, col));
      break;
    }
    case dal::TI_UINT1: {
      result = pcr::isMV(cell<UINT1>(row, col));
      break;
    }
    case dal::TI_UINT2: {
      result = pcr::isMV(cell<UINT2>(row, col));
      break;
    }
    case dal::TI_UINT4: {
      result = pcr::isMV(cell<UINT4>(row, col));
      break;
    }
    case dal::TI_REAL4: {
      result = pcr::isMV(cell<REAL4>(row, col));
      break;
    }
    case dal::TI_REAL8: {
      result = pcr::isMV(cell<REAL8>(row, col));
      break;
    }
    default: {
      assert(false);
      break;
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

} // namespace ag


