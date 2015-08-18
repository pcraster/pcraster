#include "ag_SpatialDataset.h"

// External headers.

// Project headers.
#include "dal_CoordinateMapper.h"
#include "dal_DataSpaceAddressMapper.h"
#include "dal_Table.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the SpatialDataset class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPATIALDATASET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPATIALDATASET MEMBERS
//------------------------------------------------------------------------------

SpatialDataset::SpatialDataset(
         std::string const& name,
         dal::DataSpace const& space)

  : Dataset(name, space)

{
}



SpatialDataset::~SpatialDataset()
{
}



void SpatialDataset::readTimeSeries(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address,
         dal::Table& table)
{
  dal::DataSpaceAddress localAddress(this->localAddress(space, address));
  assert(dataSpace().hasTime());
  assert(table.nrCols() == 0);
  assert(table.nrRecs() == 0);

  size_t indexOfTime = dataSpace().indexOf(dal::Time);
  dal::Dimension const& timeDimension(dataSpace().dimension(indexOfTime));

  // Create table for data values at the time steps.
  table.appendCol("values", dal::TI_REAL4);

  if(!localAddress.isValid(dataSpace().indexOf(dal::Space))) {
    table.resize(timeDimension.nrCoordinates());
    table.setAllMV();
  }
  else {
    // Convert address to data space and configure only the
    // dimension for time.
    dal::DataSpace iterSpace(dataSpace(), localAddress);
    iterSpace.dimension(indexOfTime) =timeDimension;

    // iterSpace has one value set for the scenario (it is converted
    // from the current address). Set the scenario explicitly for
    // each data source.
    if(dataSpace().hasScenarios()) {
      assert(iterSpace.hasScenarios());
      size_t index = dataSpace().indexOf(dal::Scenarios);
      dal::Dimension const& scenarioDimension(dataSpace().dimension(index));
      assert(scenarioDimension.nrValues() == 1);
      std::string scenario = scenarioDimension.value<std::string>(0);
      index = iterSpace.indexOf(dal::Scenarios);
      iterSpace.dimension(index).setValue<std::string>(scenario);
      localAddress.setCoordinate<std::string>(index, scenario);
    }

    if(!hasSelectedValue()) {
      dataSource().read(table, iterSpace, localAddress);
    }
    else {
      // assert(d_layer->typeId() == dal::TI_REAL4);
      assert(dataSource().dataSpace().hasCumProbabilities());

      localAddress.unsetCoordinate(
         dataSource().dataSpace().indexOf(dal::CumulativeProbabilities));
      localAddress.unsetCoordinate(
         dataSource().dataSpace().indexOf(dal::Time));

      dataSource().read(table, selectedValue(), /* iterSpace, */ localAddress);
    }
  }

  table.insertCol(0, "time", dal::TI_UINT4);
  dal::Array<UINT4>& timeCol(table.col<UINT4>(0));
  dal::Array<REAL4>& attrCol(table.col<REAL4>(1));
  assert(timeCol.size() <= timeDimension.nrCoordinates());

  dal::Dimension globalTimeDimension(space.dimension(space.indexOf(dal::Time)));
  dal::Array<UINT4> timeCol2(globalTimeDimension.nrCoordinates());
  dal::Array<REAL4> attrCol2(globalTimeDimension.nrCoordinates());

  size_t localTimeStep, globalTimeStep;
  dal::CoordinateMapper const* mapper(globalToLocalMapper().mapper(
         indexOfTime));

  // Convert time series in local coordinates to time series in global
  // coordinates.
  size_t nrRecs = 0;

  for(size_t i = 0, j = 0; i < timeCol.size(); ++i, ++j) {
    localTimeStep = timeDimension.coordinate<size_t>(i);
    localAddress.setCoordinate<size_t>(indexOfTime, localTimeStep);
    mapper->mapToSource(dataSpace(), localAddress, indexOfTime);
    globalTimeStep = localAddress.coordinate<size_t>(indexOfTime);

    if(i == 0) {
      timeCol2[j] = globalTimeStep;

      if(pcr::isMV(attrCol[i])) {
        pcr::setMV(attrCol2[j]);
      }
      else {
        attrCol2[j] = attrCol[i];
      }

      ++nrRecs;
    }
    else {
      size_t k = globalTimeDimension.indexOf<size_t>(timeCol2[j - 1]);
      assert(globalTimeDimension.coordinate<size_t>(k) == timeCol2[j - 1]);

      while(globalTimeDimension.coordinate<size_t>(++k) != globalTimeStep) {
        timeCol2[j] = globalTimeDimension.coordinate<size_t>(k);
        pcr::setMV(attrCol2[j]);
        ++j;
        ++nrRecs;
      }

      timeCol2[j] = globalTimeStep;

      if(pcr::isMV(attrCol[i])) {
        pcr::setMV(attrCol2[j]);
      }
      else {
        attrCol2[j] = attrCol[i];
      }

      ++nrRecs;
    }
  }

  timeCol2.resize(nrRecs);
  attrCol2.resize(nrRecs);
  dal::fillUsingPreviousValue(timeCol2);
  dal::fillUsingPreviousValue(attrCol2);

  // Copy new columns in the table.
  table.resize(nrRecs);
  table.col<UINT4>(0) = timeCol2;
  table.col<REAL4>(1) = attrCol2;
}



void SpatialDataset::readCumulativeProbabilities(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address,
         dal::Table& table)
{
  dal::DataSpaceAddress localAddress(this->localAddress(space, address));
  assert(dataSpace().hasCumProbabilities());
  assert(table.nrCols() == 0);
  assert(table.nrRecs() == 0);

  size_t indexOfCumProbabilities = dataSpace().indexOf(
         dal::CumulativeProbabilities);
  dal::Dimension const& cumProbDimension(
         dataSpace().dimension(indexOfCumProbabilities));

  // Create column for data values.
  table.appendCol("values", dal::TI_REAL4);

  if(!localAddress.isValid(dataSpace().indexOf(dal::Space))) {
    table.resize(cumProbDimension.nrCoordinates());
    table.setAllMV();
  }
  else {
    // Convert address to data space and configure only the
    // dimension for cumulative probabilities.
    dal::DataSpace iterSpace(dataSpace(), localAddress);
    iterSpace.dimension(indexOfCumProbabilities) = cumProbDimension;

    // iterSpace has one value set for the scenario (it is converted
    // from the current address). Set the scenario explicitly for
    // each data source.
    if(dataSpace().hasScenarios()) {
      assert(iterSpace.hasScenarios());
      size_t index = dataSpace().indexOf(dal::Scenarios);
      dal::Dimension const& scenarioDimension(dataSpace().dimension(index));
      assert(scenarioDimension.nrValues() == 1);
      std::string scenario = scenarioDimension.value<std::string>(0);
      index = iterSpace.indexOf(dal::Scenarios);
      iterSpace.dimension(index).setValue<std::string>(scenario);
      localAddress.setCoordinate<std::string>(index, scenario);
    }

    assert(iterSpace.contains(localAddress));
    dataSource().read(table, iterSpace, localAddress);
  }

  table.insertCol(0, "probabilities", dal::TI_REAL4);
  dal::Array<REAL4>& quantileCol(table.col<REAL4>(0));

  for(size_t i = 0; i < quantileCol.size(); ++i) {
    quantileCol[i] = cumProbDimension.coordinate<float>(i);
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

