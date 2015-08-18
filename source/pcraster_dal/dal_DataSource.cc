#ifndef INCLUDED_DAL_DATASOURCE
#include "dal_DataSource.h"
#define INCLUDED_DAL_DATASOURCE
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif

#ifndef INCLUDED_DAL_DATASET
#include "dal_Dataset.h"
#define INCLUDED_DAL_DATASET
#endif

#ifndef INCLUDED_DAL_FEATUREDRIVER
#include "dal_FeatureDriver.h"
#define INCLUDED_DAL_FEATUREDRIVER
#endif

#ifndef INCLUDED_DAL_RASTERDRIVER
#include "dal_RasterDriver.h"
#define INCLUDED_DAL_RASTERDRIVER
#endif

#ifndef INCLUDED_DAL_TABLEDRIVER
#include "dal_TableDriver.h"
#define INCLUDED_DAL_TABLEDRIVER
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif

#ifndef INCLUDED_DAL_VECTORDRIVER
#include "dal_VectorDriver.h"
#define INCLUDED_DAL_VECTORDRIVER
#endif



/*!
  \file
  This file contains the implementation of the DataSource class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASOURCE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATASOURCE MEMBERS
//------------------------------------------------------------------------------

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Remove from interface in favor of one with data space argument(?).
*/
DataSource::DataSource(
         std::string const& name)

  : d_name(name),
    d_properties(0)
    /// d_missingDataStrategy(SetToMissingValue)

{
  init(name, DataSpace());
}



DataSource::DataSource(
         std::string const& name,
         DataSpace const& space)

  : d_name(name),
    d_properties(0)
    /// d_missingDataStrategy(SetToMissingValue)

{
  init(name, space);
}



DataSource::~DataSource()
{
}



void DataSource::init(
         std::string const& name,
         DataSpace const& space)
{
  assert(!d_name.empty());

  // Search for data set name in space.
  DataSpaceQueryResult result;
  boost::tie(result, d_reader) = Client::dal().search(name, space,
         SearchThisSpaceOnly, SearchForAllItems);

  if(!result) {
    // Data set name not found in space.
    throwCannotBeOpened(name, space);
  }

  assert(space.isCompatible(result.space()));
  assert(d_reader);
  d_unitSpace = d_reader->dataSpace(name, result.space(), result.address());
  d_enclosingSpace = result.space();
  d_space = d_enclosingSpace + d_unitSpace;
}



Driver* DataSource::reader()
{
  assert(d_reader);
  return d_reader;
}



std::string const& DataSource::name() const
{
  return d_name;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  For example, the unit space of a file with one raster in it is a space with
  two space dimensions in it. This space might be enclosed by an enclosing
  space with a scenario dimension for example. A file with time series in it
  has a unit space with one time dimension in it.
*/
DataSpace const& DataSource::unitDataSpace() const
{
  return d_unitSpace;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  For example, the enclosing space of a dynamic stack of rasters might contain
  a sample dimension.
*/
DataSpace const& DataSource::enclosingDataSpace() const
{
  return d_enclosingSpace;
}



//! Returns the concatenation of the enclosingDataSpace() and the unitDataSpace().
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
DataSpace const& DataSource::dataSpace() const
{
  return d_space;
}



bool DataSource::exists(
         DataSpaceAddress const& address) const
{
  return exists(dataSpace(), address);
}



bool DataSource::exists(
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(d_reader);

  return d_reader->exists(d_name, space, address);
}



//! Returns a raster.
/*!
  \return    Raster.
  \exception Exception If the raster dataset with the layered name cannot be
             read.
  \warning   The datasource must contain a raster. This function assumes that
             the driver used for reading the dataset is a RasterDriver. The
             dataset must not be dynamic, use raster(size_t) if it is.
  \sa        raster(size_t)
*/
Raster* DataSource::raster() const
{
  assert(dataSpace().hasRaster());
  assert(!dataSpace().hasTime());
  assert(d_reader);

  RasterDriver* driver = dynamic_cast<RasterDriver*>(d_reader);
  assert(driver);

  Raster* raster = driver->read(d_name);
  assert(raster);

  // TODO If needed fill the cache memory data pool using the memory raster
  // TODO driver.

  return raster;
}



// //!
// /*!
//   \param     .
//   \return    .
//   \exception .
//   \warning   .
//   \sa        .
//   \todo      Handle situation where space has a cum_probability dimension and
//              address has no valid coordinate at that point: find median and
//              use that.
// */
// Raster* DataSource::raster(
//          DataSpaceAddress const& address) const
// {
//   return raster(dataSpace(), address);
// }



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   \a address must not contain coordinates for the space dimensions.
  \sa        .
*/
Raster* DataSource::raster(
         // DataSpace const& space,
         DataSpaceAddress address,
         TypeId typeId) const
{
  assert(d_reader);

  DataSpace space(dataSpace());

  // Erase irrelevant stuff. Otherwise the raster drivers will complain.
  space.eraseDimension(Space);

  assert(space.contains(address));

  RasterDriver* driver = dynamic_cast<RasterDriver*>(d_reader);
  assert(driver);

  // Raster* raster = driver->read(d_name, enclosingDataSpace(),
  //        enclosingDataSpace().trim(space, address), typeId);
  Raster* raster = driver->read(d_name, space, address, typeId);
  assert(raster);

  // TODO If needed fill the cache memory data pool using the memory raster
  // TODO driver.

  return raster;
}



DataSpaceAddress DataSource::findExistingAddress(
         DataSpace const& space,
         DataSpaceAddress const& address,
         MissingDataStrategy strategy) const
{
  DataSpaceAddress result(space.address());

  if(exists(space, address)) {
    result = address;
  }
  else {
    if(strategy == UsePrevious) {
      result = findPreviousExistingAddress(space, address);
    }
    else if(strategy == UseNext) {
      result = findNextExistingAddress(space, address);
    }
    else {
      assert(false);
    }
  }

  return result;
}



//! Returns the previous / lower address of an existing unit of data.
/*!
  \param     space Data space to search in.
  \param     address Start address.
  \return    Lower address.

  The returned address will be invalid if there is no lower address for
  an existing unit of data.
*/
DataSpaceAddress DataSource::findPreviousExistingAddress(
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(space.contains(address));

  DataSpaceAddress result(space.address());
  DataSpaceIterator it(DataSpaceIterator(space, address));

  if(it != space.begin()) {
    --it;
    assert(it != space.rend());

    do {
      if(exists(space, *it)) {
        result = *it;
        break;
      }

      --it;
    } while(it != space.rend());
  }

  assert(!result.isValid() || !space.equal(result, address));
  return result;
}



DataSpaceAddress DataSource::findNextExistingAddress(
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  DataSpaceAddress result(space.address());
  DataSpaceIterator it(DataSpaceIterator(space, address));

  if(it != space.rbegin()) {
    ++it;
    assert(it != space.end());

    do {
      if(exists(space, *it)) {
        result = *it;
        break;
      }

      ++it;
    } while(it != space.end());
  }

  assert(!result.isValid() || !space.equal(result, address));
  return result;
}



//! Fills \a raster with values from a specific \a address.
/*!
  \param     .
  \return    .
  \exception .
  \warning   \a address must not contain coordinates for the space dimensions.
  \warning   This function assumes that the data set contains rasters.
*/
void DataSource::read(
         Raster& raster,
         DataSpaceAddress address) const
{
  assert(d_reader);

  // Copy space and erase irrelevant stuff. Otherwise the raster drivers will
  // complain.
  DataSpace space(dataSpace());
  space.eraseDimension(Space);
  assert(space.isValid(address));

  RasterDriver* driver = dynamic_cast<RasterDriver*>(d_reader);
  assert(driver);

  if(!space.contains(address)) {
    raster.setAllMV();
  }
  else if(exists(space, address)) {
    // At the requested address in the data space data is found.
    // (Data space and address may be empty.)
    driver->read(raster, d_name, space, address);
  }
  else if(space.rank() == 0) {
    // Data space is empty.
    raster.setAllMV();
  }
  else {
    // The data space contains coordinates and at the current address there
    // is no raster to be found.
    size_t indexOfWideDimension = space.rank();
    DataSpaceAddress lower(space.address());
    DataSpaceAddress upper(space.address());
    DataSpace iterSpace(space, address);
    MissingDataStrategy missingDataStrategy = SetToMissingValue;

    if(space.hasTime() && space.hasCumProbabilities()) {
      // Find time step to use using UsePrevious strategy. Just iterate to
      // a previous available data set and use that address to find a
      // timestep for which data is available. Configure the iterSpace
      // object with that timestep.
      size_t indexOfTime = iterSpace.indexOf(Time);
      size_t indexOfProbabilities = iterSpace.indexOf(CumulativeProbabilities);

      iterSpace.dimension(indexOfTime) = space.dimension(indexOfTime);
      iterSpace.dimension(indexOfProbabilities) =
         space.dimension(indexOfProbabilities);

      lower = findPreviousExistingAddress(iterSpace, address);

      size_t timeStep = address.coordinate<size_t>(indexOfTime);

      if(lower.isValid()) {
        // Found a valid address for which data is available. Get the time
        // step of this address.
        timeStep = lower.coordinate<size_t>(indexOfTime);
        lower = DataSpaceAddress(space.address());
        address.setCoordinate<size_t>(indexOfTime, timeStep);

        if(exists(space, address)) {
          driver->read(raster, d_name, space, address);
          return;
        }
      }

      assert(timeStep <= address.coordinate<size_t>(indexOfTime));

      // From now on the iterSpace will only contain the probability
      // dimension to iterate over.
      iterSpace.dimension(indexOfTime).setValues<size_t>(timeStep, timeStep,
         iterSpace.dimension(indexOfTime).value<size_t>(2));

      indexOfWideDimension = indexOfProbabilities;
      missingDataStrategy = Interpolate;
    }
    else if(space.hasTime()) {
      size_t indexOfTime = space.indexOf(Time);
      iterSpace.dimension(indexOfTime) = space.dimension(indexOfTime);
      indexOfWideDimension = indexOfTime;
      missingDataStrategy = UsePrevious;
    }
    else if(space.hasCumProbabilities()) {
      size_t indexOfProbabilities = space.indexOf(CumulativeProbabilities);
      iterSpace.dimension(indexOfProbabilities) = space.dimension(
         indexOfProbabilities);
      indexOfWideDimension = indexOfProbabilities;
      missingDataStrategy = Interpolate;
    }

    assert(!lower.isValid() && !upper.isValid());
    assert(indexOfWideDimension < space.rank());
    Dimension const& wideDimension(space.dimension(indexOfWideDimension));

    if(missingDataStrategy == Interpolate) {
      assert(raster.typeId() == TI_REAL4);
      lower = findPreviousExistingAddress(iterSpace, address);
      upper = findNextExistingAddress(iterSpace, address);

      if(lower.isValid() && upper.isValid()) {
        assert(exists(iterSpace, lower));
        assert(exists(iterSpace, upper));
        assert(!iterSpace.equal(lower, upper));

        // if(space.equal(lower, upper)) {
        //   // Will throw the exception.
        //   driver->read(raster, d_name, space, address);
        // }
        // else {
          // Interpolate lower and higher data sets.
          assert(lower.size() == upper.size());
          assert(lower.size() <= 3); // Huh?

          // assert(indexOfWideDimension != space.rank());
          assert(lower.isValid(indexOfWideDimension));
          assert(upper.isValid(indexOfWideDimension));
          assert(iterSpace.isValid(lower));
          assert(iterSpace.isValid(upper));

          // Convert coordinates to double.
          assert(wideDimension.meaning() == Time ||
                 wideDimension.meaning() == CumulativeProbabilities);
          double lowerDistance = 0.0;
          double upperDistance = 0.0;

          switch(wideDimension.meaning()) {
            case Time: {
              lowerDistance =
                 address.coordinate<size_t>(indexOfWideDimension) -
                 lower.coordinate<size_t>(indexOfWideDimension);
              upperDistance =
                 upper.coordinate<size_t>(indexOfWideDimension) -
                 address.coordinate<size_t>(indexOfWideDimension);
              break;
            }
            case CumulativeProbabilities: {
              lowerDistance =
                 address.coordinate<float>(indexOfWideDimension) -
                 lower.coordinate<float>(indexOfWideDimension);
              upperDistance =
                 upper.coordinate<float>(indexOfWideDimension) -
                 address.coordinate<float>(indexOfWideDimension);
              break;
            }
            default: {
              assert(false);
              break;
            }
          }

          driver->read(raster, d_name, iterSpace, lower);
          Raster lowerRaster(raster);
          driver->read(raster, d_name, iterSpace, upper);
          Raster upperRaster(raster);
          interpolate<REAL4>(raster.cells<REAL4>(), raster.nrCells(),
                 lowerRaster.cells<REAL4>(), lowerDistance,
                 upperRaster.cells<REAL4>(), upperDistance);
      }
      // else if(lower.isValid()) {
      //   // Clamp to lower.
      //   driver->read(raster, d_name, iterSpace, lower);
      // }
      // else if(upper.isValid()) {
      //   // Clamp to upper.
      //   driver->read(raster, d_name, iterSpace, upper);
      // }
      else {
        // A raster at the requested address is missing and cannot be
        // interpolated.
        raster.setAllMV();
      }
    }
    else if(missingDataStrategy == UsePrevious) {
      lower = findPreviousExistingAddress(iterSpace, address);

      if(!lower.isValid()) {
        raster.setAllMV();
        // // Will throw the exception.
        // driver->read(raster, d_name, iterSpace, address);
      }
      else {
        driver->read(raster, d_name, iterSpace, lower);
      }
    }
    else if(missingDataStrategy == UseNext) {
      upper = findNextExistingAddress(iterSpace, address);

      if(!upper.isValid()) {
        raster.setAllMV();
        // // Will throw the exception.
        // driver->read(raster, d_name, iterSpace, address);
      }
      else {
        driver->read(raster, d_name, iterSpace, upper);
      }
    }
    else {
      assert(false);
    }
  }
}



void DataSource::read(
         Raster& raster,
         boost::any const& value,
         DataSpaceAddress const& address) const
{
  assert(raster.typeId() == TI_REAL4);
  assert(value.type() == typeid(REAL4));
  read(raster, boost::any_cast<REAL4>(value), address);
}



void DataSource::read(
         FeatureLayer& layer,
         boost::any const& value,
         DataSpaceAddress const& address) const
{
  assert(layer.typeId() == TI_REAL4);
  assert(value.type() == typeid(REAL4));
  read(layer, boost::any_cast<REAL4>(value), address);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  \todo      Compare with read(Raster&, DataSpaceAddress). Refactor!
*/
template<typename T>
void DataSource::read(
         Raster& raster,
         T const& value,
         DataSpaceAddress address) const
{
  assert(dataSpace().hasRaster());
  assert(dataSpace().hasCumProbabilities());
  assert(!pcr::isMV(value));

  DataSpace space(dataSpace());

  // Erase irrelevant Space dimensions from data space.
  space.eraseDimension(Space);

  assert(space.size() == address.size());
  size_t indexOfProbabilities = space.indexOf(CumulativeProbabilities);
  assert(!address.isValid(indexOfProbabilities));

  Array<T> lowerValues(raster.nrCells());
  Array<T> upperValues(raster.nrCells());
  Array<double> lowerDistances(raster.nrCells());
  Array<double> upperDistances(raster.nrCells());
  lowerValues.setAllMV();
  upperValues.setAllMV();

  assert(d_reader);
  RasterDriver* driver = dynamic_cast<RasterDriver*>(d_reader);
  assert(driver);

  DataSpace iterSpace(space, address);
  iterSpace.dimension(indexOfProbabilities) = space.dimension(
         indexOfProbabilities);

  size_t indexOfTime = space.indexOf(Time);

  if(iterSpace.hasTime()) {
    // Find and set time step to use.
    Dimension& dimension(iterSpace.dimension(indexOfTime));

    dimension = space.dimension(indexOfTime);
    address.setCoordinate(indexOfProbabilities,
         space.dimension(indexOfProbabilities).template value<float>(1));

    DataSpaceAddress lower = iterSpace.contains(address)
         ? findPreviousExistingAddress(iterSpace, address)
         : iterSpace.address();

    if(!lower.isValid()) {
      // dimension.clear();
      address.unsetCoordinate(indexOfTime);
    }
    else {
      size_t timeStep = lower.coordinate<size_t>(indexOfTime);
      dimension.setValues<size_t>(timeStep, timeStep, 1);
      address.setCoordinate(indexOfTime, timeStep);
    }
  }

  if(!iterSpace.hasTime() || (
         iterSpace.hasTime() && address.isValid(indexOfTime))) {
    float quantile;

    // Loop over all quantiles.
    for(DataSpaceIterator it = iterSpace.begin(); it != iterSpace.end(); ++it) {
      if(exists(iterSpace, *it)) {
        // A dataset for current quantile exists.
        // Read the raster.
        quantile = (*it).template coordinate<float>(indexOfProbabilities);
        driver->read(raster, d_name, iterSpace, *it);
        T const* cells(raster.template cells<T>());

        // Loop over all cells in raster.
        for(size_t i = 0; i < raster.nrCells(); ++i) {
          if(!pcr::isMV(cells[i])) {
            if(pcr::isMV(upperValues[i])) {
              // In case quantile never becomes larger than value the result
              // should be 1.0.
              // The values in lower* don't matter anymore. The result
              // will be 1.0.
              upperValues[i] = T(1.0);
              upperDistances[i] = 0.0;
            }

            if(cells[i] < value) {
              // cells[i] is increasing and still smaller than value.
              lowerValues[i] = quantile;
              lowerDistances[i] = std::abs(value - cells[i]);
              assert(lowerDistances[i] > 0.0);
              // assert(greaterOrComparable(lowerDistances[i], 0.0));

            }
            else if(comparable(cells[i], value)) {
              lowerValues[i] = quantile;
              upperValues[i] = quantile;
              lowerDistances[i] = 0.0;
              upperDistances[i] = 0.0;
            }
            else if(dal::comparable(upperValues[i], T(1.0f))) {
              // This test assumes that upperValues[i] has been set to the
              // default value of 1.0 above.
              // For the first time cells[i] has increased past value. Replace
              // existing value with this new one. This one should not be
              // overwritten anymore.
              upperValues[i] = quantile;
              upperDistances[i] = cells[i] - value;
              assert(upperDistances[i] > 0.0);

              if(pcr::isMV(lowerValues[i])) {
                // The quantile has never been lower than value. Set it
                // to zero.
                // The values in upper* don't matter anymore. The result
                // will be 0.0.
                lowerValues[i] = T(0.0);
                lowerDistances[i] = 0.0;
              }
            }
          }
        }
      }
    }
  }

  T* cells(raster.template cells<T>());

  for(size_t i = 0; i < raster.nrCells(); ++i) {
    if(pcr::isMV(lowerValues[i]) || pcr::isMV(upperValues[i])) {
      pcr::setMV(cells[i]);
    }
    else {
      assert(greaterOrComparable(lowerValues[i], T(0.0)));
      assert(smallerOrComparable(upperValues[i], T(1.0)));

      if(comparable(lowerValues[i], upperValues[i])) {
        cells[i] = lowerValues[i];
      }
      else {
        interpolate<T>(cells[i], lowerValues[i], lowerDistances[i],
              upperValues[i], upperDistances[i]);
      }

      assert(greaterOrComparable(cells[i], T(0.0)));
      assert(smallerOrComparable(cells[i], T(1.0)));
    }
  }
}



template
PCR_DAL_DECL void DataSource::read<REAL4>(
         Raster& raster,
         REAL4 const& value,
         DataSpaceAddress address) const;



template<typename T>
void DataSource::read(
         FeatureLayer& layer,
         T const& value,
         DataSpaceAddress address) const
{
  assert(dataSpace().hasFeatures());
  assert(dataSpace().hasCumProbabilities());
  assert(!pcr::isMV(value));

  DataSpace space(dataSpace());

  // Erase irrelevant Space dimensions from data space.
  space.eraseDimension(Space);

  assert(space.size() == address.size());
  size_t indexOfProbabilities = space.indexOf(CumulativeProbabilities);
  assert(!address.isValid(indexOfProbabilities));

  Array<T> lowerValues(layer.nrGeometries());
  Array<T> upperValues(layer.nrGeometries());
  Array<double> lowerDistances(layer.nrGeometries());
  Array<double> upperDistances(layer.nrGeometries());
  lowerValues.setAllMV();
  upperValues.setAllMV();

  assert(d_reader);
  FeatureDriver* driver = dynamic_cast<FeatureDriver*>(d_reader);
  assert(driver);

  DataSpace iterSpace(space, address);
  iterSpace.dimension(indexOfProbabilities) = space.dimension(
         indexOfProbabilities);

  size_t indexOfTime = space.indexOf(Time);

  if(iterSpace.hasTime()) {
    // Find and set time step to use.
    Dimension& dimension(iterSpace.dimension(indexOfTime));

    dimension = space.dimension(indexOfTime);
    address.setCoordinate(indexOfProbabilities,
         space.dimension(indexOfProbabilities).template value<float>(1));

    DataSpaceAddress lower = iterSpace.contains(address)
         ? findPreviousExistingAddress(iterSpace, address)
         : iterSpace.address();

    if(!lower.isValid()) {
      // dimension.clear();
      address.unsetCoordinate(indexOfTime);
    }
    else {
      size_t timeStep = lower.coordinate<size_t>(indexOfTime);
      dimension.setValues<size_t>(timeStep, timeStep, 1);
      address.setCoordinate(indexOfTime, timeStep);
    }
  }

  if(!iterSpace.hasTime() || (
         iterSpace.hasTime() && address.isValid(indexOfTime))) {
    float quantile;

    // Loop over all quantiles.
    for(DataSpaceIterator it = iterSpace.begin(); it != iterSpace.end(); ++it) {
      if(exists(iterSpace, *it)) {
        // A dataset for current quantile exists.
        // Read the data.
        quantile = (*it).template coordinate<float>(indexOfProbabilities);
        driver->read(layer, d_name, iterSpace, *it);
        T const* cells(layer.values().template col<T>(0).elements());

        // Loop over all cells in raster.
        for(size_t i = 0; i < layer.nrGeometries(); ++i) {
          if(!pcr::isMV(cells[i])) {
            if(pcr::isMV(upperValues[i])) {
              // In case quantile never becomes larger than value the result
              // should be 1.0.
              // The values in lower* don't matter anymore. The result
              // will be 1.0.
              upperValues[i] = T(1.0);
              upperDistances[i] = 0.0;
            }

            if(cells[i] < value) {
              // cells[i] is increasing and still smaller than value.
              lowerValues[i] = quantile;
              lowerDistances[i] = std::abs(value - cells[i]);
              assert(lowerDistances[i] > 0.0);
              // assert(greaterOrComparable(lowerDistances[i], 0.0));

            }
            else if(comparable(cells[i], value)) {
              lowerValues[i] = quantile;
              upperValues[i] = quantile;
              lowerDistances[i] = 0.0;
              upperDistances[i] = 0.0;
            }
            else if(dal::comparable(upperValues[i], T(1.0f))) {
              // This test assumes that upperValues[i] has been set to the
              // default value of 1.0 above.
              // For the first time cells[i] has increased past value. Replace
              // existing value with this new one. This one should not be
              // overwritten anymore.
              upperValues[i] = quantile;
              upperDistances[i] = cells[i] - value;
              assert(upperDistances[i] > 0.0);

              if(pcr::isMV(lowerValues[i])) {
                // The quantile has never been lower than value. Set it
                // to zero.
                // The values in upper* don't matter anymore. The result
                // will be 0.0.
                lowerValues[i] = T(0.0);
                lowerDistances[i] = 0.0;
              }
            }
          }
        }
      }
    }
  }

  T* cells(layer.values().template col<T>(0).elements());

  for(size_t i = 0; i < layer.nrGeometries(); ++i) {
    if(pcr::isMV(lowerValues[i]) || pcr::isMV(upperValues[i])) {
      pcr::setMV(cells[i]);
    }
    else {
      assert(greaterOrComparable(lowerValues[i], T(0.0)));
      assert(smallerOrComparable(upperValues[i], T(1.0)));

      if(comparable(lowerValues[i], upperValues[i])) {
        cells[i] = lowerValues[i];
      }
      else {
        interpolate<T>(cells[i], lowerValues[i], lowerDistances[i],
              upperValues[i], upperDistances[i]);
      }

      assert(greaterOrComparable(cells[i], T(0.0)));
      assert(smallerOrComparable(cells[i], T(1.0)));
    }
  }
}

template
PCR_DAL_DECL void DataSource::read<REAL4>(
         FeatureLayer& layer,
         REAL4 const& value,
         DataSpaceAddress address) const;



// void DataSource::read(
//          TableDriver const& driver,
//          Table& table,
//          DataSpaceAddress address) const
// {
//   DataSpace iterSpace(dataSpace());
//   DataSpaceAddress iterAddress(address);
// 
//   iterSpace.eraseDimension(Time);
// 
//   driver.read(table, d_name, iterSpace, iterAddress);
// }



void DataSource::read(
         FeatureLayer& layer,
         DataSpaceAddress address) const
{
  // FEATURE merge with read(raster, address)
  assert(d_reader);
  assert(enclosingDataSpace().isValid(address));

  FeatureDriver* driver = dynamic_cast<FeatureDriver*>(d_reader);
  assert(driver);

  DataSpace space(enclosingDataSpace());

  if(!space.contains(address)) {
    layer.setAllMV();
  }
  else if(exists(space, address)) {
    // At the requested address in the data space data is found.
    // (Data space and address may be empty.)
    driver->read(layer, d_name, space, address);
  }
  else if(space.rank() == 0) {
    // Data space is empty.
    layer.setAllMV();
  }
  else {
    // The data space contains coordinates and at the current address there
    // is no feature layer to be found.
    size_t indexOfWideDimension = space.rank();
    DataSpaceAddress lower(space.address());
    DataSpaceAddress upper(space.address());
    DataSpace iterSpace(space, address);
    MissingDataStrategy missingDataStrategy = SetToMissingValue;

    if(space.hasTime() && space.hasCumProbabilities()) {
      // Find time step to use using UsePrevious strategy. Just iterate to
      // a previous available data set and use that address to find a
      // timestep for which data is available. Configure the iterSpace
      // object with that timestep.
      size_t indexOfTime = iterSpace.indexOf(Time);
      size_t indexOfProbabilities = iterSpace.indexOf(CumulativeProbabilities);
      iterSpace.dimension(indexOfTime) = space.dimension(indexOfTime);
      iterSpace.dimension(indexOfProbabilities) = space.dimension(
         indexOfProbabilities);
      lower = findPreviousExistingAddress(iterSpace, address);

      size_t timeStep = address.coordinate<size_t>(indexOfTime);

      if(lower.isValid()) {
        // Found a valid address for which data is available. Get the time
        // step of this address.
        timeStep = lower.coordinate<size_t>(indexOfTime);
        lower = DataSpaceAddress(space.address());
        address.setCoordinate<size_t>(indexOfTime, timeStep);

        if(exists(space, address)) {
          driver->read(layer, d_name, space, address);
          return;
        }
      }

      assert(timeStep <= address.coordinate<size_t>(indexOfTime));

      // From now on the iterSpace will only contain the probability
      // dimension to iterate over.
      iterSpace.dimension(indexOfTime).setValues<size_t>(
         timeStep, timeStep, iterSpace.dimension(indexOfTime).value<size_t>(2));

      indexOfWideDimension = indexOfProbabilities;
      missingDataStrategy = Interpolate;
    }
    else if(space.hasTime()) {
      size_t indexOfTime = space.indexOf(Time);
      iterSpace.dimension(indexOfTime) = space.dimension(indexOfTime);
      indexOfWideDimension = indexOfTime;
      missingDataStrategy = UsePrevious;
    }
    else if(space.hasCumProbabilities()) {
      size_t indexOfProbabilities = space.indexOf(CumulativeProbabilities);
      iterSpace.dimension(indexOfProbabilities) = space.dimension(
         indexOfProbabilities);
      indexOfWideDimension = indexOfProbabilities;
      missingDataStrategy = Interpolate;
    }

    assert(!lower.isValid() && !upper.isValid());
    assert(indexOfWideDimension < space.rank());
    Dimension const& wideDimension(space.dimension(indexOfWideDimension));

    if(missingDataStrategy == Interpolate) {
      assert(layer.typeId() == TI_REAL4);
      lower = findPreviousExistingAddress(iterSpace, address);
      upper = findNextExistingAddress(iterSpace, address);

      if(lower.isValid() && upper.isValid()) {
        assert(exists(iterSpace, lower));
        assert(exists(iterSpace, upper));
        assert(!iterSpace.equal(lower, upper));

        // Interpolate lower and higher data sets.
        assert(lower.size() == upper.size());
        assert(lower.size() <= 3); // Huh?

        // assert(indexOfWideDimension != space.rank());
        assert(lower.isValid(indexOfWideDimension));
        assert(upper.isValid(indexOfWideDimension));
        assert(iterSpace.isValid(lower));
        assert(iterSpace.isValid(upper));

        // Convert coordinates to double.
        assert(wideDimension.meaning() == Time ||
               wideDimension.meaning() == CumulativeProbabilities);
        double lowerDistance = 0.0;
        double upperDistance = 0.0;

        switch(wideDimension.meaning()) {
          case Time: {
            lowerDistance =
               address.coordinate<size_t>(indexOfWideDimension) -
               lower.coordinate<size_t>(indexOfWideDimension);
            upperDistance =
               upper.coordinate<size_t>(indexOfWideDimension) -
               address.coordinate<size_t>(indexOfWideDimension);
            break;
          }
          case CumulativeProbabilities: {
            lowerDistance =
               address.coordinate<float>(indexOfWideDimension) -
               lower.coordinate<float>(indexOfWideDimension);
            upperDistance =
               upper.coordinate<float>(indexOfWideDimension) -
               address.coordinate<float>(indexOfWideDimension);
            break;
          }
          default: {
            assert(false);
            break;
          }
        }

        driver->read(layer, d_name, iterSpace, lower);
        FeatureLayer lowerLayer(layer);
        driver->read(layer, d_name, iterSpace, upper);
        FeatureLayer upperLayer(layer);
        interpolate<REAL4>(layer.values().col<REAL4>(0),
              lowerLayer.values().col<REAL4>(0), lowerDistance,
              upperLayer.values().col<REAL4>(0), upperDistance);
      }
      else {
        // Data at the requested address is missing and cannot be
        // interpolated.
        layer.setAllMV();
      }
    }
    else if(missingDataStrategy == UsePrevious) {
      lower = findPreviousExistingAddress(iterSpace, address);

      if(!lower.isValid()) {
        layer.setAllMV();
      }
      else {
        driver->read(layer, d_name, iterSpace, lower);
      }
    }
    else if(missingDataStrategy == UseNext) {
      upper = findNextExistingAddress(iterSpace, address);

      if(!upper.isValid()) {
        layer.setAllMV();
      }
      else {
        driver->read(layer, d_name, iterSpace, upper);
      }
    }
    else {
      assert(false);
    }
  }
}



void DataSource::read(
         Table& table,
         DataSpaceAddress const& address) const
{
  assert(dataSpace().contains(address));
  assert(d_reader);

  TableDriver* driver = dynamic_cast<TableDriver*>(d_reader);
  assert(driver);

  DataSpace iterSpace(dataSpace());
  DataSpaceAddress iterAddress(address);

  iterAddress = iterSpace.eraseCoordinates(iterAddress, Time);
  iterSpace.eraseDimension(Time);

  driver->read(table, d_name, iterSpace, iterAddress);

  // TODO If needed fill the cache memory data pool using the memory raster
  // TODO driver.

  // TableDriver* tableDriver = dynamic_cast<TableDriver*>(d_reader);
  // RasterDriver* rasterDriver = dynamic_cast<RasterDriver*>(d_reader);
  // assert(tableDriver || rasterDriver);

  // if(tableDriver) {
  //   read(*tableDriver, table, address);
  // }
  // else if(rasterDriver) {
  //   read(*rasterDriver, table, address);
  // }
}



void DataSource::read(
         Table& table,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(d_reader);
  assert(space.isCompatible(dataSpace()));
  // assert(space.contains(address));

  // RasterDriver* driver = dynamic_cast<RasterDriver*>(d_reader);
  Driver* driver = d_reader;
  assert(driver);

  assert(space.nrWideDimensions() > 0);
  size_t indexOfWideDimension = space.indexOfWideDimension();
  assert(indexOfWideDimension < space.size());
  Dimension const& wideDimension(space.dimension(indexOfWideDimension));
  DataSpace iterSpace(space);

  table.resize(iterSpace.dimension(indexOfWideDimension).nrCoordinates());

  if(iterSpace.hasTime() && iterSpace.hasCumProbabilities()) {
    // Determine time step with data.
    // Find time step to use using UsePrevious strategy. Just iterate to
    // a previous available data set and use that address to find a
    // timestep for which data is available.
    size_t indexOfSpace = iterSpace.indexOf(Space);
    size_t indexOfTime = iterSpace.indexOf(Time);
    size_t indexOfProbabilities = iterSpace.indexOf(CumulativeProbabilities);

    iterSpace.dimension(indexOfTime) = dataSpace().dimension(indexOfTime);
    iterSpace.dimension(indexOfProbabilities) = dataSpace().dimension(
         indexOfProbabilities);

    DataSpaceAddress iterAddress(address);
    iterAddress = iterSpace.eraseCoordinates(iterAddress, Space);
    iterSpace.eraseDimension(Space);

    assert(wideDimension.discretisation() == RegularDiscretisation);
    assert(wideDimension.meaning() == Time ||
         wideDimension.meaning() == CumulativeProbabilities);

    switch(wideDimension.meaning()) {
      case Time: {
        iterAddress.setCoordinate<size_t>(indexOfWideDimension,
              wideDimension.value<size_t>(1));
        break;
      }
      case CumulativeProbabilities: {
        iterAddress.setCoordinate<float>(indexOfWideDimension,
              wideDimension.value<float>(1));
        break;
      }
      default: {
        assert(false);
        break;
      }
    }

    DataSpaceAddress lower, upper;

    lower = iterSpace.contains(iterAddress)
         ? findExistingAddress(iterSpace, iterAddress, UsePrevious)
         : iterSpace.address();

    if(!lower.isValid()) {
      // No data at current or previous address.
      table.setAllMV();
    }
    else {
      // Found a valid address for which data is available.
      // Now we know which time step contains quantiles.
      size_t timeStep = lower.coordinate<size_t>(indexOfTime);

      iterSpace.dimension(indexOfTime).setValues<size_t>(
         timeStep, timeStep, iterSpace.dimension(indexOfTime).value<size_t>(2));
      iterAddress.setCoordinate<size_t>(indexOfTime, timeStep);

      if(wideDimension.meaning() == Time) {
        // Determine quantiles with data.
        // Find lower quantile and upper quantile.
        lower = findExistingAddress(iterSpace, iterAddress, UsePrevious);
        upper = findExistingAddress(iterSpace, iterAddress, UseNext);
        if(!(lower.isValid() && upper.isValid())) {
          table.setAllMV();
        }
        else {
          // Make time dimension wide again.
          iterSpace.dimension(indexOfTime) = dataSpace().dimension(indexOfTime);

          // Either lower AND upper are valid or one of them is. If one of them
          // is OR both of them are equal, than it/they should point to the
          // iterAddress and no interpolation needs to be done.
          // If both are valid

          // Determine time series of quantiles for time step found.
          if((lower.isValid() && !upper.isValid()) ||
             (!lower.isValid() &&  upper.isValid()) ||
             iterSpace.equal(lower, upper)) {

            iterSpace.addDimension(space.dimension(indexOfSpace));
            /// iterSpace.addDimension(space.dimension(indexOfSpace + 1));

            float quantile;

            if(lower.isValid()) {
              quantile = lower.coordinate<float>(indexOfProbabilities);
            }
            else {
              quantile = upper.coordinate<float>(indexOfProbabilities);
            }

            iterSpace.dimension(indexOfProbabilities).setValues<float>(
                   quantile, quantile,
                   iterSpace.dimension(indexOfProbabilities).value<float>(2));
            driver->read(table, d_name, iterSpace);
            fillUsingPreviousValue(table.col<REAL4>(0));
          }
          else {
            assert(!iterSpace.equal(lower, iterAddress));
            assert(!iterSpace.equal(upper, iterAddress));

            iterSpace.addDimension(space.dimension(indexOfSpace));
            /// iterSpace.addDimension(space.dimension(indexOfSpace + 1));

            // Read data for both addresses and interpolate for result.
            float lowerQuantile = lower.coordinate<float>(indexOfProbabilities);
            iterSpace.dimension(indexOfProbabilities).setValues<float>(
                   lowerQuantile, lowerQuantile,
                   iterSpace.dimension(indexOfProbabilities).value<float>(2));
            driver->read(table, d_name, iterSpace);

            fillUsingPreviousValue(table.col<REAL4>(0));
            Table lowerTable(table);
            float upperQuantile = upper.coordinate<float>(indexOfProbabilities);
            iterSpace.dimension(indexOfProbabilities).setValues<float>(
                   upperQuantile, upperQuantile,
                   iterSpace.dimension(indexOfProbabilities).value<float>(2));
            driver->read(table, d_name, iterSpace);
            fillUsingPreviousValue(table.col<REAL4>(0));
            Table upperTable(table);

            double lowerDistance = address.coordinate<float>(
              indexOfProbabilities) - lowerQuantile;
            double upperDistance = upperQuantile - address.coordinate<float>(
              indexOfProbabilities);
            interpolate<REAL4>(
                   table.col<REAL4>(0),
                   lowerTable.col<REAL4>(0), lowerDistance,
                   upperTable.col<REAL4>(0), upperDistance);
          }
        }
      }
      else if(wideDimension.meaning() == CumulativeProbabilities) {
        // From now on the iterSpace will only contain the probability
        // dimension to iterate over.
        iterSpace.addDimension(space.dimension(indexOfSpace));
        /// iterSpace.addDimension(space.dimension(indexOfSpace + 1));
        driver->read(table, d_name, iterSpace);
        interpolate(table.col<REAL4>(0));
      }
    }
  }
  else {
    // Default, no special case.
    assert(iterSpace.nrWideDimensions() == 1);
    assert(table.typeId(0) == TI_REAL4);
    driver->read(table, d_name, iterSpace);

    if(wideDimension.meaning() == Time) {
      fillUsingPreviousValue(table.col<REAL4>(0));
    }
    else if(wideDimension.meaning() == CumulativeProbabilities) {
      interpolate(table.col<REAL4>(0));
    }
  }
}



void DataSource::probability(
         Driver const& driver,
         REAL4 value,
         DataSpace const& space,
         REAL4* result) const
{
  assert(space.nrWideDimensions() == 1);
  assert(space.dimension(space.indexOfWideDimension()).meaning() ==
         CumulativeProbabilities);
  size_t indexOfProbabilities = space.indexOf(CumulativeProbabilities);

  double lowerDistance = 0.0;
  double upperDistance = 0.0;
  float quantile;
  REAL4 valueRead, lowerValue, upperValue;
  pcr::setMV(lowerValue);
  pcr::setMV(upperValue);

  // Loop over quantiles and determine probability of value passed in.
  for(DataSpaceIterator it = space.begin(); it != space.end(); ++it) {
    if(exists(space, *it)) {
      // A data source for the current quantile exists.
      // Read the value.
      quantile = (*it).coordinate<float>(indexOfProbabilities);
      driver.read(&valueRead, TI_REAL4, d_name, space, *it);

      if(!pcr::isMV(valueRead)) {
        if(valueRead < value) {
          // valueRead is increasing and still smaller than value.
          lowerValue = quantile;
          lowerDistance = std::abs(value - valueRead);
          assert(greaterOrComparable(lowerDistance, 0.0));
        }
        else if(comparable(valueRead, value)) {
          lowerValue = quantile;
          upperValue = quantile;
          lowerDistance = 0.0;
          upperDistance = 0.0;
        }
        else if(pcr::isMV(upperValue)) {
          // For the first time valueRead has increased past value.
          upperValue = quantile;
          upperDistance = valueRead - value;
          assert(upperDistance > 0.0);
          break;
        }
      }
    }
  }

  if(pcr::isMV(lowerValue) && pcr::isMV(upperValue)) {
    // No quantiles found.
    pcr::setMV(*result);
  }
  else {
    if(!pcr::isMV(lowerValue) && pcr::isMV(upperValue)) {
      // Past the range of quantiles. Assume cum prob is 1.0.
      *result = REAL4(1.0);
    }
    else if(pcr::isMV(lowerValue) && !pcr::isMV(upperValue)) {
      // Before the range of quantiles. Assume cum prob is 0.0.
      *result = REAL4(0.0);
    }
    else {
      // Within the range of quantiles. Interpolate for cum prob.
      assert(greaterOrComparable(lowerValue, REAL4(0.0)));
      assert(smallerOrComparable(upperValue, REAL4(1.0)));

      if(comparable(lowerValue, upperValue)) {
        *result = lowerValue;
      }
      else {
        interpolate<REAL4>(*result, lowerValue, lowerDistance,
              upperValue, upperDistance);
      }
    }

    assert(greaterOrComparable(*result, REAL4(0.0)));
    assert(smallerOrComparable(*result, REAL4(1.0)));
  }
}



template<typename T>
PCR_DAL_DECL void DataSource::read(
         Table& table,
         T const& value,
         DataSpaceAddress address) const
{
  assert(!pcr::isMV(value));
  assert(d_reader);

  // For each time step, determine probability related to value passed in.
  DataSpace const& space(dataSpace());
  assert(space.hasSpace());
  assert(space.hasTime());
  assert(space.hasCumProbabilities());

  size_t indexOfTime = space.indexOf(Time);
  size_t indexOfProbabilities = space.indexOf(CumulativeProbabilities);

  // Space with only time dimension.
  Dimension const& timeDimension(space.dimension(indexOfTime));
  DataSpace timeSpace(timeDimension);

  // Resize table based on the number of time steps.
  table.resize(timeDimension.nrCoordinates());

  // Column for the result.
  Array<REAL4>& probabilities(table.col<REAL4>(0));

  DataSpace iterSpace(space, address);
  iterSpace.dimension(indexOfProbabilities) = space.dimension(
         indexOfProbabilities);

  // No specific time step should be requested.
  assert(!address.isValid(indexOfTime));

  size_t i = 0;
  size_t timeStep;

  // Loop over all time steps.
  for(DataSpaceIterator it = timeSpace.begin(); it != timeSpace.end();
         ++it, ++i) {
    // Current time step.
    timeStep = (*it).coordinate<size_t>(0);

    // Fixate the time step.
    iterSpace.dimension(indexOfTime).template setValues<size_t>(timeStep,
         timeStep, size_t(1));

    // Determine probability of value at current time step.
    probability(*d_reader, value, iterSpace, &probabilities[i]);
  }
}



template
PCR_DAL_DECL void DataSource::read<REAL4>(
         Table& table,
         REAL4 const& value,
         DataSpaceAddress address) const;



void DataSource::read(
         Table& table,
         boost::any const& value,
         DataSpaceAddress const& address) const
{
  assert(table.nrCols() == 1);
  assert(table.typeId(0) == TI_REAL4);
  assert(value.type() == typeid(REAL4));
  read(table, boost::any_cast<REAL4>(value), address);
}



void DataSource::read(
         Vector& vector,
         DataSpaceAddress address) const
{
  assert(d_reader);

  DataSpace space(dataSpace());
  space.eraseDimension(Space);
  assert(space.isValid(address));

  VectorDriver* driver = dynamic_cast<VectorDriver*>(d_reader);
  assert(driver);

  if(!space.contains(address)) {
    vector.setAllMV();
  }
  else if(exists(space, address)) {
    // At the requested address in the data space data is found.
    // (Data space and address may be empty.)
    driver->read(vector, d_name, space, address);
  }
  else if(space.rank() == 0) {
    // Data space is empty.
    vector.setAllMV();
  }
  else {
    // The data space contains coordinates and at the current address there
    // is no vector to be found.
    assert(space.hasTime());
    size_t indexOfTime = space.indexOf(Time);

    DataSpace iterSpace(space, address);
    iterSpace.dimension(indexOfTime) = space.dimension(indexOfTime);

    // UsePrevious.
    DataSpaceAddress lower = findPreviousExistingAddress(iterSpace, address);

    if(!lower.isValid()) {
      vector.setAllMV();
    }
    else {
      driver->read(vector, d_name, iterSpace, lower);
    }
  }
}



/*
void DataSource::read(
         std::string& string,
         DataSpaceAddress const& address) const
{
  assert(d_reader);

  switch(d_reader->datasetType()) {
    case RASTER: {
      RasterDriver* driver = dynamic_cast<RasterDriver*>(d_reader);
      break;
    }
    case TABLE: {
      break;
    }
    default: {
      assert(false);
      break;
    }
  }


}
*/



/// void DataSource::setMissingDataStrategy(MissingDataStrategy strategy)
/// {
///   d_missingDataStrategy = strategy;
/// }



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

