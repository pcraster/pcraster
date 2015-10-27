#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

#include <boost/shared_ptr.hpp>

#ifndef INCLUDED_QSQLDATABASE
#include <QSqlDatabase>
#define INCLUDED_QSQLDATABASE
#endif

#ifndef INCLUDED_QSTRINGLIST
#include <QStringList>
#define INCLUDED_QSTRINGLIST
#endif

#ifndef INCLUDED_OGRSF_FRMTS
#include <ogrsf_frmts.h>
#define INCLUDED_OGRSF_FRMTS
#endif

#ifndef INCLUDED_GDAL_PRIV
#include "gdal_priv.h"
#define INCLUDED_GDAL_PRIV
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_CONFIGURE
#include "dev_Configure.h"
#define INCLUDED_DEV_CONFIGURE
#endif

// Module headers.
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

#ifndef INCLUDED_DAL_CSFRASTERDRIVER
#include "dal_CSFRasterDriver.h"
#define INCLUDED_DAL_CSFRASTERDRIVER
#endif

#ifndef INCLUDED_DAL_GEOEASTABLEDRIVER
#include "dal_GeoEASTableDriver.h"
#define INCLUDED_DAL_GEOEASTABLEDRIVER
#endif

#ifndef INCLUDED_DAL_GDALRASTERDRIVER
#include "dal_GDALRasterDriver.h"
#define INCLUDED_DAL_GDALRASTERDRIVER
#endif

#ifndef INCLUDED_DAL_LIBRARY
#include "dal_Library.h"
#define INCLUDED_DAL_LIBRARY
#endif

#ifndef INCLUDED_DAL_MEMORYRASTERDRIVER
#include "dal_MemoryRasterDriver.h"
#define INCLUDED_DAL_MEMORYRASTERDRIVER
#endif

#ifndef INCLUDED_DAL_OGRFEATUREDRIVER
#include "dal_OgrFeatureDriver.h"
#define INCLUDED_DAL_OGRFEATUREDRIVER
#endif

#ifndef INCLUDED_DAL_PCRBLOCKDRIVER
#include "dal_PCRBlockDriver.h"
#define INCLUDED_DAL_PCRBLOCKDRIVER
#endif

#ifndef INCLUDED_DAL_SQLTABLEDRIVER
#include "dal_SQLTableDriver.h"
#define INCLUDED_DAL_SQLTABLEDRIVER
#endif

#ifndef INCLUDED_DAL_TEXTMATRIXDRIVER
#include "dal_TextMatrixDriver.h"
#define INCLUDED_DAL_TEXTMATRIXDRIVER
#endif

#ifndef INCLUDED_DAL_TEXTTABLEDRIVER
#include "dal_TextTableDriver.h"
#define INCLUDED_DAL_TEXTTABLEDRIVER
#endif

#ifndef INCLUDED_DAL_VECTORDRIVER
#include "dal_VectorDriver.h"
#define INCLUDED_DAL_VECTORDRIVER
#endif



/*!
  \file
  This file contains the implementation of the Dal class.
*/



namespace dal {

#ifdef DEBUG_DEVELOP

#define ADD_DRIVER(driver) \
  if(_debugging) { \
    std::cout << "add driver " << driver->name() << std::endl; \
  }

#define QUERY(driver, name, space) \
  if(_debugging) { \
    std::cout << "query " << name << " in " << dataSpaceToString(space) << " using " << driver.name() << std::endl; \
  }

#define QUERY_FIRST(driver, name, space) \
  if(_debugging) { \
    std::cout << "query first " << name << " in " << dataSpaceToString(space) << " using " << driver.name() << std::endl; \
  }

#else
#define ADD_DRIVER(driver)
#define QUERY(driver, name, space)
#define QUERY_FIRST(driver, name, space)
#endif

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DAL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DAL MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \sa        add(Driver*)

  If \a addAllDrivers is false (the default case) no drivers are added
  to the object. In this case you need to explicitly add the drivers your
  application uses.

  If \a addAllDrivers is true, than all drivers this library knows about are
  automatically added to the object. In this case it is required that the
  Library::isInitialised().
*/
Dal::Dal(
         bool addAllDrivers)

#ifdef DEBUG_DEVELOP
  : _debugging(false)
#endif

{
  assert((!addAllDrivers) || Library::isInitialised());

#ifdef DEBUG_DEVELOP
  // setDebugging(true);
#endif

  if(addAllDrivers) {
    #include "autoAddRasterDrivers.cc"
#ifdef DEVBASE_GDAL_LIBRARY_HAS_OGR_SUPPORT
    #include "autoAddFeatureDrivers.cc"
#endif
    #include "autoAddTableDrivers.cc"
    #include "autoAddVectorDrivers.cc"
    #include "autoAddMatrixDrivers.cc"
    #include "autoAddBlockDrivers.cc"
  }

  // Limit drivers to the set from optional $PCRASTER_DAL_FORMATS environment
  // variable.
  Drivers selectedDrivers;
  Drivers copyOfAutoAddedDrivers(_autoAddedDrivers);
  Drivers copyOfDrivers(_drivers);

  Environment const& environment(Client::library().environment());

  BOOST_FOREACH(std::string const& name, environment.formatNames()) {
    // See if a driver by that name exists.
    Drivers::iterator it = std::find_if(_autoAddedDrivers.begin(),
         _autoAddedDrivers.end(), boost::bind(
           std::equal_to<std::string>(), boost::bind(&Driver::name, _1), name));

    if(it != _autoAddedDrivers.end()) {
      // Move driver to new collection.
      selectedDrivers.push_back(*it);
      _autoAddedDrivers.erase(it);
      _drivers.erase(std::find_if(_drivers.begin(),
         _drivers.end(), boost::bind(std::equal_to<std::string>(),
              boost::bind(&Driver::name, _1), name)));
    }
  }

  if(selectedDrivers.empty()) {
    // Revert changes.
    _autoAddedDrivers = copyOfAutoAddedDrivers;
    _drivers = copyOfDrivers;
  }
  else {
    // Erase unneeded drivers.
    BOOST_FOREACH(Driver* driver, _autoAddedDrivers) {
      delete driver;
    }

    // Replace collection.
    _autoAddedDrivers = selectedDrivers;
    _drivers = selectedDrivers;
  }
}



//! Destructor.
/*!
  Since added drivers are not owned by the Dal object, they are not deleted
  here.

  Deletes all automatically added drivers added with autoAddDriver(Driver*).
*/
Dal::~Dal()
{
  for(Drivers::iterator it = _autoAddedDrivers.begin();
         it != _autoAddedDrivers.end(); ++it) {
    delete *it;
  }
}



/*! Adds \a driver to the collection.
  \param     driver Driver to add.
  \warning   The \a driver is owned by this object and will be deleted in the
             destructor.
*/
void Dal::autoAddDriver(
         Driver* driver)
{
  _autoAddedDrivers.push_back(driver);
  add(driver);
}



//! Returns the collection of automatically added drivers.
/*!
  \return    Collection with drivers.
*/
Dal::Drivers& Dal::autoAddedDrivers()
{
  return _autoAddedDrivers;
}



//! Returns the number of drivers used by this object.
/*!
  \return    Number of drivers.
*/
size_t Dal::nrDrivers() const
{
  return _drivers.size();
}



//! Adds \a driver to the collection with drivers.
/*!
  \param     driver Driver to add.
  \warning   Do not use this object after you have deleted a driver which you
             have added here.
  \sa        remove(Driver*)

  Use this function to add drivers for datasets your application supports.

  Nothing happens if \a driver is 0 or if \a driver is already in the
  collection.

  Ownership of \a driver stays with the caller. To be save, call
  remove(Driver*) before destroying an added driver.
*/
void Dal::add(
         Driver* driver)
{
  if(driver && std::find(_drivers.begin(), _drivers.end(), driver) ==
         _drivers.end()) {
    _drivers.push_back(driver);
  }
}



//! Removes \a driver from the collection of drivers.
/*!
  \param     driver Driver to remove.
  \sa        add(Driver*)

  Nothing happens if \a driver is not part of the collection.
*/
void Dal::remove(
         Driver* driver)
{
  Drivers::iterator it = std::find(_drivers.begin(), _drivers.end(), driver);

  if(it != _drivers.end()) {
    _drivers.erase(it);
    removeDriverFromCache(driver);
  }
}



//! Puts those drivers with names equal to the names in \a names to the front of the layered collection.
/*!
  \param     name Names of driver to favour.

  When opening data sets, the drivers are tried in turn. The drivers at the
  front of the layered collection are tried first. Since opening data in
  large data spaces can take a lot of time when the wrong drivers are used,
  it makes sense to put the most likely drivers to succeed to the front of
  the collection. Which ones that are depends on the specific application.

  Nothing happens for those names passed in that don't have a corresponding
  driver with the same name.
*/
void Dal::favourDrivers(
         std::vector<std::string> const& names)
{
  std::vector<Driver*> drivers;

  BOOST_FOREACH(std::string const& name, names) {
    for(Drivers::iterator it = _drivers.begin(); it != _drivers.end(); ++it) {
      if((*it)->name() == name) {
        drivers.push_back(*it);
        _drivers.erase(it);
        break;
      }
    }
  }

  _drivers.insert(_drivers.begin(), drivers.begin(), drivers.end());
}



//! Adds the driver and some data properties associated with a data set to the cache.
/*!
  \param     name Name of data set.
  \param     space Data space data is present in.
  \param     driver Driver able to handle the data.
  \param     queryFirstResult Result of querying the data set for the first
             data element.
  \param     queryResult Result of querying the whole data set.
  \warning   The key to the cache is the name/space combination. Always use
             this same combination when using the cache later on.
  \warning   Either \a queryFirstResult or \a queryResult must be valid
             objects.
  \sa        .

  These properties are often expensive to determine.

  If properties are already present in the cache for this data set, but
  queryFirstResult or queryResult where not valid yet, the invalid items
  are updated.
*/
void Dal::addDriverToCache(
         std::string const& name,
         DataSpace const& space,
         Driver* driver,
         DataSpaceQueryResult const& queryFirstResult,
         DataSpaceQueryResult const& queryResult)
{
  assert(library()->cacheDatasetInfo());

  if(!inCache(name, space)) {
    // First time this driver is added to the cache.
    std::string key(nameAndSpaceToString(name, space));
    _driversByDataset[key] = boost::make_tuple(
         driver, DataSpaceQueryResult(), DataSpaceQueryResult());

    if(queryFirstResult) {
      setQueryFirstResult(name, space, queryFirstResult);
    }

    if(queryResult) {
      setQueryResult(name, space, queryResult);
    }
  }
  else {
    // This driver already exists in the cache.
    if(!this->queryFirstResult(name, space) && queryFirstResult) {
      setQueryFirstResult(name, space, queryFirstResult);
    }

    if(!this->queryResult(name, space) && queryResult) {
      setQueryResult(name, space, queryResult);
    }
  }
}



//! Removes all tuples with \a driver pointers from the collection.
/*!
  \param     driver Driver who's tuples needs to be removed.
  \todo      Implement alternative version which compiles under MSC.

  Nothing happens if driver is not part of any tuple.
*/
void Dal::removeDriverFromCache(
         Driver* driver)
{
  for(Cache::iterator it = _driversByDataset.begin();
         it != _driversByDataset.end(); ) {
    if(boost::get<0>(it->second) == driver) {
      _driversByDataset.erase(it++);
    }
    else {
      ++it;
    }
  }
}



/*
  \overload
*/
DatasetType Dal::datasetType(
         std::string const& name)
{
  boost::shared_ptr<Dataset> dataset;
  boost::tie(dataset, boost::tuples::ignore) = open(name);

  return dataset ? dataset->type() : NR_DATASET_TYPES;
}




//! Returns the type of the data in \a name.
/*!
  \deprecated { Use open to get a DataSpaceQueryResult object. }
  \param     name Name of data set.
  \param     space Data space data is located in.
  \return    Dataset type.

  This function returns NR_DATASET_TYPES if no driver is able to open the
  dataset.
*/
DatasetType Dal::datasetType(
         std::string const& name,
         DataSpace const& space)
{
  DataSpaceQueryResult result;
  boost::tie(result, boost::tuples::ignore) = search(name, NR_DATASET_TYPES,
      space, SearchThisSpaceOnly, HaltOnFirstItemFound);
  return result.datasetType();
}



DataSpaceQueryResult Dal::search(
         Driver* driver,
         std::string const& name,
         DataSpace const& space,
         SearchHaltCondition haltCondition) const
{
  assert(driver);

  QUERY((*driver), name, space)
  DataSpaceQueryResult result = driver->search(name, space, haltCondition);

  if(result) {
    if(library()->cacheDatasetInfo()) {
      // Store results for later reference. This is expensive information.
      switch(haltCondition) {
        case HaltOnFirstItemFound: {
          // Side effect.
          const_cast<Dal*>(this)->addDriverToCache(name, space, driver, result,
                DataSpaceQueryResult());
          break;
        }
        case SearchForAllItems: {
          // Side effect.
          const_cast<Dal*>(this)->addDriverToCache(name, space, driver,
                DataSpaceQueryResult(), result);
          break;
        }
      }
    }
  }

  return result;
}



DataSpaceQueryResult Dal::search(
         Driver* driver,
         std::string const& name,
         DataSpace const& space,
         SearchMethod searchMethod,
         SearchHaltCondition haltCondition) const
{
  assert(driver);

  // First search in the data space passed in for data items.
  DataSpaceQueryResult result = search(driver, name, space, haltCondition);

  if(!result && searchMethod == NarrowSpaceWhenNeeded && !space.isEmpty()) {
    // No data found, see if we can find data items in sub-spaces.
    DataSpace subSpace(space);

    do {
      subSpace.eraseDimension(subSpace.size() - 1);
      result = search(driver, name, subSpace, haltCondition);
    } while(!result && !subSpace.isEmpty());
  }

  return result;
}



boost::tuple<DataSpaceQueryResult, Driver*> Dal::search(
         std::string const& name,
         DataSpace const& space,
         SearchMethod searchMethod,
         SearchHaltCondition haltCondition) const
{
  return search(name, NR_DATASET_TYPES, space, searchMethod, haltCondition);
}



boost::tuple<DataSpaceQueryResult, Driver*> Dal::search(
         std::string const& name,
         DatasetType datasetType,
         DataSpace const& space,
         SearchMethod searchMethod,
         SearchHaltCondition haltCondition) const
{
  DataSpaceQueryResult result;
  Driver* driver = 0;

  if(inCache(name, space)) {
    result = queryResult(name, space);
    driver = driverByDataset(name, space);
    assert(driver);

    if(!result) {
      // No (successful) search has been performed for this data yet, but the
      // driver is already known.
      assert(datasetType == NR_DATASET_TYPES ||
         driver->datasetType() == datasetType);

      QUERY((*driver), name, space)
      result = driver->search(name, space, haltCondition);

      if(result) {
        // Store this expensive information in the cache.
        // Side effect.
        const_cast<Dal*>(this)->setQueryResult(name, space, result);
      }
    }
  }
  else {
    // 1. Copy collection of drivers.
    Drivers drivers(_drivers);

    // If a data set type is passed in:
    //   2. Remove the drivers that don't support the passed in data type.
    if(datasetType != NR_DATASET_TYPES) {
      drivers.erase(std::remove_if(drivers.begin(), drivers.end(),
         boost::bind(std::not_equal_to<DatasetType>(),
              boost::bind(&Driver::datasetType, _1), datasetType)),
         drivers.end());
    }

    // If name has extension:
    //   3. Put the drivers that support the extension to the front.
    //      (Maintain the ordering.)
    // TODO

    // Try each driver in turn.
    BOOST_FOREACH(Driver* aDriver, drivers) {
      result = search(aDriver, name, space, searchMethod, haltCondition);

      if(result) {
        driver = aDriver;
        break;
      }
    }
  }

  return boost::make_tuple(result, driver);
}



bool Dal::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         DatasetType datasetType) const
{
  bool result = false;
  Driver* driver = driverByDataset(name, space);

  if(driver) {
    result = driver->exists(name, space, address);
  }
  else {
    // Copy collection of drivers.
    Drivers drivers(_drivers);

    // If a data set type is passed in:
    // Remove the drivers that don't support the passed in data type.
    if(datasetType != NR_DATASET_TYPES) {
      drivers.erase(std::remove_if(drivers.begin(), drivers.end(),
         boost::bind(std::not_equal_to<DatasetType>(),
              boost::bind(&Driver::datasetType, _1), datasetType)),
         drivers.end());
    }

    // If name has extension:
    // Put the drivers that support the extension to the front.
    // (Maintain the ordering.)
    // TODO

    // Try each driver in turn.
    BOOST_FOREACH(Driver* driver, drivers) {
      result = driver->exists(name, space, address);

      if(result) {
        if(library()->cacheDatasetInfo()) {
          // Side effect.
          // Store name/space - driver combination.
          const_cast<Dal*>(this)->addDriverToCache(name, space, driver,
                DataSpaceQueryResult(), DataSpaceQueryResult());
        }
        break;
      }
    }
  }

  return result;
}



/*!
  \overload
*/
boost::tuple<boost::shared_ptr<Dataset>, Driver*> Dal::open(
         std::string const& name) const
{
  return open(name, NR_DATASET_TYPES);
}



//! Creates a dataset for the \a datasetType data pointed to by \a name.
/*!
  \param     name Name of dataset.
  \param     datasetType Dataset type of dataset.
  \return    Pointer to Dataset object created for the data in \a name.

  No data is loaded when this function returns. The first \a datasetType driver
  supporting the dataset may create one. If no driver can create a Dataset from
  \a name, 0 is returned.
*/
boost::tuple<boost::shared_ptr<Dataset>, Driver*> Dal::open(
         std::string const& name,
         DatasetType datasetType) const
{
  DataSpaceQueryResult result;
  Driver* driver;
  boost::tie(result, driver) = search(name, datasetType, DataSpace(),
         SearchThisSpaceOnly, HaltOnFirstItemFound);

  if(result) {
    assert(driver);
    return boost::make_tuple(boost::shared_ptr<Dataset>(driver->open(name)),
        driver);
  }

  assert(!driver);
  return boost::make_tuple(boost::shared_ptr<Dataset>(), driver);
}



boost::tuple<boost::shared_ptr<Dataset>, Driver*> Dal::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         DatasetType datasetType) const
{
  boost::shared_ptr<Dataset> dataset;
  Driver* driver = driverByDataset(name, space);

  if(driver) {
    dataset.reset(driver->open(name, space, address));
  }
  else {
    // Copy collection of drivers.
    Drivers drivers(_drivers);

    // If a data set type is passed in:
    // Remove the drivers that don't support the passed in data type.
    if(datasetType != NR_DATASET_TYPES) {
      drivers.erase(std::remove_if(drivers.begin(), drivers.end(),
         boost::bind(std::not_equal_to<DatasetType>(),
              boost::bind(&Driver::datasetType, _1), datasetType)),
         drivers.end());
    }

    // If name has extension:
    // Put the drivers that support the extension to the front.
    // (Maintain the ordering.)
    // TODO

    // Try each driver in turn.
    BOOST_FOREACH(Driver* driver, drivers) {
      dataset.reset(driver->open(name, space, address));

      if(dataset) {
        if(library()->cacheDatasetInfo()) {
          // Side effect.
          // Store name/space - driver combination.
          const_cast<Dal*>(this)->addDriverToCache(name, space, driver,
                DataSpaceQueryResult(), DataSpaceQueryResult());
        }
        break;
      }
    }
  }

  return boost::make_tuple(dataset, driver);
}



boost::shared_ptr<Dataset> Dal::read(
         std::string const& name) const
{
  assert(nrDrivers() > 0);

  boost::shared_ptr<Dataset> dataset;
  Driver* driver;
  boost::tie(dataset, driver) = open(name);

  if(!dataset) {
    throwCannotBeOpened(name);
  }

  assert(driver);

  return boost::shared_ptr<Dataset>(driver->read(name));
}



Dal::Cache::const_iterator Dal::cacheValue(
         std::string const& name,
         DataSpace const& space) const
{
  std::string key(nameAndSpaceToString(name, space));
  Cache::const_iterator it = _driversByDataset.find(key);

  return it;
}



Dal::Cache::iterator Dal::cacheValue(
         std::string const& name,
         DataSpace const& space)
{
  std::string key(nameAndSpaceToString(name, space));
  Cache::iterator it = _driversByDataset.find(key);

  return it;
}



bool Dal::inCache(
         std::string const& name,
         DataSpace const& space) const
{
  return cacheValue(name, space) != _driversByDataset.end();
}



Driver* Dal::driver(
         DataSpaceQueryResult const& result) const
{
  assert(library()->cacheDatasetInfo());
  return inCache(result.name(), result.space())
         ? boost::get<0>((*cacheValue(result.name(), result.space())).second)
         : 0;
}



Driver* Dal::driverByDataset(
         std::string const& name,
         DataSpace const& space) const
{
  assert(library()->cacheDatasetInfo());
  return inCache(name, space)
         ? boost::get<0>((*cacheValue(name, space)).second)
         : 0;
}



DataSpaceQueryResult Dal::queryFirstResult(
         std::string const& name,
         DataSpace const& space)
{
  return inCache(name, space)
         ? boost::get<1>((*cacheValue(name, space)).second)
         : DataSpaceQueryResult();
}



DataSpaceQueryResult Dal::queryResult(
         std::string const& name,
         DataSpace const& space) const
{
  return inCache(name, space)
         ? boost::get<2>((*cacheValue(name, space)).second)
         : DataSpaceQueryResult();
}



void Dal::setQueryFirstResult(
         std::string const& name,
         DataSpace const& space,
         DataSpaceQueryResult const& result)
{
  assert(result);

  boost::get<1>((*cacheValue(name, space)).second) = result;
}



void Dal::setQueryResult(
         std::string const& name,
         DataSpace const& space,
         DataSpaceQueryResult const& result)
{
  assert(result);

  boost::get<2>((*cacheValue(name, space)).second) = result;

  if(!queryFirstResult(name, space)) {
    setQueryFirstResult(name, space, result);
  }
}



bool Dal::hasDriverByName(
         std::string const& name)
{
  bool result = false;

  for(Drivers::iterator it = _drivers.begin(); it != _drivers.end(); ++it) {
    if((*it)->name() == name) {
      result = true;
      break;
    }
  }

  return result;
}



//! Selects a driver by its name.
/*!
  \param     name Name of the driver.
  \return    Pointer to the driver or 0 if no such driver exists.
*/
Driver* Dal::driverByName(
         std::string const& name)
{
  Driver* result = 0;

  for(Drivers::iterator it = _drivers.begin(); it != _drivers.end(); ++it) {
    if((*it)->name() == name) {
      result = *it;
      break;
    }
  }

  return result;
}



Dal::Drivers const& Dal::drivers() const
{
  return _drivers;
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Make use of a single function with an argument specifying
             reader, writer or both.
*/
Formats Dal::formats() const
{
  Formats result;

  // Add a format object for each driver currently stored.
  for(size_t i = 0; i < _drivers.size(); ++i) {
    Driver const* driver = _drivers[i];
    result.push_back(driver->format());
  }

  return result;
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Make use of a single function with an argument specifying
             reader, writer or both.
*/
Formats Dal::readerFormats() const
{
  Formats result;

  for(size_t i = 0; i < _drivers.size(); ++i) {
    Driver const* driver = _drivers[i];
    DriverProperties const& properties =
         driver->properties().value<DriverProperties>(DAL_DRIVER_GENERAL);

    if(properties & Reader) {
      result.push_back(driver->format());
    }
  }

  return result;
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Make use of a single function with an argument specifying
             reader, writer or both.
*/
Formats Dal::writerFormats(
         DatasetType datasetType) const
{
  Formats result;

  for(size_t i = 0; i < _drivers.size(); ++i) {
    Driver const* driver = _drivers[i];

    if(datasetType == NR_DATASET_TYPES ||
         driver->datasetType() == datasetType) {

      DriverProperties const& properties =
           driver->properties().value<DriverProperties>(DAL_DRIVER_GENERAL);

      if(properties & Writer) {
        result.push_back(driver->format());
      }
    }
  }

  return result;
}



#ifdef DEBUG_DEVELOP
void Dal::setDebugging(
         bool setting)
{
  _debugging = setting;
}
#endif



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

