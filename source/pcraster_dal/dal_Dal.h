#ifndef INCLUDED_DAL_DAL
#define INCLUDED_DAL_DAL



// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

#ifndef INCLUDED_BOOST_TUPLE_TUPLE
#include <boost/tuple/tuple.hpp>
#define INCLUDED_BOOST_TUPLE_TUPLE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif

#ifndef INCLUDED_DAL_DRIVER
#include "dal_Driver.h" // DataSpaceQueryResult
#define INCLUDED_DAL_DRIVER
#endif

#ifndef INCLUDED_DAL_FORMATS
#include "dal_Formats.h"
#define INCLUDED_DAL_FORMATS
#endif



namespace dal {
  // Dal declarations.
  class Dataset;
  class DataSpace;
  class Driver;
}



namespace dal {



//! This class represents the Data Abstraction Layer.
/*!
  Use an object of this class to read and write all kinds of datasets. Use
  more specialized Dal classes if you only want to read certain kinds of
  datasets.

  Here is an example of simple usage of this class:

  If your application supports all kinds of datasets the Dal library provides
  you can use this class as folows:

  \code
  // Create the data abstraction layer. This call will automatically add all
  // dataset drivers available in the Dal library.
  dal::Dal dal(true);

  // Use DAL to get at data.
  Dataset* dataset = dal.open("mydatafile.dat");
  if(!dataset) {
    // Dataset could not be opened, handle situation.
    // ...
  }

  // Use dataset.
  // ...
  \endcode

  Or, when your application support a certain subset of Dataset types and
  drivers:

  \code
  // Create the drivers for the datasets this application supports.
  dal::TableDriver tableDriver;
  dal::GDalDriver gdalDriver;

  // Create the data abstraction layer and add supported drivers.
  dal::Dal dal;
  dal.add(&tableDriver);
  dal.add(&gdalDriver);

  Dataset* dataset = dal.open("mydatafile.dat");
  if(!dataset) {
    // Dataset could not be opened, handle situation.
    // ...
  }

  // Use dataset.
  // ...
  \endcode

  If we know we only read raster datasets we can speed up things (but also
  have a look at the RasterDal class):

  \code
  Dataset* raster = dal.open("myrasterfile.dat", RASTER);
  if(!raster) {
    // Dataset could not be opened, handle situation.
    // ...
  }

  // Use dataset.
  // ...
  \endcode

  Dal supports caching of expensive information found while perfoming I/O of
  attributes. After a search for data, information found is stored, so a
  second search is not needed anymore.

  \sa MatrixDal, RasterDal, TableDal

  \todo Implement tests calling various drivers in turn to check whether
        the data is written and read correctly and survives conversions.
  \todo Use the extension of data set names to first select those driver
        that support names with those extensions.
  \todo The cache should support data sets of different type but with the
        same names. Store data set type along with the data set name.
  \todo Make using the cache optional.
*/
class PCR_DAL_DECL Dal: private boost::noncopyable
{

  friend class DalTest;

protected:

  //! Type of the collection with drivers.
  typedef std::vector<Driver*> Drivers;

private:

  //! All automatically added drivers. Just to be able to delete them again.
  Drivers          _autoAddedDrivers;

  //! Collection of Driver s. Use only.
  Drivers          _drivers;

  //! Type for cached information: driver, result of search for first data item, result of search for all data items.
  typedef boost::tuple<Driver*, DataSpaceQueryResult, DataSpaceQueryResult>
         CacheValue;

  // Type for lookup table with cached information. The key is the string representation of the attribute name and the data space.
  typedef std::map<std::string, CacheValue> Cache;

  //! Lookup table to find cached information of an attribute.
  Cache            _driversByDataset;

#ifdef DEBUG_DEVELOP
  //! Whether or not the user wants to debug stuff.
  bool             _debugging;
#endif

  void             addDriverToCache    (std::string const& name,
                                        DataSpace const& space,
                                        Driver* driver,
                                        DataSpaceQueryResult const& queryFirstResult,
                                        DataSpaceQueryResult const& queryResult);

  void             removeDriverFromCache(Driver* driver);

  DataSpaceQueryResult search          (Driver* driver,
                                        std::string const& name,
                                        DataSpace const& space,
                                        SearchHaltCondition haltCondition) const;

  DataSpaceQueryResult search          (Driver* driver,
                                        std::string const& name,
                                        DataSpace const& space,
                                        SearchMethod searchMethod,
                                        SearchHaltCondition haltCondition) const;

  Cache::const_iterator cacheValue     (std::string const& name,
                                        DataSpace const& space) const;

  Cache::iterator  cacheValue          (std::string const& name,
                                        DataSpace const& space);

  bool             inCache             (std::string const& name,
                                        DataSpace const& space) const;

  DataSpaceQueryResult queryFirstResult(std::string const& name,
                                        DataSpace const& space);

  DataSpaceQueryResult queryResult     (std::string const& name,
                                        DataSpace const& space) const;

  void             setQueryFirstResult (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceQueryResult const& result);

  void             setQueryResult      (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceQueryResult const& result);

protected:

  void             autoAddDriver       (Driver* driver);

  Drivers&         autoAddedDrivers    ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Dal                 (bool addAllDrivers=false);

  virtual          ~Dal                ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             add                 (Driver* driver);

  void             remove              (Driver* driver);

  void             favourDrivers       (std::vector<std::string> const& names);

#ifdef DEBUG_DEVELOP
  void             setDebugging        (bool setting);
#endif

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrDrivers           () const;

  Drivers const&   drivers             () const;

  DatasetType      datasetType         (std::string const& name);

  DatasetType      datasetType         (std::string const& name,
                                        DataSpace const& space);

  boost::tuple<DataSpaceQueryResult, Driver*> search(
                                        std::string const& name,
                                        DataSpace const& space,
                                        SearchMethod searchMethod,
                                        SearchHaltCondition haltCondition) const;

  boost::tuple<DataSpaceQueryResult, Driver*> search(
                                        std::string const& name,
                                        DatasetType datasetType,
                                        DataSpace const& space,
                                        SearchMethod searchMethod,
                                        SearchHaltCondition haltCondition) const;

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        DatasetType datasetType) const;

  boost::tuple<boost::shared_ptr<Dataset>, Driver*> open(
                                        std::string const& name) const;

  boost::tuple<boost::shared_ptr<Dataset>, Driver*> open(
                                        std::string const& name,
                                        DatasetType datasetType) const;

  boost::tuple<boost::shared_ptr<Dataset>, Driver*> open(
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        DatasetType datasetType) const;

  boost::shared_ptr<Dataset> read      (std::string const& name) const;

  Driver*          driver              (DataSpaceQueryResult const& result) const;

  Driver*          driverByName        (std::string const& name);

  Driver*          driverByDataset     (std::string const& name,
                                        DataSpace const& space) const;

  bool             hasDriverByName     (std::string const& name);

  Formats          formats             () const;

  Formats          readerFormats       () const;

  Formats          writerFormats       (
                             DatasetType datasetType=NR_DATASET_TYPES) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif

