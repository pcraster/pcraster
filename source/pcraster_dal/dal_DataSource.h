#ifndef INCLUDED_DAL_DATASOURCE
#define INCLUDED_DAL_DATASOURCE



// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

#include <boost/shared_ptr.hpp>

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif

#ifndef INCLUDED_DAL_DRIVER
#include "dal_Driver.h"
#define INCLUDED_DAL_DRIVER
#endif

#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif

/// #ifndef INCLUDED_DAL_RASTERDRIVER
/// #include "dal_RasterDriver.h"
/// #define INCLUDED_DAL_RASTERDRIVER
/// #endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif


namespace dal {
  // DataSource declarations.
  class Dal;
  class FeatureLayer;
  class MemoryTableDriver;
  class Table;
  class TableDriver;
  class Vector;
}



namespace dal {



//! High level class for querying, reading and writing data sets.
/*!
  This class provides the user of the Dal library with a generic interface
  which separates the in-memory datasets from the way the values are stored.

  \todo Constructor should configure a reader dal with only the reader driver
        in it. Setting the SpeedForMemory option should insert a relevant
        Memory*Driver to this dal, before the reader driver. Reading
        data should have a side effect of writing read data using the
        Memory*Driver, if the data is not already present in the cache.
        Or, perhaps, create a CachingRasterDal and CachingTableDal which does
        al the magick for us. Or make this a setting of Dal:
        setCacheOnRead(bool), setCacheOnWrite(bool). This requires that the
        dal object has relevant Memory*Driver(s) set.
        What about cache on reading, cache on writing and cache on both.
*/
class PCR_DAL_DECL DataSource: private boost::noncopyable
{

  friend class DataSourceTest;

public:

private:

  std::string      d_name;

  //! Data space enclosing the space one unit of data read or written.
  DataSpace        d_enclosingSpace;

  //! Data space of one unit of data read or written.
  DataSpace        d_unitSpace;

  DataSpace        d_space;

  Driver*          d_reader;

  //! Driver for reading and writing the cache for tabels.
  MemoryTableDriver* d_tableCacheDriver;

  DataSourceProperties d_properties;

  /// //! TODO Remove from interface.
  /// MissingDataStrategy d_missingDataStrategy;

  void             init                (std::string const& name,
                                        DataSpace const& space);

  bool             exists              (DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  DataSpaceAddress findExistingAddress (DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        MissingDataStrategy strategy) const;

  DataSpaceAddress findPreviousExistingAddress(
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  // DataSpaceAddress findPreviousExistingAddress(
  //                                       DataSpace const& space) const;

  DataSpaceAddress findNextExistingAddress(
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  // void             read                (TableDriver const& driver,
  //                                       Table& table,
  //                                       DataSpaceAddress address) const;

  // void             read                (RasterDriver const& driver,
  //                                       Table& table,
  //                                       DataSpaceAddress address) const;

  template<typename T>
  void             applyMissingDataStrategy(
                                       Array<T>& array) const;

  void             probability         (Driver const& driver,
                                        REAL4 value,
                                        DataSpace const& space,
                                        REAL4* result) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataSource          (std::string const& name);

                   DataSource          (std::string const& name,
                                        DataSpace const& space);

  /* virtual */    ~DataSource         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  /// void             setMissingDataStrategy(MissingDataStrategy strategy);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Driver*          reader              ();

  std::string const& name              () const;

  DataSpace const& unitDataSpace       () const;

  DataSpace const& enclosingDataSpace  () const;

  DataSpace const& dataSpace           () const;

  bool             exists              (DataSpaceAddress const& address) const;

  template<class T>
  T*               open                ();

  Raster*          raster              () const;

  // Raster*          raster              (DataSpaceAddress const& address) const;

  Raster*          raster              (// DataSpace const& space,
                                        DataSpaceAddress address,
                                        TypeId typeId=TI_NR_TYPES) const;

  void             read                (Raster& raster,
                                        DataSpaceAddress address) const;

  void             read                (Raster& raster,
                                        boost::any const& value,
                                        DataSpaceAddress const& address) const;

  template<typename T>
  PCR_DAL_DECL void read               (Raster& raster,
                                        T const& value,
                                        DataSpaceAddress address) const;

  void             read                (FeatureLayer& layer,
                                        DataSpaceAddress address) const;

  void             read                (FeatureLayer& layer,
                                        boost::any const& value,
                                        DataSpaceAddress const& address) const;

  template<typename T>
  PCR_DAL_DECL void read               (FeatureLayer& layer,
                                        T const& value,
                                        DataSpaceAddress address) const;

  void             read                (Vector& layer,
                                        DataSpaceAddress address) const;

  void             read                (Table& table,
                                        DataSpaceAddress const& address) const;

  void             read                (Table& table,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  template<typename T>
  PCR_DAL_DECL void read               (Table& table,
                                        T const& value,
                                        DataSpaceAddress address) const;

  void             read                (Table& table,
                                        boost::any const& value,
                                        DataSpaceAddress const& address) const;

  template<typename T>
  void             uniqueValues        (std::set<T>& values,
                                        TypeId typeId=TI_NR_TYPES) const;

  template<typename T>
  void             uniqueValues        (std::set<T>& values,
                                        // DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId=TI_NR_TYPES) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
inline T* DataSource::open()
{
  DataSpaceQueryResult result(d_reader->search(d_name, enclosingDataSpace(),
         HaltOnFirstItemFound));

  return result
         ? dynamic_cast<T*>(d_reader->open(d_name, enclosingDataSpace(),
              result.address()))
         : 0;
}

template<typename T>
inline void DataSource::uniqueValues(
         std::set<T>& values,
         TypeId typeId) const
{
  if(dataSpace().hasRaster()) {
    assert(d_reader);

    DataSpace space(dataSpace());

    // Erase irrelevant stuff. Otherwise the raster drivers will complain.
    space.eraseDimension(Space);

    if(space.rank() == 0) {
      // No dimensions to iterator over.
      uniqueValues(values, /* space, */ space.address(), typeId);
    }
    else {
      for(DataSpaceIterator it = space.begin(); it != space.end(); ++it) {
        uniqueValues(values, /* space, */ *it, typeId);
      }
    }
  }
}

template<typename T>
inline void DataSource::uniqueValues(
         std::set<T>& values,
         // DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  // assert(!space.hasSpace());

  boost::shared_ptr<Raster> raster(this->raster(/* space, */ address, typeId));
  assert(raster);

  // Iterate over inidividual cells.
  T const* array = raster->template cells<T>();
  for(size_t i = 0; i < raster->nrCells(); ++i) {
    if(!pcr::isMV(array[i])) {
      values.insert(array[i]);
    }
  }
}

/// template<typename T>
/// inline void DataSource::applyMissingDataStrategy(
///          Array<T>& array) const
/// {
///   switch(d_missingDataStrategy) {
///     case Interpolate: {
///       interpolate(array);
///       break;
///     }
///     case UsePrevious: {
///       fillUsingPreviousValue(array);
///       break;
///     }
///     case UseNext: {
///       assert(false);
///       break;
///     }
///     default: {
///       // Do nothing.
///       break;
///     }
///   }
/// }



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
