#ifndef INCLUDED_DAL_MEMORYDATAPOOL
#define INCLUDED_DAL_MEMORYDATAPOOL



// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif

#ifndef INCLUDED_DAL_MEMORYRASTERDATA
#include "dal_MemoryRasterData.h"
#define INCLUDED_DAL_MEMORYRASTERDATA
#endif

#ifndef INCLUDED_DAL_MEMORYTABLEDATA
#include "dal_MemoryTableData.h"
#define INCLUDED_DAL_MEMORYTABLEDATA
#endif



namespace dal {
  // MemoryDataPool declarations.
  class DataSpaceAddress;
}



namespace dal {



//! Class for collection of data which are stored in memory.
/*!
  This class can be used to store data objects which remain in memory. The
  Library object has a MemoryDataPool object which is used by the
  Memory*Drivers (for example the MemoryRasterDriver) to look for data.

  Data which is stored in memory is an alternative for data which is stored
  in the file system or in database tables, for example. The MemoryDataPool
  object can be filled with data created by the process. The Memory*Drivers
  can then be used to find the data again. Also, a MemoryDataPool can be used
  to cache data read from other sources. When data has to be read repeatedly
  and when memory is abundant, speed can be gained this way. The
  Library object has a MemoryDataPool object which can be used for this
  purpose.

  Every data item stored in the pool must have a name associated with it to
  be able to find it and to be able to implement the Memory*Drivers similar
  to all other data drivers. Different data items can have the same name as
  long as the data space in which the data is defined is different. This can
  be compared to different files in a file system which have the same name,
  but a different path.

  The data stored in the pool is not owned by the pool. The pool is filled
  and cleaned by the client code. In a typical situation a process creates
  data, fills the pool, calls code which queries the pool by use of the
  Memory*Drivers, and, after it is certain that the pool is not used anymore,
  cleans the pool again.

  \todo Add code to make ownership configurable: memory data pools must be
        allowed to own all the data or not own all the data. That will do for
        now. Use boost::shared_ptr.
  \todo Maybe the pool is the owner of the data and should decide if and when
        the actual data is to be deleted from memory. Then the Memory*Data
        objects should not have to care.

*/
class MemoryDataPool: private boost::noncopyable
{

  friend class MemoryDataPoolTest;

private:

  //! Pool of in-memory rasters.
  std::multimap<std::string, MemoryRasterData> d_memoryRasters;

  //! Pool of in-memory tables.
  std::multimap<std::string, MemoryTableData> d_memoryTables;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MemoryDataPool      ();

  /* virtual */    ~MemoryDataPool     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             add                 (std::string const& name,
                                        MemoryRasterData const& data);

  void             add                 (std::string const& name,
                                        MemoryTableData const& data);

  void             add                 (std::string const& name,
                                        MemoryRasterData const& data,
                                        DataSpaceAddress const& address);

  void             remove              (std::string const& name,
                                        DataSpace const& space=DataSpace());

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             rasterExists        (std::string const& name,
                                        DataSpace const& space=DataSpace()) const;

  bool             tableExists         (std::string const& name,
                                        DataSpace const& space=DataSpace()) const;

  MemoryRasterData raster              (std::string const& name,
                                        DataSpace const& space=DataSpace());

  MemoryTableData  table               (std::string const& name,
                                        DataSpace const& space=DataSpace());

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
