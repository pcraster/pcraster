#ifndef INCLUDED_DAL_MEMORYRASTERDRIVER
#define INCLUDED_DAL_MEMORYRASTERDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_RASTERDRIVER
#include "dal_RasterDriver.h"
#define INCLUDED_DAL_RASTERDRIVER
#endif



namespace dal {
  // MemoryRasterDriver declarations.
  class MemoryDataPool;
}



namespace dal {



//! RasterDriver class for rasters which are already loaded in memory.
/*!
  MemoryRasterDriver objects uses a MemoryDataPool object as the source or
  target of raster data.

  \todo      Once MemoryRasterData stores rasters instead of arrays, open()
             and read() can do the same thing.
  \todo      If we start using boost::shared_ptr everywhere, than open() and
             read() don't need to return a newly allocated object, but can
             return the object that is stored in memory.
*/
class MemoryRasterDriver: public RasterDriver
{

  friend class MemoryRasterDriverTest;

private:

  //! Data pool to store memory data in.
  MemoryDataPool* const d_dataPool;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MemoryRasterDriver  (MemoryDataPool* const dataPool);

  /* virtual */    ~MemoryRasterDriver ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Raster*          open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const;

  Raster*          read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const;

  void             read                (Raster& raster,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             write               (Raster const& raster,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const;

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
