#ifndef INCLUDED_DAL_RASTERDAL
#define INCLUDED_DAL_RASTERDAL

#include "dal_Configure.h"
#include "dal_Dal.h"



namespace dal {
  // RasterDal declarations.
  class Raster;
  class RasterDriver;
}



namespace dal {

//! This class represents the Data Abstraction Layer for raster datasets.
/*!
  Use an object of this class to read and write Raster datasets.

  \todo Use StackInfo to get at name of raster.
*/
class PCR_DAL_DECL RasterDal: public Dal
{

  friend class RasterDalTest;

private:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RasterDal           (bool addAllDrivers=false);

  /* virtual */    ~RasterDal          () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  std::tuple<std::shared_ptr<Raster>, RasterDriver*> open(
                                        std::string const& name) const;

  std::tuple<std::shared_ptr<Raster>, RasterDriver*> open(
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  std::shared_ptr<Raster> read       (std::string const& name,
                                        TypeId typeId = TI_NR_TYPES) const;

  void             read                (Raster& raster,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  bool             extremes            (std::any& min,
                                        std::any& max,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space) const;

  RasterDriver*    driverByDataset     (std::string const& name,
                                        DataSpace const& space) const;

  RasterDriver*    driverByName        (std::string const& name);

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
