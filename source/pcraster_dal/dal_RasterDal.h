#ifndef INCLUDED_DAL_RASTERDAL
#define INCLUDED_DAL_RASTERDAL



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif



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

  /* virtual */    ~RasterDal          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  boost::tuple<boost::shared_ptr<Raster>, RasterDriver*> open(
                                        std::string const& name) const;

  boost::tuple<boost::shared_ptr<Raster>, RasterDriver*> open(
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  boost::shared_ptr<Raster> read       (std::string const& name,
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

  bool             extremes            (boost::any& min,
                                        boost::any& max,
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
