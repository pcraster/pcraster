#ifndef INCLUDED_DAL_NETCDFRASTERDRIVER
#define INCLUDED_DAL_NETCDFRASTERDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_RASTERDRIVER
#include "dal_RasterDriver.h"
#define INCLUDED_DAL_RASTERDRIVER
#endif



class NcVar;
namespace dal {
  // NetCDFRasterDriver declarations.
}



namespace dal {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  Links to relevant information:
  * http://www.unidata.ucar.edu/software/netcdf
  * http://www.cgd.ucar.edu/cms/eaton/cf-metadata/CF-1.0.html (NetCDF Climate and Forecast (CF) Metadata Conventions)

  \todo Configure error handling in netcdf lib.

  - write a data set in a test
  - check the data set using a utility
  * create and test open
  * create and test dataSpace
  * create and test read
  * add support for more dimensions
*/
class NetCDFRasterDriver: public RasterDriver
{

  friend class NetCDFRasterDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  NetCDFRasterDriver& operator=        (NetCDFRasterDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   NetCDFRasterDriver  (NetCDFRasterDriver const& rhs);

  DataSpace        dataSpace           (std::string const& name) const;

  template<typename T>
  void             write               (Raster const& raster,
                                        NcVar* data) const;

  void             write               (Raster const& raster,
                                        NcVar* data) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   NetCDFRasterDriver  ();

  /* virtual */    ~NetCDFRasterDriver ();

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

  DataSpace        dataSpace           (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

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

  void             remove              (std::string const& name,
                                        DataSpace space) const;

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
