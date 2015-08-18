#ifndef INCLUDED_DAL_GDALRASTERDRIVER
#define INCLUDED_DAL_GDALRASTERDRIVER



// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>
#define INCLUDED_BOOST_FILESYSTEM
#endif

#ifndef INCLUDED_GDAL
#include <gdal.h>
#define INCLUDED_GDAL
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif

#ifndef INCLUDED_DAL_RASTERDRIVER
#include "dal_RasterDriver.h"
#define INCLUDED_DAL_RASTERDRIVER
#endif



class GDALDataset;
class GDALDriver;
namespace dal {
  // GDALRasterDriver declarations.
}

namespace dal {

//! Raster driver which uses the format drivers from the GDAL library.
/*!
  GDAL is a translator library for raster geospatial data formats. For more
  information see http://www.gdal.org

  This driver enables the use of all formats supported by GDAL.

  GDALDriver: drivers from the GDAL library.

  GDALRasterDriver: this class!

  \todo How will the GDALDriver drivers be destructed?

  \code
  hints om het grote aantal open bestanden op te lossen, lijkt een gdal
  (gebruik) fout te zijn

  registerGDALDriverToUse is een boosdoener. Al ik hem nix laat doen, blijven
  de .bil .asc en ESRI grid binaries niet openstaan en werken.
  Alleen main.cc en emptyFile heeft dan nog een probleem, swa.

  GDALDestroyDriverManager gebruiken?
  \endcode

  \todo Treats all raster formats as file based and attribute type. Not the
        case for all formats.
*/
class PCR_DAL_DECL GDALRasterDriver: public RasterDriver
{

  friend class GDALRasterDriverTest;

private:

  //! GDAL drivers.
  static std::vector<GDALDriver*> d_drivers;

  // GDAL raster driver to use.
  GDALDriver*      d_driver;

  // static size_t d_nrCreated;

  //! Assignment operator. NOT IMPLEMENTED.
  GDALRasterDriver& operator=          (GDALRasterDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   GDALRasterDriver    (GDALRasterDriver const& rhs);

  static GDALDataset* openGDALDataset  (boost::filesystem::path const& path,
                                        GDALAccess access);

  // static GDALDataset* openGDALDataset  (std::string const& name,
  //                                       DataSpace const& space,
  //                                       DataSpaceAddress const& address,
  //                                       GDALAccess access);

  static GDALDriver* driverByName      (std::string const& name);

  void             registerGDALDriverToUse() const;

  void             init                ();

public:

  static void      registerGDALDrivers ();

  static void      deregisterGDALDrivers();

  static bool      driverIsAvailable   (std::string const& name);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GDALRasterDriver    (std::string const& name);

                   GDALRasterDriver    (GDALDriver* driver);

  /* virtual */    ~GDALRasterDriver   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  typedef std::vector<GDALDriver*>::iterator iterator;

  static iterator  begin               ();

  static iterator  end                 ();

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Raster*          open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const;

  void             read                (Raster& raster,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  /// void             read                (void* cell,
  ///                                       TypeId typeId,
  ///                                       std::string const& name,
  ///                                       size_t row,
  ///                                       size_t col) const;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Raster*          read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const;

  void             write               (Raster const& raster,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const;

  void             browse              (std::vector<BrowseInfo>& attributes,
                                        std::string const& location) const;

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
