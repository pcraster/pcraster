#ifndef INCLUDED_DAL_GDALRASTERDRIVER
#include "dal_GDALRasterDriver.h"
#define INCLUDED_DAL_GDALRASTERDRIVER
#endif

// Library headers.
#include<vector>

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

#include <boost/shared_ptr.hpp>

#ifndef INCLUDED_GDAL_PRIV
#include "gdal_priv.h"
#define INCLUDED_GDAL_PRIV
#endif

#ifndef INCLUDED_CPL_STRING
#include <cpl_string.h> // CSLFetchBoolean
#define INCLUDED_CPL_STRING
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif

#ifndef INCLUDED_DAL_DIMENSION
#include "dal_Dimension.h"
#define INCLUDED_DAL_DIMENSION
#endif

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif

#ifdef DEBUG_DEVELOP
  #ifndef INCLUDED_DAL_LIBRARY
  #include "dal_Library.h"
  #define INCLUDED_DAL_LIBRARY
  #endif
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif



/*!
  \file
  This file contains the implementation of the GDALRasterDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GDALRASTERDRIVER MEMBERS
//------------------------------------------------------------------------------

// size_t GDALRasterDriver::d_nrCreated = 0;

static TypeId typeId(
         GDALDataType dataType)
{
  TypeId typeId = TI_NR_TYPES;

  switch(dataType) {
    case GDT_Byte: {
      // Eight bit unsigned integer
      typeId = TI_UINT1;
      break;
    }
    case GDT_UInt16: {
      // Sixteen bit unsigned integer
      typeId = TI_UINT2;
      break;
    }
    case GDT_Int16: {
      // Sixteen bit signed integer
      typeId = TI_INT2;
      break;
    }
    case GDT_UInt32: {
      // Thirty two bit unsigned integer
      typeId = TI_UINT4;
      break;
    }
    case GDT_Int32: {
      // Thirty two bit signed integer
      typeId = TI_INT4;
      break;
    }
    case GDT_Float32: {
      // Thirty two bit floating point
      typeId = TI_REAL4;
      break;
    }
    case GDT_Float64: {
      // Sixty four bit floating point
      typeId = TI_REAL8;
      break;
    }
    case GDT_CInt16: {
      // Complex Int16
      // TODO Handle situation.
      assert(false);
      break;
    }
    case GDT_CInt32: {
      // Complex Int32
      // TODO Handle situation.
      assert(false);
      break;
    }
    case GDT_CFloat32: {
      // Complex Float32
      // TODO Handle situation.
      assert(false);
      break;
    }
    case GDT_CFloat64: {
      // Complex Float64
      // TODO Handle situation.
      assert(false);
      break;
    }
    default: {
      // TODO Handle situation.
      assert(false);
      break;
    }
  }

  return typeId;
}



static GDALDataType gdalDataType(
         TypeId typeId)
{
  GDALDataType gdalDataType = GDT_TypeCount;

  switch(typeId) {
    case TI_INT1: {
      // TODO Handle situation.
      assert(false);
      break;
    }
    case TI_INT2: {
      gdalDataType = GDT_Int16;
      break;
    }
    case TI_INT4: {
      gdalDataType = GDT_Int32;
      break;
    }
    case TI_UINT1: {
       gdalDataType = GDT_Byte;
       break;
     }
    case TI_UINT2: {
       gdalDataType = GDT_UInt16;
       break;
     }
    case TI_UINT4: {
       gdalDataType = GDT_UInt32;
       break;
     }
    case TI_REAL4: {
       gdalDataType = GDT_Float32;
       break;
     }
    case TI_REAL8: {
       gdalDataType = GDT_Float64;
       break;
     }
    default: {
      assert(false);
      break;
    }
  }

  return gdalDataType;
}



//! Determines value scale based on a string representation.
/*!
  \param     description String representation.
  \return    value scale, or VS_UNDEFINED if not recognized
  \warning   This function must be kept in sync with similar code in the
             PCRaster gdal driver. That driver writes these descriptions as
             meta data to the rasters.
*/
static CSF_VS valueScale(
         std::string const& description)
{
  CSF_VS result = VS_UNDEFINED;

  // CSF version2. -------------------------------------------------------------
  if(description == "VS_BOOLEAN") {
    result = VS_BOOLEAN;
  }
  else if(description == "VS_NOMINAL") {
    result = VS_NOMINAL;
  }
  else if(description == "VS_ORDINAL") {
    result = VS_ORDINAL;
  }
  else if(description == "VS_SCALAR") {
    result = VS_SCALAR;
  }
  else if(description == "VS_DIRECTION") {
    result = VS_DIRECTION;
  }
  else if(description == "VS_LDD") {
    result = VS_LDD;
  }
  // CSF version1. -------------------------------------------------------------
  else if(description == "VS_CLASSIFIED") {
    result = VS_CLASSIFIED;
  }
  else if(description == "VS_CONTINUOUS") {
    result = VS_CONTINUOUS;
  }
  else if(description == "VS_NOTDETERMINED") {
    result = VS_NOTDETERMINED;
  }

  return result;
}



static CSF_VS valueScale(
         GDALColorInterp colorInterpretation)
{
  CSF_VS result = VS_UNDEFINED;

  switch(colorInterpretation) {
    case GCI_GrayIndex: {
      result = VS_SCALAR;
      break;
    }
    default: {
      result = VS_UNDEFINED;
      break;
    }
  }

  return result;
}



RasterDimensions rasterDimensions(
         GDALDataset& gdalDataset)
{
  size_t nrRows = static_cast<size_t>(gdalDataset.GetRasterYSize());
  size_t nrCols = static_cast<size_t>(gdalDataset.GetRasterXSize());

  // Note that some formats don't support transformation to projection
  // coordinates. In those cases geoTransform is set to (0,1,0,0,0,1).
  double geoTransform[6];

  if(gdalDataset.GetGeoTransform(geoTransform) == CE_Failure) {
    // According to the docs at least, but doesn't seem to work (for png's) so
    // let's do it ourselves.
    geoTransform[0] = 0.0;
    geoTransform[1] = 1.0;
    geoTransform[2] = 0.0;
    geoTransform[3] = 0.0;
    geoTransform[4] = 0.0;
    geoTransform[5] = 1.0;
  }

  double cellWidth = std::fabs(geoTransform[1]);
  double cellHeight = std::fabs(geoTransform[5]);

  // We only support rasters with equal cell width and height.
  if(!dal::comparable(cellWidth, cellHeight)) {
    throw Exception(
         "Only raster with equal cell width and hight are supported");
  }

  double cellSize = cellWidth;
  double west = geoTransform[0];
  double north = geoTransform[3];

  return RasterDimensions(nrRows, nrCols, cellSize, west, north);
}


class DeleteGDALDataset
{

public:

    void operator()(GDALDataset* dataset)
    {
        GDALClose(dataset);
    }

};


Raster* GDALDataset2Raster(
  /* std::string const& name, */
  boost::shared_ptr<GDALDataset> gdalDataset,
  TypeId typeId)
{
  if(gdalDataset->GetRasterCount() < 1) {
    return 0;
  }

  GDALRasterBand* rasterBand = gdalDataset->GetRasterBand(1);
  assert(rasterBand);

  if(typeId == TI_NR_TYPES) {
    typeId = dal::typeId(rasterBand->GetRasterDataType());
  }

  Raster* raster = new Raster(rasterDimensions(*gdalDataset), typeId);

  int hasMinimum = 0;
  int hasMaximum = 0;
  double minimum = rasterBand->GetMinimum(&hasMinimum);
  double maximum = rasterBand->GetMaximum(&hasMaximum);

  if(hasMinimum && hasMaximum) {
    switch(typeId) {
      case TI_UINT1: {
        raster->setExtremes(boost::any(UINT1(minimum)),
              boost::any(UINT1(maximum)));
        break;
      }
      case TI_UINT2: {
        raster->setExtremes(boost::any(UINT2(minimum)),
              boost::any(UINT2(maximum)));
        break;
      }
      case TI_UINT4: {
        raster->setExtremes(boost::any(UINT4(minimum)),
              boost::any(UINT4(maximum)));
        break;
      }
      case TI_INT1: {
        raster->setExtremes(boost::any(INT1(minimum)),
              boost::any(INT1(maximum)));
        break;
      }
      case TI_INT2: {
        raster->setExtremes(boost::any(INT2(minimum)),
              boost::any(INT2(maximum)));
        break;
      }
      case TI_INT4: {
        raster->setExtremes(boost::any(INT4(minimum)),
              boost::any(INT4(maximum)));
        break;
      }
      case TI_REAL4: {
        raster->setExtremes(boost::any(REAL4(minimum)),
              boost::any(REAL4(maximum)));
        break;
      }
      case TI_REAL8: {
        raster->setExtremes(boost::any(REAL8(minimum)),
              boost::any(REAL8(maximum)));
        break;
      }
      default: {
        assert(false);
        break;
      }
    }

    assert(raster->hasExtremes());
  }

  // Determine value scale.
  CSF_VS valueScale = VS_UNDEFINED;

  {
    // 1. Based on meta data.
    char const* valueScaleDescription = gdalDataset->GetMetadataItem(
         "PCRASTER_VALUESCALE");

    if(valueScaleDescription) {
      valueScale = dal::valueScale(valueScaleDescription);
    }

    // 2. Based on color interpretation.
    if(valueScale == VS_UNDEFINED) {
      valueScale = dal::valueScale(rasterBand->GetColorInterpretation());
    }

    // 3. Based on type id.
    if(valueScale == VS_UNDEFINED) {
      valueScale = typeIdToValueScale(raster->typeId());
    }

    assert(valueScale != VS_UNDEFINED);
  }

  raster->properties().setValue<CSF_VS>(DAL_CSF_VALUESCALE, valueScale);

  return raster;
}



// static void GDALErrorHandler(CPLErr errorClass, int /* errorNr */,
//          const char* message) {
//   if(errorClass == CE_Failure || errorClass == CE_Fatal) {
//     throw Exception(message);
//   }
// }



std::vector<GDALDriver*> GDALRasterDriver::d_drivers;



GDALDataset* GDALRasterDriver::openGDALDataset(
         boost::filesystem::path const& path,
         GDALAccess access)
{
  GDALDataset* dataset = 0;

  // Installed a non-throwing error handler, see dal_Client.cc.
  dataset = static_cast<GDALDataset*>(GDALOpen(
         path.string().c_str(), access));

  if(!dataset) {
    throwCannotBeOpened(path.string(), RASTER);
  }

  assert(dataset);

  return dataset;
}



// GDALDataset* GDALRasterDriver::openGDALDataset(
//          std::string const& name,
//          DataSpace const& space,
//          DataSpaceAddress const& address,
//          GDALAccess access)
// {
//   GDALDataset* dataset = 0;
// 
//   // Installed a non-throwing error handler, see dal_Client.cc.
//   dataset = static_cast<GDALDataset*>(GDALOpen(
//          this->pathFor(name, space, address).
//          string().c_str(), access));
// 
//   if(!dataset) {
//     throwCannotBeOpened(name, RASTER, space, address);
//   }
// 
//   assert(dataset);
// 
//   return dataset;
// }



//! Registers all drivers currently known to GDAL.
/*!
  This function should be called after GDAL is configured and before the first
  use of the GDALRasterDriver class.
*/
void GDALRasterDriver::registerGDALDrivers()
{
  assert(d_drivers.empty());

  auto* manager = GetGDALDriverManager();

  for(int i = 0; i < manager->GetDriverCount(); ++i) {
    auto* driver = manager->GetDriver(i);
    auto metadata = driver->GetMetadata();

    if(CSLFetchBoolean(metadata, GDAL_DCAP_RASTER, FALSE)) {
      d_drivers.push_back(driver);
    }
  }
}


namespace detail {

std::vector<GDALDriver*> rasterDrivers()
{
  auto* manager = GetGDALDriverManager();
  std::vector<GDALDriver*> drivers;

  for(int i = 0; i < manager->GetDriverCount(); ++i) {
    auto* driver = manager->GetDriver(i);
    auto metadata = driver->GetMetadata();

    if(CSLFetchBoolean(metadata, GDAL_DCAP_RASTER, FALSE)) {
        drivers.emplace_back(driver);
    }
  }

  return drivers;
}


size_t rasterDriverCount()
{
    return rasterDrivers().size();
}


void deregisterGDALDrivers()
{
  // Deregister currently registered raster drivers.
  auto registeredDrivers = rasterDrivers();
  auto* manager = GetGDALDriverManager();

  for(auto* driver: registeredDrivers) {
    manager->DeregisterDriver(driver);
  }

  assert(rasterDriverCount() == 0);
}

} // namespace detail


void GDALRasterDriver::deregisterGDALDrivers()
{
  detail::deregisterGDALDrivers();

  // Remove drivers from memory.
  BOOST_FOREACH(GDALDriver* driver, d_drivers) {
    // Let GDal destroy the driver, prevent that a second heap manager assumes
    // the memory is from a different heap than what is used by the GDal dll.
    GDALDestroyDriver(driver);
  }

  d_drivers.clear();
}



//! Select a GDALDriver by name.
/*!
  \param     name Name of the GDALDriver to use.
  \return    driver or 0 if not found
*/
GDALDriver* GDALRasterDriver::driverByName(
         std::string const& name)
{
  assert(Library::isInitialised());

  GDALDriver* result = 0;

  for(size_t i = 0; i < d_drivers.size(); ++i) {
    if(d_drivers[i]->GetDescription() == name) {
      result = d_drivers[i];
      break;
    }
  }

  return result;
}



//! Returns whether a GDALDriver with name \a name is known.
/*!
  \param     name Name of the GDALDriver to test.
  \return    true or false
*/
bool GDALRasterDriver::driverIsAvailable(
         std::string const& name)
{
  return driverByName(name) != 0;
}



//! Returns an iterator to the first GDALDriver.
/*!
  \return    iterator
*/
GDALRasterDriver::iterator GDALRasterDriver::begin()
{
  assert(Library::isInitialised());

  return d_drivers.begin();
}



//! Returns an iterator to the one-past-the-last GDALDriver.
/*!
  \return    iterator
*/
GDALRasterDriver::iterator GDALRasterDriver::end()
{
  assert(Library::isInitialised());

  return d_drivers.end();
}



//------------------------------------------------------------------------------
// DEFINITION OF GDALRASTERDRIVER MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     name Name of GDAL driver to use.
  \exception Exception When a driver with name \a name is not available.

  Only the specific format supported by driver \a name is supported by this
  object.
*/
GDALRasterDriver::GDALRasterDriver(
         std::string const& name)

  : RasterDriver(Format(name, std::string("GDAL raster driver for ") + name,
         RASTER, Format::File, Format::Attribute)),
    d_driver(0)

{
  if(!driverIsAvailable(name)) {
    throw Exception((boost::format(
         "GDAL raster driver for %1%: Not available")
         % name).str());
  }

  d_driver = driverByName(name);
  assert(d_driver);

  init();
}



//! Constructor.
/*!
  \param     driver Specific GDAL driver to use.
  \warning   The driver passed in must be part of the collection registered
             during the call to registerGDALDrivers().

  Only the specific format supported by \a driver is supported by this object.
*/
GDALRasterDriver::GDALRasterDriver(
         GDALDriver* driver)

  : RasterDriver(Format(driver->GetDescription(),
         std::string("GDAL raster driver for ") + driver->GetDescription(),
         RASTER, Format::File, Format::Attribute)),
    d_driver(driver)

{
  assert(std::find(d_drivers.begin(), d_drivers.end(), driver) !=
         d_drivers.end());
  init();
}



void GDALRasterDriver::init()
{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;

  char** metadata = d_driver->GetMetadata();

  if(CSLFetchBoolean(metadata, GDAL_DCAP_CREATECOPY, FALSE)) {
    properties |= Writer;
  }

  std::vector<std::string> extensions;

  if(name() == "AAIGrid") {
    extensions.push_back(".asc");
  }
  else if(name() == "BMP") {
    extensions.push_back(".bmp");
  }
  else if(name() == "Erdas Imagine Images (.img)") {
    extensions.push_back(".img");
  }
  else if(name() == "GIF") {
    extensions.push_back(".gif");
  }
  else if(name() == "GTiff") {
    extensions.push_back(".tif");
    extensions.push_back(".tiff");
  }
  else if(name() == "HDF4") {
    extensions.push_back(".hdf");
    extensions.push_back(".hdf4");
  }
  else if(name() == "HDF4Image") {
    extensions.push_back(".hdf");
  }
  else if(name() == "ILWIS") {
    extensions.push_back(".mpl");
    extensions.push_back(".mpr");
  }
  else if(name() == "JPEG") {
    extensions.push_back(".jpg");
  }
  else if(name() == "PCRASTER") {
    extensions.push_back(".csf");
    extensions.push_back(".map");
    extensions.push_back(".pcr");
  }
  else if(name() == "PNM") {
    extensions.push_back(".pnm");
  }
  else if(name() == "WCS" || name() == "WMS") {
    extensions.push_back(".xml");
  }

  format().setExtensions(extensions);
}



/* NOT IMPLEMENTED
//! Copy constructor.
GDALRasterDriver::GDALRasterDriver(GDALRasterDriver const& rhs)

  : Base(rhs)

{
}
*/



//! Destructor.
/*!
*/
GDALRasterDriver::~GDALRasterDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
GDALRasterDriver& GDALRasterDriver::operator=(GDALRasterDriver const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



//! Returns whether raster \a name exists in \a space at \a address.
/*!
  \param     name Name of raster to check.
  \param     space Dataspace to search in.
  \param     address Address to check.
  \return    true or false
*/
bool GDALRasterDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return dal::exists(this->pathFor(name, space, address));
}



//! Configures the GDALDriverManager object to contain only the layered driver.
/*!
  \warning   The manager only contains the layered driver after calling this
             function, previous manager settings are lost.

  Some drivers depend on other drivers to work correctly. These will be added
  if needed.
*/
void GDALRasterDriver::registerGDALDriverToUse() const
{
  assert(d_driver);

  detail::deregisterGDALDrivers();

  // Register driver.
  GDALDriverManager* manager = GetGDALDriverManager();
  manager->RegisterDriver(d_driver);

  // Hack for HDF4Image driver. It seems it depends on the HDF4 driver also
  // being registered.
  if(std::string(d_driver->GetDescription()) == "HDF4Image") {
    for(size_t i = 0; i < d_drivers.size(); ++i) {
      if(std::string(d_drivers[i]->GetDescription()) == "HDF4") {
        manager->RegisterDriver(d_drivers[i]);
        break;
      }
    }

    assert(detail::rasterDriverCount() == 2);
  }
  // The WCS driver depends on VRT and format drivers. The data is stored in
  // memory in a certain format. Currently we only add the GeoTiff driver
  // here. This could be the full list of format drivers though, except for
  // WCS itself.
  else if(std::string(d_driver->GetDescription()) == "WCS") {
    for(size_t i = 0; i < d_drivers.size(); ++i) {
      if(std::string(d_drivers[i]->GetDescription()) == "VRT") {
        manager->RegisterDriver(d_drivers[i]);
      }
      else if(std::string(d_drivers[i]->GetDescription()) == "GTiff") {
        manager->RegisterDriver(d_drivers[i]);
      }
    }

    assert(detail::rasterDriverCount() == 3);
  }
#ifdef DEBUG_DEVELOP
  else {
    assert(detail::rasterDriverCount() == 1);
  }
#endif
}



Raster* GDALRasterDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  assert(!space.hasSpace());

  Raster* raster = 0;

  try {
    registerGDALDriverToUse();
    boost::shared_ptr<GDALDataset> gdalDataset(openGDALDataset(
         this->pathFor(name, space, address), GA_ReadOnly), DeleteGDALDataset());
    raster = GDALDataset2Raster(gdalDataset, typeId);
  }
  catch(Exception& /* exception */) {
    assert(raster == 0);
  }

  return raster;
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Get rid of the calculation of the extremes from this function.
             See TODO in the code.
*/
void GDALRasterDriver::read(
         Raster& raster,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(!space.hasSpace());

  registerGDALDriverToUse();
  boost::shared_ptr<GDALDataset> gdalDataset(openGDALDataset(
         this->pathFor(name, space, address), GA_ReadOnly),
             DeleteGDALDataset());

  assert(gdalDataset->GetRasterCount() >= 1);
  GDALRasterBand* rasterBand = gdalDataset->GetRasterBand(1);
  assert(rasterBand);

  if(!rasterBand) {
    throwCannotBeOpened(name, RASTER, space, address);
  }

  if(!raster.cellsAreCreated()) {
    raster.createCells();
  }

  if(rasterBand->RasterIO(GF_Read, 0, 0, raster.nrCols(), raster.nrRows(),
      raster.cells(), raster.nrCols(), raster.nrRows(),
      gdalDataType(raster.typeId()), 0, 0) != CE_None) {
    throwCannotReadCells(name, RASTER, space, address);
  }

  int hasNoDataValue = 0;
  double noDataValue = rasterBand->GetNoDataValue(&hasNoDataValue);

  if(hasNoDataValue) {
    toStdMV(raster.typeId(), raster.cells(), raster.nrCells(), noDataValue);
  }

  // TODO setExtremes -> calculateExtremes. Don't do this for every raster
  // read, but only when it is necessary. That's up to the client code.
  // If the extremes were not read from a header determine them from the
  // values.
  if(!raster.hasExtremes()) {
    raster.setExtremes();
  }
}



Raster* GDALRasterDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  assert(!space.hasSpace());

  Raster* raster = open(name, space, address, typeId);

  if(!raster) {
    throwCannotBeOpened(name, RASTER, space, address);
  }

  read(*raster, name, space, address);

  return raster;
}



/// void GDALRasterDriver::read(
///          void* cell,
///          TypeId typeId,
///          std::string const& name,
///          size_t row,
///          size_t col) const
/// {
///   registerGDALDriverToUse();
///   boost::shared_ptr<GDALDataset> gdalDataset(openGDALDataset(name,
///          GA_ReadOnly), DeleteGDALDataset());
/// 
///   assert(gdalDataset->GetRasterCount() >= 1);
///   GDALRasterBand* rasterBand = gdalDataset->GetRasterBand(1);
///   assert(rasterBand);
/// 
///   if(!rasterBand) {
///     throwCannotBeOpened(name, RASTER);
///   }
/// 
///   // Read value.
///   if(rasterBand->RasterIO(GF_Read, col, row, 1, 1,
///       cell, 1, 1,
///       gdalDataType(typeId), 0, 0) != CE_None) {
///     throwCannotReadCell(name, RASTER);
///   }
/// 
///   int hasNoDataValue = 0;
///   double noDataValue = rasterBand->GetNoDataValue(&hasNoDataValue);
/// 
///   if(hasNoDataValue) {
///     toStdMV(typeId, cell, 1, noDataValue);
///   }
/// }



void GDALRasterDriver::read(
         void* cell,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(space.hasSpace());

  registerGDALDriverToUse();
  boost::shared_ptr<GDALDataset> gdalDataset(
         openGDALDataset(this->pathFor(name, space, address), GA_ReadOnly),
              DeleteGDALDataset());

  assert(gdalDataset->GetRasterCount() >= 1);
  GDALRasterBand* rasterBand = gdalDataset->GetRasterBand(1);
  assert(rasterBand);

  if(!rasterBand) {
    throwCannotBeOpened(name, RASTER, space, address);
  }

  // Get x and y coordinate from address.
  // Convert x and y coordinates to row and col indices.
  assert(space.hasSpace());
  SpatialCoordinate const& spatialCoordinate(
         address.coordinate<SpatialCoordinate>(space.indexOf(Space)));

  double row, col;

  RasterDimensions dimensions(rasterDimensions(*gdalDataset));
  dimensions.indices(spatialCoordinate.x(), spatialCoordinate.y(), row, col);

  assert(dal::greaterOrComparable(row, 0.0) &&
         dal::smallerOrComparable(row,
              static_cast<double>(dimensions.nrRows())));
  assert(dal::greaterOrComparable(col, 0.0) &&
         dal::smallerOrComparable(col,
              static_cast<double>(dimensions.nrCols())));

  // Read value.
  if(rasterBand->RasterIO(GF_Read, static_cast<int>(col),
         static_cast<int>(row), 1, 1,
      cell, 1, 1,
      gdalDataType(typeId), 0, 0) != CE_None) {
    throwCannotReadCell(name, RASTER, space, address);
  }

  int hasNoDataValue = 0;
  double noDataValue = rasterBand->GetNoDataValue(&hasNoDataValue);

  if(hasNoDataValue) {
    toStdMV(typeId, cell, 1, noDataValue);
  }
}



//! Writes \a raster to file \a name.
/*!
  \param     name Name of raster file to write.
  \param     raster Raster object to write from.
  \exception Exception If something goes wrong while opening the file or
                       writing the values.

  \todo      Add support for space and address, refactor with CSFRasterDriver.
  \todo      Not every driver can write.
*/
void GDALRasterDriver::write(
         Raster const& raster,
         DataSpace const&
#ifdef DEBUG_DEVELOP
         space
#endif
         ,
         DataSpaceAddress const& /* address */,
         std::string const& name) const
{
  assert(space.isEmpty());

  boost::filesystem::path path(dal::pathFor(name));

  // FROM api Tutorial

  // needs this for BIL format: ENVI
  // const char *pszFormat = "EHdr";
  // const char *pszFormat = "EHdr";

  registerGDALDriverToUse();

#ifdef DEBUG_DEVELOP
  char **papszMetadata = d_driver->GetMetadata();
  assert(CSLFetchBoolean(papszMetadata, GDAL_DCAP_CREATE, TRUE));
#endif

  // d_driver = GetGDALDriverManager()->GetDriverByName(pszFormat);


  /* from API Tutorial: All drivers that support creating new files support the CreateCopy() method, but only a few support the Create() method.
   * seems not true, Create is supported but CreateCopy not!
   * if( CSLFetchBoolean( papszMetadata, GDAL_DCAP_CREATE, FALSE ) )
   *           fprintf(stderr, "Driver %s supports Create() method.\n", pszFormat );
   *  if( CSLFetchBoolean( papszMetadata, GDAL_DCAP_CREATECOPY, FALSE ) )
   *           fprintf(stderr, "Driver %s supports CreateCopy() method.\n", pszFormat );
   * const char *pszFormat = "EHdr"; hmm. no create documented
   */

   char **papszOptions = NULL;

   boost::shared_ptr<GDALDataset> poDstDS(d_driver->Create(name.c_str(),
       raster.nrCols(), raster.nrRows(), 1, gdalDataType(raster.typeId()),
           papszOptions), DeleteGDALDataset());
   assert(poDstDS);
/*
 * double adfGeoTransform[6] = { 444720, 30, 0, 3751320, 0, -30 };
 * OGRSpatialReference oSRS;
 * char *pszSRS_WKT = NULL;
 * poDstDS->SetGeoTransform( adfGeoTransform );
 * oSRS.SetUTM( 11, TRUE );
 * oSRS.SetWellKnownGeogCS( "NAD27" );
 * oSRS.exportToWkt( &pszSRS_WKT );
 * poDstDS->SetProjection( pszSRS_WKT );
 * CPLFree( pszSRS_WKT );
 */
   GDALRasterBand *poBand=poDstDS->GetRasterBand(1);
   poBand->RasterIO(GF_Write,
       0, 0,
       raster.nrCols(), raster.nrRows(),
       const_cast<void *>(raster.cells()),
       raster.nrCols(), raster.nrRows(),
       gdalDataType(raster.typeId()), 0, 0 );

/*
 * CSFMap map(path, raster.nrRows(), raster.nrCols(),
 *        raster.west(), raster.north(), raster.angle(), raster.cellSize(),
 *        raster.typeId(), raster.valueScale());
 * map.putCells(raster.cells());
*/
}



void GDALRasterDriver::browse(
         std::vector<BrowseInfo>& attributes,
         std::string const& location) const
{
  // TODO If this driver is a remote driver (WCS), it should not be used(?).
  browseFileBasedRasterAttributes(attributes, boost::filesystem::path(
         location), DALConvention);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

