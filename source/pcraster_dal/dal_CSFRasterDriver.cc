#ifndef INCLUDED_DAL_CSFRASTERDRIVER
#include "dal_CSFRasterDriver.h"
#define INCLUDED_DAL_CSFRASTERDRIVER
#endif

// Library headers.
#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_BOOST_LEXICAL_CAST
#include <boost/lexical_cast.hpp>
#define INCLUDED_BOOST_LEXICAL_CAST
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CSFMAP
#include "dal_CSFMap.h"
#define INCLUDED_DAL_CSFMAP
#endif

#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
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

#ifndef INCLUDED_DAL_PROPERTYKEYS
#include "dal_PropertyKeys.h"
#define INCLUDED_DAL_PROPERTYKEYS
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif

#ifndef INCLUDED_DAL_TYPE
#include "dal_Type.h"
#define INCLUDED_DAL_TYPE
#endif



/*!
  \file
  This file contains the implementation of the CSFRasterDriver class.
*/



namespace dal {

namespace detail {

void configureRaster(
         Raster& raster,
         CSFMap const& map)
{
  raster.setExtremes(map.min(), map.max());

  // Add CSF specific properties.
  raster.properties().setValue<REAL8>(DAL_CSF_ANGLE, map.angle());
  raster.properties().setValue<CSF_VS>(DAL_CSF_VALUESCALE, map.valueScale());
  raster.properties().setValue<CSF_PT>(DAL_CSF_PROJECTION, map.projectionType());

  if(map.hasLegend()) {
    raster.properties().setValue<Table>(DAL_LEGEND, map.legend());
  }
}

} // namespace detail

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CSFRASTERDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CSFRASTERDRIVER MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
*/
CSFRasterDriver::CSFRasterDriver()

  : RasterDriver(Format("CSF", "CSF-2.0 raster file format",
         RASTER, Format::File, Format::Attribute))

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  properties |= Writer;

  std::vector<std::string> extensions;
  extensions.push_back(".csf");
  extensions.push_back(".map");
  extensions.push_back(".pcrmap");
  format().setExtensions(extensions);
}



//! Destructor.
/*!
*/
CSFRasterDriver::~CSFRasterDriver()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
bool CSFRasterDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return dal::exists(this->pathFor(name, space, address));
}



//! Opens the raster pointed to by \a path.
/*!
  \param     path Pathname of the raster.
  \return    Pointer to newly allocated raster.
  \warning   The projection setting in the file will not be read.
  \sa        open(std::string const&)

  Always keep this function private, otherwise it will be picked up in favour
  of open(std::string const&, TypeId).
*/
Raster* CSFRasterDriver::open(
         boost::filesystem::path const& path,
         TypeId typeId) const
{
  Raster* raster = 0;

  try {
    CSFMap map(path);

    if(typeId != TI_NR_TYPES) {
      map.useAs(typeId);
    }

    raster = new Raster(
         map.nrRows(), map.nrCols(), map.cellSize(),
         map.west(), map.north(), map.useTypeId());
    detail::configureRaster(*raster, map);
  }
  catch(...) {
    // An error occured, we must make sure 0 is returned below.
    delete raster;
    raster = 0;
  }

  // 0 in case an error occured.
  return raster;
}



Raster* CSFRasterDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  assert(!space.hasSpace());

  Raster* result = 0;

  if(/* pathnameIsNative(name) && */ exists(name, space, address)) {
    result = open(this->pathFor(name, space, address), typeId);
  }

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  \todo      Add exceptions.
*/
void CSFRasterDriver::read(
         Raster& raster,
         CSFMap& map) const
{
  // TODO exceptions!
  assert(map.nrRows() == raster.nrRows());
  assert(map.nrCols() == raster.nrCols());

  detail::configureRaster(raster, map);

  if(!raster.cellsAreCreated()) {
    raster.createCells();
  }

  if(raster.typeId() == map.fileTypeId()) {
    // can copy into raster directly
    map.getCells(0, raster.nrCells(), raster.cells());
  }
  else {
    /* We need a read buffer so we are sure we have enough space
     * Rmalloc can do that for us.
     * TODO In the case that the file type is smaller than that of
     * raster we could reuse the raster.cells() space.
     */
    map.useAs(raster.typeId());
    void* buffer = map.malloc(raster.nrCells());

    try {
      map.getCells(0, raster.nrCells(), buffer);
    }
    catch(...) {
      free(buffer);
      throw;
    }

    Type const& t(Type::get(raster.typeId()));
    assert(t.hasTrivialCopy());
    std::memcpy(raster.cells(), buffer, t.size() * raster.nrCells());
    free(buffer);
  }

  // raster.properties().setValue<REAL8>(DAL_CSF_ANGLE, map.angle());
  // raster.properties().setValue<CSF_VS>(DAL_CSF_VALUESCALE, map.valueScale());
  // raster.properties().setValue<CSF_PT>(DAL_CSF_PROJECTION, map.projectionType());

  // if(map.hasLegend()) {
  //   raster.properties().setValue<Table>(DAL_LEGEND, map.legend());
  // }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Always keep this function private, otherwise it will be picked up in favour
  of read(Raster&, std::string const&).
*/
void CSFRasterDriver::read(
         Raster& raster,
         boost::filesystem::path const& path) const
{
  CSFMap map(path);
  read(raster, map);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Always keep this function private, otherwise it will be picked up in favour
  of read(std::string const&, TypeId).
*/
Raster* CSFRasterDriver::read(
         boost::filesystem::path const& path,
         TypeId typeId) const
{
  CSFMap map(path);

  if(typeId != TI_NR_TYPES) {
    map.useAs(typeId);
  }

  Raster* raster = new Raster(map.nrRows(), map.nrCols(), map.cellSize(),
    map.west(), map.north(), map.useTypeId());

  try {
    if(typeId != TI_NR_TYPES) {
      raster->setTypeId(typeId);
    }

    read(*raster, map);
  }
  catch(...) {
    delete raster;
    throw;
  }

  return raster;
}



Raster* CSFRasterDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  assert(!space.hasSpace());

  if(!exists(name, space, address)) {
    throwCannotBeOpened(name, RASTER, space, address);
  }

  return read(this->pathFor(name, space, address), typeId);
}



void CSFRasterDriver::read(
         Raster& raster,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(!space.hasSpace());

  if(!exists(name, space, address)) {
    throwCannotBeOpened(name, RASTER, space, address);
  }

  read(raster, this->pathFor(name, space, address));
}



void CSFRasterDriver::read(
         void* cell,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  if(!exists(name, space, address)) {
    throwCannotBeOpened(name, RASTER, space, address);
  }

  CSFMap map(this->pathFor(name, space, address));

  // Get x and y coordinate from address.
  // Convert x and y coordinates to row and col indices.
  assert(space.hasSpace());
  SpatialCoordinate const& spatialCoordinate(
         address.coordinate<SpatialCoordinate>(space.indexOf(Space)));

  double row, col;

  RasterDimensions dimensions(map.nrRows(), map.nrCols(), map.cellSize(),
         map.west(), map.north());
  dimensions.indices(spatialCoordinate.x(), spatialCoordinate.y(), row, col);

  assert(dal::greaterOrComparable(row, 0.0) &&
         dal::smallerOrComparable(row,
              static_cast<double>(dimensions.nrRows())));
  assert(dal::greaterOrComparable(col, 0.0) &&
         dal::smallerOrComparable(col,
              static_cast<double>(dimensions.nrCols())));

  // Read value from cell.
  map.useAs(typeId);
  map.getCell(static_cast<size_t>(row), static_cast<size_t>(col), cell);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Always keep this function private, otherwise it will be picked up in favour
  of write(std::string const&, Raster const&).
*/
void CSFRasterDriver::write(
         Raster const& raster,
         boost::filesystem::path const& path) const
{
  Properties const& p(raster.properties());
  REAL8 angle = p.value<REAL8>(DAL_CSF_ANGLE, 0.0);
  CSF_VS valueScale = p.value<CSF_VS>(DAL_CSF_VALUESCALE,
         typeIdToValueScale(raster.typeId()));
  CSF_PT projection = p.value<CSF_PT>(DAL_CSF_PROJECTION, PT_YDECT2B);

  CSFMap map(path, raster.nrRows(), raster.nrCols(),
         raster.west(), raster.north(), angle, raster.cellSize(),
         raster.typeId(), valueScale, projection);
  map.putCells(raster.cells());
}



void CSFRasterDriver::write(
         Raster const& raster,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& name) const
{
  write(raster, this->pathFor(name, space, address));
}



void CSFRasterDriver::browse(
         std::vector<BrowseInfo>& attributes,
         std::string const& location) const
{
  browseFileBasedRasterAttributes(attributes, boost::filesystem::path(
         location), PCRConvention | DALConvention);
}

} // namespace dal



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



