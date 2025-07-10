#include "dal_RasterDal.h"
#include "dal_CSFRasterDriver.h"
#include "dal_Dataset.h"
#include "dal_GDALRasterDriver.h"
#include "dal_Library.h"
#include "dal_MemoryRasterDriver.h"
#include "dal_Raster.h"
#include "dal_RasterDriver.h"
#include "dal_Utils.h"

#include "gdal_priv.h"


/*!
  \file
  This file contains the implementation of the RasterDal class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERDAL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTERDAL MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     addAllDrivers Whether or not to add all available drivers automatically.

  If \a addAllDrivers is false (the default case) no drivers are added
  to the object. In this case you need to explicitly add the drivers your
  application uses.
*/
RasterDal::RasterDal(
         bool addAllDrivers)

  : Dal(false)

{
  if(addAllDrivers) {
    #include "autoAddRasterDrivers.cc"
  }
}



//! Destructor.
/*!
*/
RasterDal::~RasterDal()
{
}



bool RasterDal::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(nrDrivers() > 0);

  return Dal::exists(name, space, address, RASTER);
}



//! Opens the raster dataset pointed to by \a name.
/*!
  \param     name Name of raster dataset.
  \return    A pointer to a newly created Raster object or 0 if no driver could open \a name.

  The caller is responsible of deleting the Raster object again.
*/
std::tuple<std::shared_ptr<Raster>, RasterDriver*> RasterDal::open(
         std::string const& name) const
{
  assert(nrDrivers() > 0);
  std::shared_ptr<Dataset> dataset;
  Driver* driver = nullptr;
  std::tie(dataset, driver) = Dal::open(name, RASTER);
  return std::make_tuple(std::dynamic_pointer_cast<Raster>(dataset),
      dynamic_cast<RasterDriver*>(driver));
}



std::tuple<std::shared_ptr<Raster>, RasterDriver*> RasterDal::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(nrDrivers() > 0);
  std::shared_ptr<Dataset> dataset;
  Driver* driver = nullptr;
  std::tie(dataset, driver) = Dal::open(name, space, address, RASTER);
  return std::make_tuple(std::dynamic_pointer_cast<Raster>(dataset),
      dynamic_cast<RasterDriver*>(driver));
}



//! Reads the raster dataset pointed to by \a name.
/*!
  \param     name Name of raster dataset.
  \param     typeId Type id of values to use.
  \return    A pointer to a newly created Raster object.
  \exception Exception If no driver could read \a name.
*/
std::shared_ptr<Raster> RasterDal::read(
         std::string const& name,
         TypeId typeId) const
{
  assert(nrDrivers() > 0);

  std::shared_ptr<Raster> raster;
  RasterDriver* driver = nullptr;
  std::tie(raster, driver) = open(name);

  if(!raster) {
    throwCannotBeOpened(name, RASTER);
  }

  assert(driver);

  return std::shared_ptr<Raster>(driver->read(name, typeId));
}



void RasterDal::read(
         Raster& raster,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  RasterDriver const* driver(driverByDataset(name, space));
  assert(driver); // TODO
  driver->read(raster, name, space, address);
}



void RasterDal::read(
         void* cell,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  RasterDriver const* driver(driverByDataset(name, space));
  assert(driver); // TODO
  driver->read(cell, typeId, name, space, address);
}



bool RasterDal::extremes(
         boost::any& min,
         boost::any& max,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space) const
{
  RasterDriver const* driver(driverByDataset(name, space));
  assert(driver); // TODO
  return driver->extremes(min, max, typeId, name, space);
}



RasterDriver* RasterDal::driverByDataset(
         std::string const& name,
         DataSpace const& space) const
{
  return dynamic_cast<RasterDriver*>(Dal::driverByDataset(name, space));
}



//! Returns the driver with name \a name.
/*!
  \param     name Name of driver to look for.
  \return    Pointer to the driver.
*/
RasterDriver* RasterDal::driverByName(
         std::string const& name)
{
  return dynamic_cast<RasterDriver*>(Dal::driverByName(name));
}

} // namespace dal

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



