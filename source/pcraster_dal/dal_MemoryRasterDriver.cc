#ifndef INCLUDED_DAL_MEMORYRASTERDRIVER
#include "dal_MemoryRasterDriver.h"
#define INCLUDED_DAL_MEMORYRASTERDRIVER
#endif

// Library headers.
#include <boost/shared_ptr.hpp>

// PCRaster library headers.
#ifndef INCLUDED_DAL_MEMORYDATAPOOL
#include "dal_MemoryDataPool.h"
#define INCLUDED_DAL_MEMORYDATAPOOL
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the MemoryRasterDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYRASTERDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYRASTERDRIVER MEMBERS
//------------------------------------------------------------------------------

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   The caller is responsible for the \a dataPool being valid for as
             long as this driver lives.
  \sa        .
*/
MemoryRasterDriver::MemoryRasterDriver(
         MemoryDataPool* dataPool)

  : RasterDriver(Format("memory", "Memory Raster Driver",
         RASTER, Format::Memory, Format::Attribute)),
    d_dataPool(dataPool)

{
  assert(d_dataPool);

  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  // properties |= Writer;
}



MemoryRasterDriver::~MemoryRasterDriver()
{
  // Data pools are for use only.
}



bool MemoryRasterDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return d_dataPool->rasterExists(name, space) &&
         d_dataPool->raster(name, space).exists(address);
}



Raster* MemoryRasterDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  return d_dataPool->rasterExists(name, space)
         ? d_dataPool->raster(name, space).raster(address, typeId,
              MemoryRasterData::HeaderOnly)
         : 0;
}



Raster* MemoryRasterDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  assert(space.isValid(address));

  Raster* result = 0;

  if(d_dataPool->rasterExists(name, space)) {
    result = d_dataPool->raster(name, space).raster(address, typeId,
              MemoryRasterData::IncludingValues);
  }

  if(!result) {
    throwCannotBeOpened(name, RASTER, space, address);
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
  \todo      Handle situation where raster already has cells, see
             CSFRasterDriver. See dal_rasterdriver.h docs for this function.
  \todo      See implementation of read in MemoryTableDriver, copy strategy.
*/
void MemoryRasterDriver::read(
         Raster& raster,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  // Make sure the temp raster is deleted again.
  boost::shared_ptr<Raster> tmpRaster(read(name, space, address, TI_NR_TYPES));
  raster = *tmpRaster;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement.
*/
void MemoryRasterDriver::read(
         void* /* cell */,
         TypeId /* typeId */,
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(false);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement.
*/
void MemoryRasterDriver::write(
         Raster const& /* raster */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */,
         std::string const& /* name */) const
{
  /*
  assert(space.isValid(address));

  // Create a copy of the raster to write. Copy own the cells.
  boost::shared_ptr<Raster> raster(raster);

  // Create a MemoryRasterData object to add to the pool. Put it at the record
  // for a raster with the same name and space at the specified address.
  MemoryRasterData data(raster.cells(), space, typeId, nrRows, nrCols, cellSize,
       west, north);

  // Add the data to the pool. The pool owns the data now. Previous contents
  // with the same name and space are removed first.

  */


  assert(false);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

