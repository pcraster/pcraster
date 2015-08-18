#ifndef INCLUDED_DAL_MEMORYTABLEDRIVER
#include "dal_MemoryTableDriver.h"
#define INCLUDED_DAL_MEMORYTABLEDRIVER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_MEMORYDATAPOOL
#include "dal_MemoryDataPool.h"
#define INCLUDED_DAL_MEMORYDATAPOOL
#endif



/*!
  \file
  This file contains the implementation of the MemoryTableDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

MemoryTableDriver::MemoryTableDriver(
         MemoryDataPool* dataPool)

  : TableDriver(Format("memory", "Memory Table Driver",
         TABLE, Format::Memory)),
    d_dataPool(dataPool)

{
  assert(d_dataPool);

  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  properties |= Writer;
}



/* NOT IMPLEMENTED
//! Copy constructor.
MemoryTableDriver::MemoryTableDriver(
         MemoryTableDriver const& rhs)

  : Base(rhs)

{
}
*/



MemoryTableDriver::~MemoryTableDriver()
{
  // Data pools are for use only.
}



/* NOT IMPLEMENTED
//! Assignment operator.
MemoryTableDriver& MemoryTableDriver::operator=(
         MemoryTableDriver const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



bool MemoryTableDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return d_dataPool->tableExists(name, space) &&
         d_dataPool->table(name, space).exists(address);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      No need to copy the whole table, just header info is ok for now.
*/
Table* MemoryTableDriver::open(
         std::string const& name,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  Table* result = 0;

  if(d_dataPool->tableExists(name, space)) {
    Table const* table = d_dataPool->table(name, space).table(address);

    if(table) {
      result = new Table(*table);
    }
  }

  return result;
}



Table* MemoryTableDriver::read(
         std::string const& name,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  assert(space.isValid(address));

  Table* result = 0;

  if(d_dataPool->tableExists(name, space)) {
    Table const* table = d_dataPool->table(name, space).table(address);

    if(table) {
      // Create a copy to be returned.
      result = new Table(*table);
    }
  }

  if(!result) {
    throwCannotBeOpened(name, TABLE, space, address);
  }

  return result;
}



void MemoryTableDriver::read(
         Table& table,
         std::string const& name,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  assert(space.isValid(address));

  Table const* tmpTable = 0;

  if(d_dataPool->tableExists(name, space)) {
    tmpTable = d_dataPool->table(name, space).table(address);
  }

  if(!tmpTable) {
    throwCannotBeOpened(name, TABLE, space, address);
  }

  // Assign to table passed in.
  table = *tmpTable;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void MemoryTableDriver::write(
         Table const& table,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& name) const
{
  assert(space.isValid(address));

  // Create a MemoryTableData object to add to the pool. Put it at the record
  // for a table with the same name and space at the specified address.
  MemoryTableData data(space, address, new Table(table));

  // Add the data to the pool. The pool owns the data now. Previous contents
  // are removed first if needed.
  d_dataPool->add(name, data);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal




