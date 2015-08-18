#ifndef INCLUDED_DAL_MEMORYDATAPOOL
#include "dal_MemoryDataPool.h"
#define INCLUDED_DAL_MEMORYDATAPOOL
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the MemoryDataPool class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYDATAPOOL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYDATAPOOL MEMBERS
//------------------------------------------------------------------------------

MemoryDataPool::MemoryDataPool()
{
}



MemoryDataPool::~MemoryDataPool()
{
  typedef std::multimap<std::string, MemoryTableData> Map;

  for(Map::iterator it = d_memoryTables.begin();
         it != d_memoryTables.end(); ++it) {
    it->second.clear();
  }
}



bool MemoryDataPool::rasterExists(
         std::string const& name,
         DataSpace const& space) const
{
  bool result = false;

  std::pair<
         std::multimap<std::string, MemoryRasterData>::const_iterator,
         std::multimap<std::string, MemoryRasterData>::const_iterator> range =
         d_memoryRasters.equal_range(name);

  while(range.first != range.second) {
    if((*range.first).second.dataSpace() == space) {
      result = true;
      break;
    }

    ++range.first;
  }

  return result;
}



bool MemoryDataPool::tableExists(
         std::string const& name,
         DataSpace const& space) const
{
  bool result = false;

  std::pair<
         std::multimap<std::string, MemoryTableData>::const_iterator,
         std::multimap<std::string, MemoryTableData>::const_iterator> range =
         d_memoryTables.equal_range(name);

  while(range.first != range.second) {
    if((*range.first).second.dataSpace() == space) {
      result = true;
      break;
    }

    ++range.first;
  }

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   A data item with name \a name and data space \a data.dataSpace()
             must not already be present in the pool. If it does, the
             properties (number of rows, type of the values, etc) must be the
             same.
  \sa        .
*/
void MemoryDataPool::add(
         std::string const& name,
         MemoryRasterData const& data)
{
  assert(!rasterExists(name, data.dataSpace()));

  d_memoryRasters.insert(
         std::map<std::string, MemoryRasterData>::value_type(name, data));
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   A data item with name \a name and data space \a data.dataSpace()
             must not already be present in the pool. If it does, the
             properties (number of rows, type of the values, etc) must be the
             same.
  \sa        .
*/
void MemoryDataPool::add(
         std::string const& name,
         MemoryTableData const& data)
{
  assert(!tableExists(name, data.dataSpace()));

  d_memoryTables.insert(
         std::map<std::string, MemoryTableData>::value_type(name, data));
}



//! Adds an raster data set item to an dataset stored at \a address.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void MemoryDataPool::add(
         std::string const& name,
         MemoryRasterData const& data,
         DataSpaceAddress const& address)
{
  raster(name, data.dataSpace()).add(data, address);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   A data item with name \a name and data space \a space must be
             present in the pool.
  \sa        .
*/
void MemoryDataPool::remove(
         std::string const& name,
         DataSpace const& space)
{
  assert(rasterExists(name, space));

  std::pair<std::multimap<std::string, MemoryRasterData>::iterator,
         std::multimap<std::string, MemoryRasterData>::iterator> range =
         d_memoryRasters.equal_range(name);

  while(range.first != range.second) {
    if((*range.first).second.dataSpace() == space) {
      d_memoryRasters.erase(range.first);
      break;
    }

    ++range.first;
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
MemoryRasterData MemoryDataPool::raster(
         std::string const& name,
         DataSpace const& space)
{
  MemoryRasterData* result = 0;

  std::pair<std::multimap<std::string, MemoryRasterData>::iterator,
         std::multimap<std::string, MemoryRasterData>::iterator> range =
         d_memoryRasters.equal_range(name);

  while(range.first != range.second) {
    if((*range.first).second.dataSpace() == space) {
      result = &(*range.first).second;
      break;
    }

    ++range.first;
  }

  assert(result);

  return *result;
}



MemoryTableData MemoryDataPool::table(
         std::string const& name,
         DataSpace const& space)
{
  MemoryTableData* result = 0;

  std::pair<std::multimap<std::string, MemoryTableData>::iterator,
         std::multimap<std::string, MemoryTableData>::iterator> range =
         d_memoryTables.equal_range(name);

  while(range.first != range.second) {
    if((*range.first).second.dataSpace() == space) {
      result = &(*range.first).second;
      break;
    }

    ++range.first;
  }

  assert(result);

  return *result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

