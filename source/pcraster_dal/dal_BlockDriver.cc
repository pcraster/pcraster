#include "dal_BlockDriver.h"



/*!
  \file
  This file contains the implementation of the BlockDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------

BlockDriver::BlockDriver(
         Format const& format)

  : Driver(format)

{
}



/* NOT IMPLEMENTED
//! Copy constructor.
BlockDriver::BlockDriver(
         BlockDriver const& rhs)

  : Base(rhs)

{
}
*/



BlockDriver::~BlockDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
BlockDriver& BlockDriver::operator=(
         BlockDriver const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



Block* BlockDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(!space.hasSpace());

  return open(name, space, address, TI_NR_TYPES);
}



DataSpace BlockDriver::dataSpace(
         std::string const& name) const
{
  return dataSpace(name, DataSpace(), DataSpaceAddress());
}



DataSpace BlockDriver::dataSpace(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(!space.hasSpace());

  std::shared_ptr<Block> const block(open(name, space, address));

  if(!block) {
    throwCannotBeOpened(name, BLOCK, space, address);
  }

  std::vector<std::any> rows;
  std::vector<std::any> cols;
  rows.push_back(size_t(1));
  rows.push_back(block->nrRows());
  rows.push_back(size_t(1));
  cols.push_back(size_t(1));
  cols.push_back(block->nrCols());
  cols.push_back(size_t(1));

  DataSpace const rasterSpace;
  assert(false); // Update to use RasterDimensions, see elsewhere.
  // rasterSpace.addDimension(Dimension(NumericalCoordinates, Space,
  //        RegularDiscretisation, rows));
  // rasterSpace.addDimension(Dimension(NumericalCoordinates, Space,
  //        RegularDiscretisation, cols));
  // TODO add 3D information
  assert(false);

  return rasterSpace;
}



Block* BlockDriver::read(
         std::string const& name) const
{
  return read(name, DataSpace(), DataSpaceAddress(), TI_NR_TYPES);
}



Block* BlockDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return read(name, space, address, TI_NR_TYPES);
}



void BlockDriver::write(
         Block const& block,
         std::string const& name) const
{
  write(block, DataSpace(), DataSpaceAddress(), name);
}



void BlockDriver::read(
         void* cell,
         TypeId
#ifdef DEBUG_BUILD
           typeId
#endif
         ,
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(typeId == TI_REAL4);
  pcr::setMV(*static_cast<REAL4*>(cell));
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

