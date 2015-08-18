#ifndef INCLUDED_DAL_BINARYTABLEDRIVER
#include "dal_BinaryTableDriver.h"
#define INCLUDED_DAL_BINARYTABLEDRIVER
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif



/*!
  \file
  This file contains the implementation of the BinaryTableDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BINARYTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BINARYTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

BinaryTableDriver::BinaryTableDriver()

  : TableDriver(Format("BinaryTable", "Binary table file format",
         TABLE, Format::File)),
    TextFileDriver()

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Writer;
}



/* NOT IMPLEMENTED
//! Copy constructor.
BinaryTableDriver::BinaryTableDriver(
         BinaryTableDriver const& rhs)

  : Base(rhs)

{
}
*/



BinaryTableDriver::~BinaryTableDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
BinaryTableDriver& BinaryTableDriver::operator=(
         BinaryTableDriver const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



bool BinaryTableDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return dal::exists(pathForDataSpaceAddress(name, space, address));
}



// Table* BinaryTableDriver::open(
//          std::string const& name,
//          DataSpace const& space,
//          DataSpaceAddress const& address) const
// {
// }



// Table* BinaryTableDriver::read(
//          std::string const& name,
//          DataSpace const& space,
//          DataSpaceAddress const& address) const
// {
// }



// void BinaryTableDriver::read(
//          Table& table,
//          std::string const& name,
//          DataSpace const& space,
//          DataSpaceAddress const& address) const
// {
// }



template<typename T>
void BinaryTableDriver::write(
         Table const& table,
         std::ofstream& stream) const
{
  assert(table.title().empty());
  assert(table.title(0).empty());
  assert(table.nrCols() == 1);

  Array<T> const& array(table.col<T>(0));
  T zero(0);

  for(size_t rec = 0; rec < array.size(); ++rec) {
    if(pcr::isMV(array[rec])) {
      stream.write((char const*)(&zero), sizeof(T));
    }
    else {
      stream.write((char const*)&(array[rec]), sizeof(T));
    }
  }
}



void BinaryTableDriver::write(
         Table const& table,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& name) const
{
  std::ofstream stream;
  boost::filesystem::path path(pathForDataSpaceAddress(name, space, address));

  if(!TextFileDriver::open(stream, path, std::ios::binary)) {
    throwCannotBeCreated(path.string(), TABLE);
  }

  switch(table.typeId(0)) {
    case TI_INT1: {
      write<INT1>(table, stream);
      break;
    }
    case TI_INT2: {
      write<INT2>(table, stream);
      break;
    }
    case TI_INT4: {
      write<INT4>(table, stream);
      break;
    }
    case TI_UINT1: {
      write<UINT1>(table, stream);
      break;
    }
    case TI_UINT2: {
      write<UINT2>(table, stream);
      break;
    }
    case TI_UINT4: {
      write<UINT4>(table, stream);
      break;
    }
    case TI_REAL4: {
      write<REAL4>(table, stream);
      break;
    }
    case TI_REAL8: {
      write<REAL8>(table, stream);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

