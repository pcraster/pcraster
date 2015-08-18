#ifndef INCLUDED_DAL_GSLIBBLOCKDRIVER
#include "dal_GSLIBBlockDriver.h"
#define INCLUDED_DAL_GSLIBBLOCKDRIVER
#endif

// Library headers.
#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif



/*!
  \file
  This file contains the implementation of the GSLIBBlockDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GSLIBBLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GSLIBBLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------

GSLIBBlockDriver::GSLIBBlockDriver()

  : BlockDriver(Format("GSLIB", "GSLIB block file format",
         BLOCK, Format::File, Format::Block, Format::Attribute)),
    TextFileDriver()

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Writer;

  std::vector<std::string> extensions;
  extensions.push_back(".gslib");
  format().setExtensions(extensions);
}



/* NOT IMPLEMENTED
//! Copy constructor.
GSLIBBlockDriver::GSLIBBlockDriver(
         GSLIBBlockDriver const& rhs)

  : Base(rhs)

{
}
*/



GSLIBBlockDriver::~GSLIBBlockDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
GSLIBBlockDriver& GSLIBBlockDriver::operator=(
         GSLIBBlockDriver const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



bool GSLIBBlockDriver::exists(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(false);
  return false;
}



Block* GSLIBBlockDriver::open(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */,
         TypeId /* typeId */) const
{
  assert(false);
  return 0;
}



Block* GSLIBBlockDriver::read(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */,
         TypeId /* typeId */) const
{
  assert(false);
  return 0;
}



template<typename T>
void GSLIBBlockDriver::write(
         Block const& block,
         std::ofstream& stream) const
{
  // Assumes/requires regular block.
  size_t nrVoxelsPerStack = block.cell<std::vector<T> >(0).size();

  std::vector<std::vector<T> const*> stacks(block.nrCells());
  size_t i;

  // Store pointers to stacks for efficiency.
  for(int row = block.nrRows() - 1; row >= 0; --row) {
    for(size_t col = 0; col < block.nrCols(); ++col) {
      i = row * block.nrCols() + col;
      stacks[i] = &block.cell<std::vector<T> >(i);
    }
  }

  for(size_t voxel = 0; voxel < nrVoxelsPerStack; ++voxel) {
    for(int row = block.nrRows() - 1; row >= 0; --row) {
      for(size_t col = 0; col < block.nrCols(); ++col) {
        i = row * block.nrCols() + col;
        if(pcr::isMV((*stacks[i])[voxel])) {
          stream << std::numeric_limits<T>::min() << '\n';
        }
        else {
          stream << (*stacks[i])[voxel] << '\n';
        }
      }
    }
  }
}



template<>
void GSLIBBlockDriver::write<UINT1>(
         Block const& block,
         std::ofstream& stream) const
{
  // Assumes/requires regular block.
  size_t nrVoxelsPerStack = block.cell<std::vector<UINT1> >(0).size();

  std::vector<std::vector<UINT1> const*> stacks(block.nrCells());
  size_t i;

  // Store pointers to stacks for efficiency.
  for(int row = block.nrRows() - 1; row >= 0; --row) {
    for(size_t col = 0; col < block.nrCols(); ++col) {
      i = row * block.nrCols() + col;
      stacks[i] = &block.cell<std::vector<UINT1> >(i);
    }
  }

  for(size_t voxel = 0; voxel < nrVoxelsPerStack; ++voxel) {
    for(int row = block.nrRows() - 1; row >= 0; --row) {
      for(size_t col = 0; col < block.nrCols(); ++col) {
        i = row * block.nrCols() + col;
        if(pcr::isMV((*stacks[i])[voxel])) {
          stream << 255 << '\n';
        }
        else {
          stream << INT4((*stacks[i])[voxel]) << '\n';
        }
      }
    }
  }
}



void GSLIBBlockDriver::write(
         Block const& block,
         boost::filesystem::path const& path) const
{
  assert(
         block.containsData());

  // A block which only contains data is assumed to be regular.
  // if(!block.isRegular()) {
  //   throwCannotWrite(path.string(), BLOCK,
  //        (boost::format("Driver %1% only supports regular blocks")
  //          % name()).str());
  // }

  std::ofstream stream;

  if(!TextFileDriver::open(stream, path)) {
    throwCannotBeOpened(path.string(), BLOCK);
  }

  stream <<
         "GSLIB file written by PCRaster BlockDriver\n"
         "1\n"
         "Attribute\n";

  switch(block.typeId()) {
    case(TI_UINT1_VECTOR): {
      write<UINT1>(block, stream);
      break;
    }
    case(TI_INT4_VECTOR): {
      write<INT4>(block, stream);
      break;
    }
    case(TI_REAL4_VECTOR): {
      write<REAL4>(block, stream);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



void GSLIBBlockDriver::write(
         Block const& block,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& name) const
{
  write(block, pathForDataSpaceAddress(name, space, address));
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

