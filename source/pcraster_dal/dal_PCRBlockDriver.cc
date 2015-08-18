#ifndef INCLUDED_DAL_PCRBLOCKDRIVER
#include "dal_PCRBlockDriver.h"
#define INCLUDED_DAL_PCRBLOCKDRIVER
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
  This file contains the implementation of the PCRBlockDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PCRBLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------

char PCRBlockDriver::d_magicString[] = "PCRaster Block Format";

size_t const PCRBlockDriver::d_lengthOfMagicString = 21;



//------------------------------------------------------------------------------
// DEFINITION OF PCRBLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------

PCRBlockDriver::PCRBlockDriver()

  : BlockDriver(Format("PCRBlock", "PCRaster block file format",
         BLOCK, Format::File, Format::Block, Format::Attribute)),
    TextFileDriver()

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  properties |= Writer;

  std::vector<std::string> extensions;
  extensions.push_back(".pcrblock");
  format().setExtensions(extensions);
}



/* NOT IMPLEMENTED
//! Copy constructor.
PCRBlockDriver::PCRBlockDriver(
         PCRBlockDriver const& rhs)

  : Base(rhs)

{
}
*/



PCRBlockDriver::~PCRBlockDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
PCRBlockDriver& PCRBlockDriver::operator=(
         PCRBlockDriver const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
bool PCRBlockDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return dal::exists(dal::pathForDataSpaceAddress(name, space, address));
}



Block* PCRBlockDriver::open(
         std::ifstream& stream,
         TypeId
#ifdef DEBUG_DEVELOP
         typeId
#endif
         ) const
{
  Block* result(0);

  char magicString[d_lengthOfMagicString + 1];
  stream.read(magicString, d_lengthOfMagicString);
  magicString[d_lengthOfMagicString] = '\0';

  if(stream.good() && std::strcmp(magicString, d_magicString) == 0) {
    UINT4 contentType;
    UINT4 nrRows, nrCols;
    stream.read((char*)&contentType, 4);
    stream.read((char*)&nrRows, 4);
    stream.read((char*)&nrCols, 4);

    if(contentType == 0) {
      // File contains discretisation information.
      REAL8 cellSize, west, north;
      stream.read((char*)&cellSize, 8);
      stream.read((char*)&west, 8);
      stream.read((char*)&north, 8);

      if(stream.good()) {
        result = new Block(nrRows, nrCols, cellSize, west, north);
        assert(result->containsDiscretisationInfo());
      }
    }
    else if(contentType == 1) {
      // File contains data.
      TypeId readTypeId;
      stream.read((char*)&readTypeId, sizeof(readTypeId));

      if(stream.good()) {
        assert(typeId == TI_NR_TYPES || readTypeId == typeId);
        assert(
              readTypeId == TI_UINT1_VECTOR ||
              readTypeId == TI_INT4_VECTOR ||
              readTypeId == TI_REAL4_VECTOR);
        result = new Block(nrRows, nrCols, readTypeId);
        assert(result->containsData());
      }
    }
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
*/
Block* PCRBlockDriver::open(
         boost::filesystem::path const& path,
         TypeId typeId) const
{
  Block* result(0);

  std::ifstream stream;

  if(TextFileDriver::open(stream, path), std::ios::binary) {
    result = open(stream, typeId);
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
*/
Block* PCRBlockDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  return open(dal::pathForDataSpaceAddress(name, space, address), typeId);
}



void PCRBlockDriver::readThicknesses(
         std::ifstream& stream,
         Block& block) const
{
  for(size_t i = 0; i < block.nrCells(); ++i) {
    stream.read((char*)&(block.baseElevation()->cell<REAL4>(i)), 4);

    if(!pcr::isMV(block.baseElevation()->cell<REAL4>(i))) {
      readVoxels<REAL4>(stream, block.cell<std::vector<REAL4> >(i));
    }
  }
}



Block* PCRBlockDriver::read(
         boost::filesystem::path const& path,
         TypeId typeId) const
{
  std::ifstream stream;

  if(!TextFileDriver::open(stream, path, std::ios::binary)) {
    // File cannot be opened for reading.
    throwCannotBeOpened(path.string(), BLOCK);
  }

  Block* result(open(stream, typeId));

  if(!result) {
    // File does not contain block stuff.
    throwCannotBeOpened(path.string(), BLOCK,
         "does not contain block discretisation or data");
  }

  assert(stream.good());
  result->createCells();

  if(result->containsDiscretisationInfo()) {
    // Read discretisation info.
    result->baseElevation()->createCells();
    readThicknesses(stream, *result);
  }
  else {
    // Read data.
    switch(result->typeId()) {
      case TI_UINT1_VECTOR: {
        readVoxels<UINT1>(stream, *result);
        break;
      }
      case TI_INT4_VECTOR: {
        readVoxels<INT4>(stream, *result);
        break;
      }
      case TI_REAL4_VECTOR: {
        readVoxels<REAL4>(stream, *result);
        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

  if(stream.eof()) {
    throwCannotBeRead(path.string(), BLOCK, "end of file reached");
  }
  else if(!stream.good()) {
    throwCannotBeRead(path.string(), BLOCK);
  }

  assert(stream.good());

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
Block* PCRBlockDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  return read(dal::pathForDataSpaceAddress(name, space, address), typeId);
}



void PCRBlockDriver::writeThicknesses(
         Block const& block,
         std::ofstream& stream) const
{
  for(size_t i = 0; i < block.nrCells(); ++i) {
    stream.write((char const*)&(block.baseElevation()->cell<REAL4>(i)), 4);

    if(!pcr::isMV(block.baseElevation()->cell<REAL4>(i))) {
      writeVoxels<REAL4>(block.cell<std::vector<REAL4> >(i), stream);
    }
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Rewrite raster class, replace size_t with UINT4. size_t is
             platform dependent.
  \todo      Make sure type id's have a fixed value!
*/
void PCRBlockDriver::write(
         Block const& block,
         boost::filesystem::path const& path) const
{
  std::ofstream stream;

  if(!TextFileDriver::open(stream, path, std::ios::binary)) {
    throwCannotBeCreated(path.string(), BLOCK);
  }

  UINT4 contentType = block.containsDiscretisationInfo() ? 0 : 1;
  UINT4 nrRows(block.nrRows());
  UINT4 nrCols(block.nrCols());
  REAL8 cellSize(block.cellSize());
  REAL8 west(block.west());
  REAL8 north(block.north());

  stream.write(d_magicString, d_lengthOfMagicString);
  stream.write((char*)&contentType, sizeof(contentType));
  stream.write((char*)&nrRows, sizeof(nrRows));
  stream.write((char*)&nrCols, sizeof(nrCols));

  if(block.containsDiscretisationInfo()) {
    // Block contains discretisation information.
    stream.write((char*)&cellSize, sizeof(cellSize));
    stream.write((char*)&west, sizeof(west));
    stream.write((char*)&north, sizeof(north));
    writeThicknesses(block, stream);
  }
  else {
    // Block contains data.
    TypeId typeId(block.typeId());
    stream.write((char*)&typeId, sizeof(typeId));

    switch(block.typeId()) {
      case TI_UINT1_VECTOR: {
        writeVoxels<UINT1>(block, stream);
        break;
      }
      case TI_INT4_VECTOR: {
        writeVoxels<INT4>(block, stream);
        break;
      }
      case TI_REAL4_VECTOR: {
        writeVoxels<REAL4>(block, stream);
        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

  if(!stream.good()) {
    throwCannotWrite(path.string(), BLOCK);
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
void PCRBlockDriver::write(
         Block const& block,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& name) const
{
  write(block, dal::pathForDataSpaceAddress(name, space, address));
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

