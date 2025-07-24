#include "stddefx.h"
#include "calc_nullcompressor.h"
#include "calc_decompresseddata.h"
#include "calc_spatial.h"
#include "calc_compressioninput.h"



/*!
  \file
  This file contains the implementation of the NullCompressor class.
*/



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC NULLCOMPRESSOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF NULLCOMPRESSOR MEMBERS
//------------------------------------------------------------------------------

calc::NullCompressor::NullCompressor(const geo::RasterSpace& rs):
  Compressor(rs)
{
}



calc::NullCompressor::~NullCompressor()
{
}

void calc::NullCompressor::decompress(
    DecompressedData& d,
    const void *decompressedData) const
{
  d.setOriginalData(decompressedData);
}

calc::Spatial *calc::NullCompressor::createSpatial(CompressionInput& ci) const
{
  // decompressed is equal to compressed, just return the decompressed data
  return new Spatial(ci.vs(),rasterSpace().nrCells(),ci.detachData());
}

size_t calc::NullCompressor::toDecompressedIndex(size_t linIndexCompressed) const
{
  return linIndexCompressed;
}

size_t calc::NullCompressor::nrCellsCompressed() const
{
  return rasterSpace().nrCells();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



