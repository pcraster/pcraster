#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_NULLCOMPRESSOR
#include "calc_nullcompressor.h"
#define INCLUDED_CALC_NULLCOMPRESSOR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_DECOMPRESSEDDATA
#include "calc_decompresseddata.h"
#define INCLUDED_CALC_DECOMPRESSEDDATA
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif
#ifndef INCLUDED_CALC_COMPRESSIONINPUT
#include "calc_compressioninput.h"
#define INCLUDED_CALC_COMPRESSIONINPUT
#endif



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



