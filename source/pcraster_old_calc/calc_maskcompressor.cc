#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MASKCOMPRESSOR
#include "calc_maskcompressor.h"
#define INCLUDED_CALC_MASKCOMPRESSOR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_DECOMPRESSEDDATA
#include "calc_decompresseddata.h"
#define INCLUDED_CALC_DECOMPRESSEDDATA
#endif
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif
#ifndef INCLUDED_CALC_COMPRESSIONINPUT
#include "calc_compressioninput.h"
#define INCLUDED_CALC_COMPRESSIONINPUT
#endif
#ifndef INCLUDED_CALC_VALUEBUFFER
#include "calc_valuebuffer.h"
#define INCLUDED_CALC_VALUEBUFFER
#endif
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

/*!
  \file
  This file contains the implementation of the MaskCompressor class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MASKCOMPRESSOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MASKCOMPRESSOR MEMBERS
//------------------------------------------------------------------------------

/*!
 * \param mask array of rs.nrCells() elements
 *
 */
calc::MaskCompressor::MaskCompressor(
    const geo::RasterSpace& rs,
    const unsigned char * const mask):
        Compressor(rs),
        d_mask(rs.nrCells()),
        d_compressedToDecompressed(rs.nrCells())
{
  size_t compressedSize(0);
  for(size_t i=0; i < rs.nrCells(); i++) {
    d_mask[i]= mask[i]==1;
    if (d_mask[i])
      d_compressedToDecompressed[compressedSize++]=i;
  }
  d_compressedToDecompressed.resize(compressedSize);
}

calc::MaskCompressor::~MaskCompressor()
{
}


size_t calc::MaskCompressor::toDecompressedIndex(size_t linIndexCompressed) const
{
  PRECOND(linIndexCompressed < nrCellsCompressed());
  return d_compressedToDecompressed[linIndexCompressed];
}

size_t  calc::MaskCompressor::nrCellsCompressed() const
{
  return d_compressedToDecompressed.size();
}

void calc::MaskCompressor::decompress(
    DecompressedData& d,
    const void * compressedData) const
{
  CSF_CR cr = biggestCellRepr(d.vs());
  ConstValueBuffer compressed;
  compressed.d_void=compressedData;

  ValueBuffer copy=createValueBuffer(cr,d_mask.size());
  size_t compressedIndex=0;
  for(size_t i=0; i<d_mask.size(); i++)
    switch(cr) {
      case CR_UINT1: {
            if (d_mask[i])
              copy.d_UINT1[i]= compressed.d_UINT1[compressedIndex++];
            else
              pcr::setMV(copy.d_UINT1[i]);
         } break;
      case CR_INT4: {
            if (d_mask[i])
              copy.d_INT4[i]= compressed.d_INT4[compressedIndex++];
            else
              pcr::setMV(copy.d_INT4[i]);
         } break;
      case CR_REAL4: {
            if (d_mask[i])
              copy.d_REAL4[i]= compressed.d_REAL4[compressedIndex++];
            else
              pcr::setMV(copy.d_REAL4[i]);
         } break;
      default: ;
    }
  d.setDecompressedCopy(copy.d_UINT1);
}

//! create Spatial with compressed contents
calc::Spatial* calc::MaskCompressor::createSpatial(CompressionInput& ci)const
{
  CSF_CR cr = biggestCellRepr(ci.vs());
  ConstValueBuffer decompressed;
  decompressed.d_void=ci.decompressedData();

  ValueBuffer compressed=createValueBuffer(cr,nrCellsCompressed());
  size_t size=CELLSIZE(cr);
  DEVELOP_PRECOND(size==1||size==4);
  size_t compressedIndex=0;
  for(size_t i=0; i<d_mask.size(); i++)
    if (d_mask[i])
     switch(size) {
      case 1: compressed.d_UINT1[compressedIndex++] = decompressed.d_UINT1[i];
              break;
      case 4: compressed.d_INT4[compressedIndex++] = decompressed.d_INT4[i];
              break;
     }
  DEVELOP_PRECOND(compressedIndex == nrCellsCompressed());
  return new Spatial(ci.vs(),nrCellsCompressed(),detach(compressed));
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



