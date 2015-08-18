#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_GRIDMAP
#include "calc_gridmap.h"
#define INCLUDED_CALC_GRIDMAP
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif

// Module headers.
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif
#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif
#ifndef INCLUDED_CALC_COMPRESSIONINPUT
#include "calc_compressioninput.h"
#define INCLUDED_CALC_COMPRESSIONINPUT
#endif


/*!
  \file
  This file contains the implementation of the GridMap class.
*/



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GRIDMAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GRIDMAP MEMBERS
//------------------------------------------------------------------------------

calc::GridMap::GridMap(
  const std::string& fileName,
  size_t nrRows,size_t nrCols, VS vs):
    d_fileName(fileName),
    d_nrRows(nrRows),d_nrCols(nrCols),d_vs(vs)
{
}

calc::GridMap::GridMap(
    const std::string& fileName):
    d_fileName(fileName),
    d_nrRows(0),d_nrCols(0),d_vs(VS_FIELD)
{
}

calc::GridMap::~GridMap()
{
}


//! write an identical value to each grid cell of the map
/*!
 * \param value ptr to 1 single value
 */
void calc::GridMap::writeNonSpatial(const void *value)
{
  switch(bytesPerCell(d_vs)) {
    case 1: {
      geo::SimpleRaster<UINT1> r(nrRows(),nrCols(),*(static_cast<const UINT1 *>(value)));
      writeData(r.cells());
    } break;
    case 4: {
      geo::SimpleRaster<INT4> r(nrRows(),nrCols(),*(static_cast<const INT4 *>(value)));
      writeData(r.cells());
    } break;
    default:
      PRECOND(false);
  }
}

//! write values to a map
/*!
 * \param partialValues list of values to write
 */
void calc::GridMap::writeSpatial(const void *values)
{
  writeData(values);
}


/*
 * \todo
 *  Get rid of readAs while d_vs is set in ctor. (When) are they different?
 *  If so, can we do the conversion as conversion operator?
 */
calc::Spatial* calc::GridMap::readData(VS readAs, const Compressor& compressor)
{
  CompressionInput ci(readAs,0,compressor);
  readInBuffer(readAs,ci.decompressedData());
  return compressor.createSpatial(ci);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



