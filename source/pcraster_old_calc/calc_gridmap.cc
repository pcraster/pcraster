#include "stddefx.h"
#include "calc_gridmap.h"
#include "geo_simpleraster.h"
#include "calc_map2csf.h"
#include "calc_compressor.h"
#include "calc_compressioninput.h"

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

calc::GridMap::GridMap(const std::string &fileName, size_t nrRows, size_t nrCols, VS vs)
    : d_fileName(fileName), d_nrRows(nrRows), d_nrCols(nrCols), d_vs(vs)
{
}

calc::GridMap::GridMap(const std::string &fileName)
    : d_fileName(fileName), d_nrRows(0), d_nrCols(0), d_vs(VS_FIELD)
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
  switch (bytesPerCell(d_vs)) {
    case 1: {
      geo::SimpleRaster<UINT1> r(nrRows(), nrCols(), *(static_cast<const UINT1 *>(value)));
      writeData(r.cells());
    } break;
    case 4: {
      geo::SimpleRaster<INT4> r(nrRows(), nrCols(), *(static_cast<const INT4 *>(value)));
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
calc::Spatial *calc::GridMap::readData(VS readAs, const Compressor &compressor)
{
  CompressionInput ci(readAs, nullptr, compressor);
  readInBuffer(readAs, ci.decompressedData());
  return compressor.createSpatial(ci);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
