#include "stddefx.h"
#include "calc_csfmap.h"
#include "calc_vs.h"
#include "geo_csfmap.h"
#include "geo_rasterspace.h"
#include "calc_map2csf.h"

/*!
 * \param fileName  name of existing file to open
 * \exception geo::NotA_PCRasterMap if the file is not a PCRaster map
 */
calc::CsfMap::CsfMap(const std::string& fileName):
  GridMap(fileName,0,0,VS_FIELD),
  d_map(fileName)
{
  d_vs = csfVs2vs(d_map.valueScale());
  d_nrRows = d_map.nrRows();
  d_nrCols = d_map.nrCols();
}

//! ctor of map to be created
calc::CsfMap::CsfMap(
  const std::string&   fileName,
  const geo::RasterSpace& rs,
  VS vs):
  GridMap(fileName,rs.nrRows(),rs.nrCols(),vs), 
  d_map(d_fileName, rs, vs2CsfVs(vs))
{
}

calc::CsfMap::~CsfMap()
{
}

void calc::CsfMap::readInBuffer(VS readAs, void *val)
{
  size_t const len = nrCells();
  if (!val)
  switch(bytesPerCell(vs())) {
   case 1: val = new UINT1[len]; break;
   case 4: val = new  INT4[len]; break;
  }
  d_map.useAs(biggestCellRepr(readAs));
  d_map.getCells(val);
}

bool calc::CsfMap::getMinMax(double& min, double& max) const
{
  return d_map.getMinMax(min, max);
}

void calc::CsfMap::writeData(const void *allValues)
{
  d_map.putCells(allValues);
}


geo::RasterSpace calc::CsfMap::rasterSpace() const
{
  return d_map.rasterSpace();
}
