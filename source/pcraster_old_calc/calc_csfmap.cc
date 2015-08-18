#include "stddefx.h"

#ifndef INCLUDED_CALC_CSFMAP
#include "calc_csfmap.h"
#define INCLUDED_CALC_CSFMAP
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_GEO_CSFMAP
#include "geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC__MAP2CSF
#endif

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
  size_t len = nrCells();
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
