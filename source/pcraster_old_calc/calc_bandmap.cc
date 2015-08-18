#include "stddefx.h"

#ifndef INCLUDED_CALC_BANDMAP
#include "calc_bandmap.h"
#define INCLUDED_CALC_BANDMAP
#endif

#ifndef INCLUDED_GEO_BANDMAP
#include "geo_bandmap.h"
#define INCLUDED_GEO_BANDMAP
#endif
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif


#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

/*!
 * \param fileName  name of existing file to open
 * \exception geo::NotA_PCRasterMap if the file is not a PCRaster map
 */
calc::BandMap::BandMap(const std::string& fileName):
  GridMap(fileName),
  d_map(fileName)
{
  geo::BandMap mapFile(fileName);
  // zoals afgesproken dd 18 sept 2002
  if (mapFile.cellRepr()== CR_REAL4)
     d_vs=VS_S; /* vergeet VS_D */
  else
     d_vs=VS_BNO; /* vergeet VS_L */
  d_nrRows = d_map.nrRows();
  d_nrCols = d_map.nrCols();
}

//! ctor of map to be created
calc::BandMap::BandMap(
  const std::string&   fileName,
  const geo::RasterSpace& rs,
  VS vs):
  GridMap(fileName,rs.nrRows(),rs.nrCols(),vs),
  d_map(d_fileName, rs, biggestCellRepr(vs),false,0)
{
}

calc::BandMap::~BandMap()
{
}

void calc::BandMap::readInBuffer(VS readAs, void *val)
{
  switch(biggestCellRepr(readAs)) {
   case CR_UINT1:
        d_map.getCellsAsUINT1((UINT1 *)val);break;
   case CR_INT4:
        d_map.getCellsAsINT4(( INT4 *)val) ;break;
   case CR_REAL4:
        d_map.getCellsAsREAL4((REAL4 *)val);break;
   default:
        POSTCOND(false);
  }
}

//! sets \a min and \a max to 0, only needed for dynamic data
bool calc::BandMap::getMinMax(double& /*min*/, double& /*max*/) const
{
  return false;
}

void calc::BandMap::writeData(const void *allValues)
{
  switch(d_map.cellRepr()) {
   case CR_UINT1:
        d_map.putCellsAsUINT1((const UINT1 *)allValues);break;
   case CR_INT2:
        d_map.putCellsAsINT4((const INT4 *)allValues);  break;
   case CR_REAL4:
        d_map.putCellsAsREAL4((const REAL4 *)allValues);break;
   default:
        POSTCOND(false);
  }
}
