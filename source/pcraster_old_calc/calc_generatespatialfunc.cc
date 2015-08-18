#include "stddefx.h"

#ifndef INCLUDED_CALC_GENERATESPATIALFUNC
#include "calc_generatespatialfunc.h"
#define INCLUDED_CALC_GENERATESPATIALFUNC
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif

#ifndef INCLUDED_GEO_APPRASTERSPACE
#include "geo_apprasterspace.h"
#define INCLUDED_GEO_APPRASTERSPACE
#endif

namespace calc {
  class CoordinateTranslator {
    const Compressor&   d_compressor;
    geo::AppRasterSpace d_ars;
  public:
    CoordinateTranslator(const Compressor& c):
      d_compressor(c),d_ars(d_compressor.rasterSpace())
      {}
    void get(size_t linIndexCompressed, double& x, double& y) const
    {
      size_t row,col;
      d_compressor.rasterSpace().linear2RowCol(
          d_compressor.toDecompressedIndex(linIndexCompressed),row,col);
      d_ars.getCoords(row,col,x,y);
    }
    double getX(size_t linIndexCompressed) const {
      double x,y;
      get(linIndexCompressed,x,y);
      return x;
    }
    double getY(size_t linIndexCompressed) const {
      double x,y;
      get(linIndexCompressed,x,y);
      return y;
    }
  };
}

/*!
   <ul>
   <li>  \a res  result (always spatial)
   <li>  \a mask mask if 1, generate value
   <li>  \a nrMask nr of cells in mask
   <li>  \a rs   raster space descriptor
   </ul>
 */
calc::GenerateSpatialFunc::GenerateSpatialFunc(
    const UINT1 *maskVal,
    size_t nrMask,
    const  Compressor &c):
    d_mask(maskVal),d_nrMask(nrMask),d_compressor(c)
{
}

UINT1 calc::GenerateSpatialFunc::maskAt(size_t pos) const
{
  if (d_nrMask == 1)
    return *d_mask;
  return d_mask[pos];
}

void calc::GenerateSpatialFunc::xcoordinate(REAL4 *res) const
{
  CoordinateTranslator ct(d_compressor);
  size_t n = d_compressor.nrCellsCompressed();
  for(size_t i=0; i < n; i++) {
      if (maskAt(i) == 1)
          res[i] = static_cast<REAL4>(ct.getX(i));
      else
          pcr::setMV(res[i]);
  }
}

void calc::GenerateSpatialFunc::ycoordinate(REAL4 *res) const
{
  CoordinateTranslator ct(d_compressor);
  size_t n = d_compressor.nrCellsCompressed();
  for(size_t i=0; i < n; i++) {
      if (maskAt(i) == 1)
          res[i] = static_cast<REAL4>(ct.getY(i));
      else
          pcr::setMV(res[i]);
  }
}


void calc::GenerateSpatialFunc::uniqueid(REAL4 *res) const
{
  size_t n = d_compressor.nrCellsCompressed();
  if (d_nrMask == 1) {
    if (d_mask[0] == 1) {
      for(size_t i=0; i < n; i++)
        res[i] = static_cast<REAL4>(i+1);
    } else {
      for(size_t i=0; i < n; i++)
        res[i] = 0;
    }
  } else {
    size_t id=1;
    for(size_t i=0; i < n; i++)
      switch(d_mask[i]) {
       case 0:  res[i] = 0; break;
       case 1:  res[i] = static_cast<REAL4>(id++); break;
       default: pcr::setMV(res[i]);
      }
  }
}
