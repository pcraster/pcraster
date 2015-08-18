#include "stddefx.h"

#ifndef INCLUDED_CALC_GENERATEFIELD
#include "calc_generatefield.h"
#define INCLUDED_CALC_GENERATEFIELD
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_CALC_SPATIALPACKING
#include "calc_spatialpacking.h"
#define INCLUDED_CALC_SPATIALPACKING
#endif

#ifndef INCLUDED_GEO_APPRASTERSPACE
#include "geo_apprasterspace.h"
#define INCLUDED_GEO_APPRASTERSPACE
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_MATHX
#include "mathx.h"
#define INCLUDED_MATHX
#endif

namespace calc {
  class CoordinateTranslator {
    const SpatialPacking&   d_spatialpacking;
    geo::AppRasterSpace     d_ars;
  public:
    CoordinateTranslator(
        const geo::RasterSpace& rs,
        const SpatialPacking&   c):
      d_spatialpacking(c),d_ars(rs)
      {}
    void get(size_t fieldId, double& x, double& y) const
    {
      size_t row,col;
      d_spatialpacking.rasterDim().linear2RowCol(
          d_spatialpacking.toRasterId(fieldId),row,col);
      d_ars.getCoords(row,col,x,y);
    }
    double getX(size_t fieldId) const {
      double x,y;
      get(fieldId,x,y);
      return x;
    }
    double getY(size_t fieldId) const {
      double x,y;
      get(fieldId,x,y);
      return y;
    }
  };
}

calc::GenerateSpatial::GenerateSpatial(
    const Field            &mask,
    const SpatialPacking   &c,
    const geo::RasterSpace &rs):
    d_mask(mask.src_1()),d_nrMask(mask.nrValues()),
    d_spatialpacking(c),
    d_rasterSpace(rs)
{
}
calc::GenerateSpatial::~GenerateSpatial()
{
}

calc::GenerateNonSpatial::GenerateNonSpatial(const geo::RasterSpace &rs):
 d_rasterSpace(rs)
{}
calc::GenerateNonSpatial::~GenerateNonSpatial()
{
}
UINT1 calc::GenerateSpatial::maskAt(size_t pos) const
{
  if (d_nrMask == 1)
    return *d_mask;
  return d_mask[pos];
}

bool calc::GenerateSpatial::maskTrue(size_t pos) const
{
  return maskAt(pos)==1;
}

void calc::GenerateSpatial::xcoordinate(REAL4 *res) const
{
  CoordinateTranslator ct(d_rasterSpace,d_spatialpacking);
  size_t n = d_spatialpacking.nrFieldCells();
  for(size_t i=0; i < n; i++) {
      if (maskTrue(i))
          res[i] = (REAL4)ct.getX(i);
      else
          pcr::setMV(res[i]);
  }
}

void calc::GenerateSpatial::ycoordinate(REAL4 *res) const
{
  CoordinateTranslator ct(d_rasterSpace,d_spatialpacking);
  size_t n = d_spatialpacking.nrFieldCells();
  for(size_t i=0; i < n; i++) {
      if (maskTrue(i))
          res[i] = (REAL4)ct.getY(i);
      else
          pcr::setMV(res[i]);
  }
}

void calc::GenerateSpatial::uniform(REAL4 *res) const
{
 size_t n = d_spatialpacking.nrFieldCells();
 for(size_t i=0; i < n ; i++)
  if (maskTrue(i))
   res[i] = (REAL4)Ran();
  else
   pcr::setMV(res[i]);
}

void calc::GenerateSpatial::normal(REAL4 *res) const
{
 size_t n = d_spatialpacking.nrFieldCells();
 for(size_t i=0; i < n ; i++)
  if (maskTrue(i))
   res[i] = (REAL4)GasDev();
  else
   pcr::setMV(res[i]);
}

void calc::GenerateSpatial::uniqueid(REAL4 *res) const
{
  size_t n = d_spatialpacking.nrFieldCells();
  if (d_nrMask == 1) {
    if (d_mask[0] == 1) {
      for(size_t i=0; i < n; i++)
        res[i] = (REAL4)(i+1);
    } else {
      for(size_t i=0; i < n; i++)
        res[i] = 0;
    }
  } else {
    size_t id=1;
    for(size_t i=0; i < n; i++)
      switch(d_mask[i]) {
       case 0:  res[i] = 0; break;
       case 1:  res[i] = (REAL4)id++; break;
       default: pcr::setMV(res[i]);
      }
  }
}

void calc::GenerateNonSpatial::celllength (REAL4 *res) const
{
    geo::AppRasterSpace     ars(d_rasterSpace);
    *res=(REAL4)ars.cellSize();
}

void calc::GenerateNonSpatial::cellarea   (REAL4 *res) const
{
  celllength(res);
  *res *= *res;
}
void calc::GenerateNonSpatial::mapuniform (REAL4 *res) const
{
  *res=(REAL4)Ran();
}
void calc::GenerateNonSpatial::mapnormal  (REAL4 *res) const
{
  *res=(REAL4)GasDev();
}
