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
#ifndef INCLUDED_GEO_UTIL
#include "geo_util.h"
#define INCLUDED_GEO_UTIL
#endif
#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif
#ifndef INCLUDED_GEO_BANDMAP
#include "geo_bandmap.h"
#define INCLUDED_GEO_BANDMAP
#endif
#ifndef INCLUDED_DAL_PROPERTYKEYS
#include "dal_PropertyKeys.h"
#define INCLUDED_DAL_PROPERTYKEYS
#endif
#ifndef INCLUDED_DAL_RASTERDRIVER
#include "dal_RasterDriver.h"
#define INCLUDED_DAL_RASTERDRIVER
#endif
#ifndef INCLUDED_DAL_RASTERDAL
#include "dal_RasterDal.h"
#define INCLUDED_DAL_RASTERDAL
#endif
#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif
#ifndef INCLUDED_GEO_EXCEPTION
#include "geo_exception.h"
#define INCLUDED_GEO_EXCEPTION
#endif
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif
// Module headers.
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

/*!
  \file
  This file contains the implementation of the GridMap class.
*/



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GRIDMAP MEMBERS
//------------------------------------------------------------------------------

static dal::RasterDal* rasterDal(0);

//------------------------------------------------------------------------------
// DEFINITION OF GRIDMAP MEMBERS
//------------------------------------------------------------------------------

calc::GridMap::~GridMap()
{
}

calc::GridMap::GridMap(
  std::string const& fileName):
    d_fileName(fileName)
{
  if (!rasterDal)
    rasterDal=new  dal::RasterDal(true);
}

calc::GridMapOut::GridMapOut(
  std::string const& fileName,
  dal::RasterDriver const&  driver,
  geo::RasterSpace const& rs,
  VS vs):
  GridMap(fileName),
  d_driver(driver)
{
  d_rs=rs;
  d_vs=vs;
}

calc::GridMapOut::~GridMapOut()
{
}


//! write an identical value to each grid cell of the map
/*!
 * \param value ptr to 1 single value
 */
calc::GridStat calc::GridMapOut::writeNonSpatial(const void *value)
{
  if (bytesPerCell(d_vs)==1) {
      geo::SimpleRaster<UINT1> r(nrRows(),nrCols(),*(static_cast<const UINT1 *>(value)));
      return writeData(r.cells());
  }
  PRECOND(bytesPerCell(d_vs)==4);
  geo::SimpleRaster<INT4> r(nrRows(),nrCols(),*(static_cast<const INT4 *>(value)));
  return writeData(r.cells());
}

//! write values to a map
/*!
 * \param partialValues list of values to write
 */
calc::GridStat calc::GridMapOut::writeSpatial(const void *values)
{
  return writeData(values);
}

calc::GridStat calc::GridMapOut::writeData(const void *allValues)
{
  if (d_driver.name() == "EHdr") {
    // Hack up old "WL" bandmap thing
    geo::BandMap map(d_fileName, d_rs, biggestCellRepr(d_vs),false,0);
    switch(map.cellRepr()) {
     case CR_UINT1:
          map.putCellsAsUINT1((const UINT1 *)allValues);break;
     case CR_INT2:
          map.putCellsAsINT4((const INT4 *)allValues);  break;
     case CR_REAL4:
          map.putCellsAsREAL4((const REAL4 *)allValues);break;
     default:
          POSTCOND(false);
    }
    return GridStat();
  }

  dal::TypeId t;

  switch(biggestCellRepr(d_vs)) {
    case CR_UINT1: t=dal::TI_UINT1; break;
    case CR_INT4:  t=dal::TI_INT4; break;
    case CR_REAL4: t=dal::TI_REAL4; break;
    default: POSTCOND(false); break;
  }

  dal::Raster raster(d_rs.nrRows(),d_rs.nrCols(),
      d_rs.cellSize(), d_rs.west(), d_rs.north(),t);

  dal::Properties& p(raster.properties());
  p.setValue(DAL_CSF_ANGLE ,d_rs.angle());
  p.setValue(DAL_CSF_VALUESCALE,vs2CsfVs(d_vs));
  p.setValue(DAL_CSF_PROJECTION,geo::geoProjToCsf(d_rs.projection()));

  raster.setCellsReference((void *)allValues);

  d_driver.write(raster, d_fileName);
  raster.setExtremes();

  GridStat s;
  if (!raster.allMV()) {
    s.d_minMaxSet=true;
    switch(t) {
     case dal::TI_UINT1:
       s.d_min = boost::any_cast<UINT1>(raster.min());
       s.d_max = boost::any_cast<UINT1>(raster.max());
       break;
     case dal::TI_INT4:
       s.d_min = boost::any_cast<INT4>(raster.min());
       s.d_max = boost::any_cast<INT4>(raster.max());
       break;
     case dal::TI_REAL4:
       s.d_min = boost::any_cast<REAL4>(raster.min());
       s.d_max = boost::any_cast<REAL4>(raster.max());
       break;
    default: POSTCOND(false); break;
   }
  }
  return s;
}

calc::GridMapIn::GridMapIn(
    std::string const& fileName):
    GridMap(fileName),
    d_bandMap(false)
{
  // try {
    boost::shared_ptr<dal::Raster> raster;
    dal::RasterDriver* driver;
    boost::tie(raster, driver) = rasterDal->open(fileName);
    if (!raster) // TODO not  a recognized map instead of a PCRasterMap
      throw geo::NotA_PCRasterMap(fileName);

    const dal::Properties& p(raster->properties());

    double angle=p.value<double>(DAL_CSF_ANGLE,0.0);
    CSF_PT prj  =p.value<CSF_PT>(DAL_CSF_PROJECTION,PT_YINCT2B);
    CSF_VS vs   =p.value<CSF_VS>(DAL_CSF_VALUESCALE,VS_NOTDETERMINED);

    if (vs!=VS_NOTDETERMINED)
      d_vs = csfVs2vs(vs);
    else {
      // as in old calc_bandmap.cc
      if(dal::isInteger(raster->typeId()))
        d_vs=VS_BNO; // forget VS_L fttb
      else {
        if (dal::isFloatingPoint(raster->typeId()))
          d_vs=VS_S; // forget VS_D fttb
        else
         throw geo::NotA_PCRasterMap(fileName+"(not_a_numeric_raster)");
      }
    }
     d_rs = geo::RasterSpace(raster->nrRows(),raster->nrCols(),
            raster->cellSize(),
            raster->west(),
            raster->north(),
            geo::csfProjToGeo(prj), angle);
     assert(driver);
     d_bandMap = driver->name()=="EHdr";

  //  } catch(const dal::Exception& e) {
  //  PRINT_VAR(e.message());
  //  exit(1);
  //  }
}

calc::GridMapIn::~GridMapIn()
{
}

//! read map data in buffer
void  calc::GridMapIn::createSpatial(void *dest,VS readAs)
{
  if (d_bandMap) {
    // Hack up old "WL" bandmap thing
    geo::BandMap map(d_fileName);
    switch(biggestCellRepr(readAs)) {
     case CR_UINT1:
          map.getCellsAsUINT1((UINT1 *)dest);break;
     case CR_INT4:
          map.getCellsAsINT4(( INT4 *)dest) ;break;
     case CR_REAL4:
          map.getCellsAsREAL4((REAL4 *)dest);break;
     default:
          POSTCOND(false);
    }
   return;
  }
  dal::TypeId typeId;
  switch(calc::biggestCellRepr(readAs)) {
    case CR_UINT1: typeId=dal::TI_UINT1; break;
    case CR_INT4 : typeId=dal::TI_INT4 ; break;
    case CR_REAL4: typeId=dal::TI_REAL4; break;
    default: POSTCOND(false); break;
  }
 // dal::Raster raster(nrRows(),nrCols(),1,1,1,typeId);
 // raster.setCellsReference(dest);
 // d_driver->read(raster,d_fileName);

  boost::shared_ptr<dal::Raster> raster;
  dal::RasterDriver* driver;
  boost::tie(raster, driver) = rasterDal->open(d_fileName);
  POSTCOND(raster); // TODO not  a recognized map, but catched earlier on
  raster->setTypeId(typeId);
  raster->setCellsReference(dest);
  assert(driver);
  driver->read(*raster,d_fileName);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



