#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_AREAMAP
#include "calc_areamap.h"
#define INCLUDED_CALC_AREAMAP
#endif

// Library headers.
#ifndef INCLUDED_API
#include "api.h"        // BootTestApi
#define INCLUDED_API
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif

// Module headers.
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif



/*!
  \file
  This file contains the implementation of the AreaMap class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class AreaMapPrivate
{
public:

  AreaMapPrivate()
  {
  }

  ~AreaMapPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC AREAMAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF AREAMAP MEMBERS
//------------------------------------------------------------------------------

namespace calc {

AreaMap::AreaMap():
  d_setComputationMaskCallAllowed(true),
  d_areaMap(0),
  d_computationMask(0)
{
}

AreaMap::AreaMap(const geo::RasterSpace& rs):
  d_areaMap(0),
  d_computationMask(0)
{
  setRasterSpace(rs);
}


AreaMap::AreaMap(pcrxml::AreaMapScript const& am):
  d_areaMap(0),
  d_computationMask(0)
{
  d_areaMap = new pcrxml::AreaMapScript(am);

  size_t nrRows(d_areaMap->nrRows().get());
  size_t nrCols(d_areaMap->nrCols().get());

  double cellSize=1;
  if (d_areaMap->cellSize())
   cellSize=d_areaMap->cellSize().get();
  double xLowerLeftCorner=0;
  if (d_areaMap->xLowerLeftCorner())
   xLowerLeftCorner=d_areaMap->xLowerLeftCorner().get();
  double yLowerLeftCorner=0;
  if (d_areaMap->yLowerLeftCorner())
       yLowerLeftCorner=d_areaMap->yLowerLeftCorner().get();
  geo::RasterSpace rs(
       nrRows,nrCols,cellSize,
       xLowerLeftCorner, yLowerLeftCorner+(cellSize*nrRows));
  setRasterSpace(rs);
}

//! Copy constructor.
AreaMap::AreaMap(
         AreaMap const& rhs):
   d_areaMap(0),
   d_computationMask(0)
{
    d_rs   =rhs.d_rs;
    d_mask =rhs.d_mask;
    if (rhs.d_areaMap)
     d_areaMap=new pcrxml::AreaMapScript(*rhs.d_areaMap);
}

AreaMap::~AreaMap()
{
  delete d_areaMap;
  delete d_computationMask;
}



//! Assignment operator.
AreaMap& AreaMap::operator=(
         AreaMap const& rhs)
{
  d_areaMap=0;
  d_computationMask=0;
  if (this != &rhs) {
    d_rs   =rhs.d_rs;
    d_mask =rhs.d_mask;
    if (rhs.d_areaMap)
     d_areaMap=new pcrxml::AreaMapScript(*rhs.d_areaMap);
    if (rhs.d_computationMask)
     d_computationMask=new pcrxml::ComputationMask(*rhs.d_computationMask);
  }
  return *this;
}

void AreaMap::syncMask()
{
  if (d_rs.nrCells() != d_mask.size())
    d_mask = Mask(d_rs.nrCells(),1);
  if (hasCoordinateMask())
    setMaskOnCoordinates();
}

//! is a valid areamap set?
bool AreaMap::isSet() const
{
  return d_rs.valid();
}

bool AreaMap::hasCoordinateMask() const
{
  return d_computationMask && d_computationMask->coordinates();
}

void AreaMap::setComputationMask(pcrxml::ComputationMask const&  computationMask)
{
  PRECOND(d_setComputationMaskCallAllowed);
  d_setComputationMaskCallAllowed=false;
  delete d_computationMask;
  d_computationMask = new pcrxml::ComputationMask(computationMask);
}

void AreaMap::setMaskOnCoordinates()
{
  PRECOND(hasCoordinateMask());
  pcrxml::CoordinateMask const& m(d_computationMask->coordinates().get());
  for(geo::LinearLoc l=0; l < d_rs.nrCells(); ++l) {
    double x,y;
    d_rs.coordinates(x,y,l);
    if (d_mask[l])
      d_mask[l]=
             m.xMinimum() <= x && x <= m.xMaximum() &&
             m.yMinimum() <= y && y <= m.yMaximum() ;
  }
}

/*!
 * f is deleted
 * \pre isSet()
 */
void AreaMap::transferMask(const Field* f)
{
  PRECOND(isSet());
  d_mask = Mask(d_rs.nrCells(),0);

  bool atNonMVs= (d_computationMask                &&
                  d_computationMask->areaMap()     &&
                  d_computationMask->areaMap().get().maskType() ==
                   pcrxml::MaskMapType::computeAtNonMissingValues);
  if (atNonMVs) {
    // computeAtNonMissingValues
    for(size_t i=0; i < d_rs.nrCells(); ++i) {
      double v;
      d_mask[i] = f->getCell(v,i);
     }
  } else {
      // computeAtNonZeroValues
      for(size_t i=0; i < d_rs.nrCells(); ++i) {
       double v;
       if (f->getCell(v,i))
         d_mask[i]= v!=0 ;
      }
  }
  d_setComputationMaskCallAllowed=false;
  deleteFromPcrme(f);
}

void AreaMap::setRasterSpace(const geo::RasterSpace& rs)
{
  d_rs=rs;
  throwIfNotSet();
  BootTestApi(d_rs.cellSize(), d_rs.projection() == geo::YIncrT2B);
  syncMask();
}

const geo::RasterSpace& AreaMap::rasterSpace() const
{
  return d_rs;
}

const AreaMap::Mask& AreaMap::mask() const
{
  return d_mask;
}

void AreaMap::throwIfNotSet() const
{
  if (!isSet())
   throw PosException("no clone or area map specified");
}

/*!
 * create context for pcrxml::RunContext or pcrxml::CheckContext
 * return a CheckContext (less restrictive) if AreaMap is not
 * set the CheckContext's areaMap element to empty
 */
pcrxml::CheckContext* AreaMap::createXMLContext() const
{
  pcrxml::CheckContext *cc= new pcrxml::CheckContext();

  if (!isSet())
    return cc;

  std::unique_ptr<pcrxml::AreaMap> ams(new pcrxml::AreaMap(
    d_rs.nrRows(),d_rs.nrCols()));
  cc->areaMap(std::move(ams));
  PRECOND(cc->areaMap());

  cc->areaMap()->cellSize         (d_rs.cellSize());
  cc->areaMap()->xLowerLeftCorner (d_rs.west());
  cc->areaMap()->yLowerLeftCorner (d_rs.south());

  if (hasCoordinateMask())
    cc->computationMask(*d_computationMask);

  return cc;
}


} // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



