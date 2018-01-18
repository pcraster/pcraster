#include "stddefx.h"

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif
#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif

#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif

#ifndef INCLUDED_CALC_GRIDMAP
#include "calc_gridmap.h"
#define INCLUDED_CALC_GRIDMAP
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_DECOMPRESSEDDATA
#include "calc_decompresseddata.h"
#define INCLUDED_CALC_DECOMPRESSEDDATA
#endif
#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif

size_t calc::Spatial::d_maxBPC=0;
size_t calc::Spatial::d_currentBPC=0;


//! ctor with allocation by default
calc::Spatial::Spatial(VS vs, size_t nrValues, bool doAllocation):
  Field(vs),d_val(0),d_nrValues(nrValues)
{
  if (doAllocation)
    allocate();
}

//! ctor that will own the value buffer
/*!
 * \param valueBuffer owned by this, this will delete
 */
calc::Spatial::Spatial(VS vs, size_t nrValues, void *valueBuffer):
  Field(vs),d_val(valueBuffer),d_nrValues(nrValues)
{
  countBPC(vs); // consume
}

void calc::Spatial::allocate() const
{
  countBPC(vs());
  switch(bytesPerCell(vs())) {
   case 1: d_val1 = new UINT1[d_nrValues];
           break;
   case 4: d_val4 = new  INT4[d_nrValues];
           break;
  }
}

//! dtor
calc::Spatial::~Spatial()
{
  if (!d_val) // e.g. ZeroMap may have 0 as d_val
    return;
  // 'de-'count allocated
  d_currentBPC -= bytesPerCell(vs());
  switch(bytesPerCell(vs())) {
   case 1: delete [] d_val1; break;
   case 4: delete [] d_val4; break;
  }
}

void calc::Spatial::loadGrid(
    GridMap& m,
    const Compressor& c) const
{
   Spatial *s=m.readData(vs(),c);
   POSTCOND(d_nrValues==s->d_nrValues);
   POSTCOND(vs() == s->vs());
   d_val= s->d_val;
   s->d_val=0;
   delete s;
}

//! number of values, cells
size_t calc::Spatial::nrValues() const
{
  return d_nrValues;
}

void calc::Spatial::makeDataAvailable() const
{
  if (!d_val)
    loadExternal();
}

//! check if no cells are true and no cells are false
void calc::Spatial::analyzeBoolean(
    bool& noneAreTrue,
    bool& noneAreFalse) const
{
  makeDataAvailable();
  PRECOND(vs() == VS_B);
  PRECOND(biggestCellRepr(vs()) == CR_UINT1);
  noneAreTrue = noneAreFalse = true;
  for (size_t i=0; i < d_nrValues; i++) {
   if (d_val1[i] == 1)
    noneAreTrue  = false;
   if (d_val1[i] == 0)
    noneAreFalse = false;
  }
}

namespace calc {
template<typename CR,     // type operated on
         typename CR0=CR  // for float not default, but an integral type that with
                          //  the same bit pattern will yield 0
                          //  this is to prefent normalization issues (I think)
        > class MaskChecker {
  bool d_newMVsFound;
  bool d_allZero;
public:
  MaskChecker(UINT1 *mask, const Spatial *me):
    d_newMVsFound(false),
    d_allZero(true)
  {
    const CR *val = static_cast<const CR *>(me->srcValue());
    size_t   n    = me->nrValues();
    for(size_t i=0; i < n; i++)
      if (mask[i] == 1) {
        // the allZero check is done only in the mask
        // that is why -0 implies -m
        if (pcr::isMV(val[i]) ) {
          d_newMVsFound =true;
          mask[i] = 2;
        } else {
          const CR0 *v0 = (const CR0 *)(val+i);
          if ((*v0) != 0)
              d_allZero=false;
        }
      }
  };
  bool newMVsFound() const { return d_newMVsFound; };
  bool allZero() const { return d_allZero; };
};
}

//! check if I have MV's on cells where the areaMask is 1 (true)
/*! if any such MV's  are found then the script's areaMask
 *  is modified and written to debugMapName
 *  \note
 *     name is incorrect it is also used for -0 without -d
 *  \todo
 *    writting debug map more than once will crash
 */
bool calc::Spatial::checkDebug(
    const calc::IScript& s,
    bool& allZero,
    size_t& bpc) const
{
  UINT1 *mask = static_cast<UINT1 *>(s.areaMask());
  CSF_CR cr = biggestCellRepr(vs());

   bool   newMVsFound;
   /* check for MV created if we are assigning a spatial*/
   bpc=4;
   switch(cr) {
    case CR_UINT1 : {
           MaskChecker<UINT1> c(mask, this);
           newMVsFound=c.newMVsFound();
           allZero = c.allZero();
           bpc=1;
          } break;
    case CR_INT4 : {
           MaskChecker<INT4> c(mask, this);
           newMVsFound=c.newMVsFound();
           allZero = c.allZero();
          } break;
  case CR_REAL4 : {
           MaskChecker<REAL4,INT4> c(mask, this);
           newMVsFound=c.newMVsFound();
           allZero = c.allZero();
          } break;
  default: PRECOND(FALSE);
  }
  if (newMVsFound && s.debugMvAssignments()) {
    // wroteOnce: prevent crash writing when more than once
    static bool wroteOnce(false);
    if (wroteOnce)
      return true;
    DecompressedData dd(VS_L); // UINT1
    s.compressor().decompress(dd,mask);
    std::unique_ptr<GridMap> debugMap(s.createMap(s.debugMapName(),VS_N));
    size_t nrOutValues = s.rasterSpace().nrCells();
    geo::SimpleRaster<INT4> i4(1,nrOutValues);
    com::copyCells(i4.cells(), (const UINT1 *)dd.decompressed(), nrOutValues);
    debugMap->writeData(i4.cells());
    wroteOnce=true;
  }
  return newMVsFound;
}

/*!
 * \todo
 *  this is illegal for ZeroMap derived class
 */
void calc::Spatial::setCell(const double& value, size_t i)
{
   makeDataAvailable();
   switch(biggestCellRepr(vs())) {
    case CR_UINT1:
      com::CastCell<UINT1,double>()(d_val1[i],value);
      break;
    case CR_INT4:
      com::CastCell<INT4, double>()(d_val4[i],value);
      break;
    case CR_REAL4:
      com::CastCell<REAL4,double>()(d_vals[i],value);
      break;
    default: PRECOND(FALSE);
  }
}

bool calc::Spatial::getCell(double& value, size_t i) const
{
  makeDataAvailable();
  bool isNotMV;
  switch(biggestCellRepr(vs())) {
    case CR_UINT1:
      isNotMV = !pcr::isMV(d_val1[i]);
      com::CastCell<double,UINT1>()(value,d_val1[i]);
      break;
    case CR_INT4:
      isNotMV = !pcr::isMV(d_val4[i]);
      com::CastCell<double,INT4>()(value,d_val4[i]);
      break;
    case CR_REAL4:
      isNotMV = !pcr::isMV(d_vals[i]);
      com::CastCell<double,REAL4>()(value,d_vals[i]);
      break;
    default: PRECOND(FALSE);
  }
  return isNotMV;
}

//! return a new allocated copy of this
calc::Spatial *calc::Spatial::copy() const
{
  makeDataAvailable();
  calc::Spatial *c = new calc::Spatial(vs(),d_nrValues,true);
  CSF_CR cr = biggestCellRepr(vs());
  size_t len = CSFSIZEOF(d_nrValues,cr);
  memcpy(c->d_val,srcValue(),len);
  return c;
}

void calc::Spatial::loadExternal() const
{
}

void *calc::Spatial::valuePtr() const
{
  return d_val;
}

//! return value as read only, initialized value
const void *calc::Spatial::srcValue() const
{
  makeDataAvailable();
  PRECOND(d_val != 0);
  return d_val;
}

//! return value writable and readable, allocated value
void *calc::Spatial::destValue()
{
  makeDataAvailable();
  PRECOND(d_val != 0);
  return d_val;
}


//! false, this is never a non spatial mv
bool calc::Spatial::isMv()const
{
  return false;
}

//! true, this is a spatial
bool calc::Spatial::isSpatial() const
{
  return true;
}

void calc::Spatial::countBPC(VS vs) const
{
  d_currentBPC += bytesPerCell(vs);
  d_maxBPC  = std::max(d_maxBPC, d_currentBPC);
}

//! maximum nr of bytes per cell ever allocated
size_t calc::Spatial::maxBPC()
{
  return d_maxBPC;
}

size_t calc::Spatial::valLen() const
{
  return d_nrValues*bytesPerCell(vs());
}

//! nr of bytes per cell currently allocated
size_t calc::Spatial::currentBPC()
{
  return d_currentBPC;
}
