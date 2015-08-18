#include "stddefx.h"

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h" // bytesPerCell
#define INCLUDED_CALC_MAP2CSF
#endif
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"  // getCell
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_DISCR_RASTERDATA
#include "discr_rasterdata.h"
#define INCLUDED_DISCR_RASTERDATA
#endif



size_t calc::Spatial::d_maxBPC=0;
size_t calc::Spatial::d_currentBPC=0;
#ifdef DEBUG_DEVELOP
std::set<size_t> calc::Spatial::d_sizes;
#endif

void calc::Spatial::resetBPC()
{
  d_maxBPC=0;
  d_currentBPC=0;
#ifdef DEBUG_DEVELOP
  d_sizes.clear();
#endif
}

void calc::Spatial::countBPC(VS vs) const
{
  d_currentBPC += bytesPerCell(vs);
  d_maxBPC      = std::max(d_maxBPC, d_currentBPC);
#ifdef DEBUG_DEVELOP
  d_sizes.insert(d_nrValues);
#endif
}

//! maximum nr of bytes per cell ever allocated
size_t calc::Spatial::maxBPC()
{
  return d_maxBPC;
}

//! nr of bytes per cell currently allocated
size_t calc::Spatial::currentBPC()
{
  return d_currentBPC;
}


//! ctor with allocation by default
calc::Spatial::Spatial(VS vs, CRIndex cri, size_t nrValues):
  Field(vs,cri), d_nrValues(nrValues),d_val(0)
{
  allocate();
}

//! copy ctor
calc::Spatial::Spatial(const Spatial& rhs):
  Field(rhs.vs(),rhs.cri()),d_nrValues(rhs.nrValues()),d_val(0)
{
  allocate();
  beMemCpyDest(rhs.src());
}



namespace calc {

Spatial::Spatial(
         VS vs,
         discr::RasterData<UINT1> const& data)

  : Field(vs, crIndex<UINT1>()),
    d_nrValues(data.raster()->nrCells()),
    d_val(0)

{
  allocate();
  beMemCpyDest(data.cells());
}



Spatial::Spatial(
         VS vs,
         discr::RasterData<INT4> const& data)

  : Field(vs, crIndex<INT4>()),
    d_nrValues(data.raster()->nrCells()),
    d_val(0)

{
  allocate();
  beMemCpyDest(data.cells());
}



Spatial::Spatial(
         VS vs,
         discr::RasterData<REAL4> const& data)

  : Field(vs, crIndex<REAL4>()),
    d_nrValues(data.raster()->nrCells()),
    d_val(0)

{
  allocate();
  beMemCpyDest(data.cells());
}

} // namespace calc



void calc::Spatial::allocate()
{
  PRECOND(allFitCRIndex(vs())==cri());
  switch(bytesPerCell(vs())) {
   case 1: d_val1 = new UINT1[nrValues()];
           break;
   case 4: d_val4 = new  INT4[nrValues()];
           break;
  }
  countBPC(vs());
}

//! dtor
calc::Spatial::~Spatial()
{
  if (!d_val)
    return;
  d_currentBPC -= bytesPerCell(vs());
  switch(bytesPerCell(vs())) {
   case 1: delete [] d_val1; break;
   case 4: delete [] d_val4; break;
  }
}


//! return value as read only, initialized value
const void *calc::Spatial::src() const
{
  PRECOND(d_val != 0);
  return d_val;
}

//! return value writable and readable, allocated value
void *calc::Spatial::dest()
{
  PRECOND(d_val != 0);
  return d_val;
}

bool calc::Spatial::getCell(double& value, size_t i) const
{
 /* this now is a work around for bug 147 */
   bool isNotMV;
   switch(cri()) {
    case CRI_1:
      isNotMV = !pcr::isMV(d_val1[i]);
      com::CastCell<double,UINT1>()(value,d_val1[i]);
      break;
    case CRI_4:
      isNotMV = !pcr::isMV(d_val4[i]);
      com::CastCell<double,INT4>()(value,d_val4[i]);
      break;
    case CRI_f:
      isNotMV = !pcr::isMV(d_vals[i]);
      com::CastCell<double,REAL4>()(value,d_vals[i]);
      break;
    default: PRECOND(FALSE);
      isNotMV=true;
  }
  return isNotMV;
  // return !pcr::isMV(value);
}

//! \brief generic set interface
/*!
 * the generic interface does the automatic cast-down with MV handling.
 * a faster interface is to use the type specific dest_1(), dest_4(), dest_f()
 * interface:
 * \code
 *   Spatial s(VS_L,CRI_1,nrCells());
 *   // set cell 4 to value 5 in two ways
 *   double v=5;
 *   s.setCell(v,4);
 *   s.dest_1()[4]=5;
 * \param value  can be missing value
 */
void calc::Spatial::setCell(const double& value, size_t i)
{
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


calc::Spatial* calc::Spatial::createClone() const
{
  return new Spatial(*this);
}

//! number of values, cells
size_t calc::Spatial::nrValues() const
{
  return d_nrValues;
}

//! false, this is never a nonspatial mv
bool calc::Spatial::isMV()const
{
  return false;
}

//! true, this is a spatial
bool calc::Spatial::isSpatial() const
{
  return true;
}


//! check if no cells are true and no cells are false
void calc::Spatial::analyzeBoolean(
    bool& noneAreTrue,
    bool& noneAreFalse) const
{
  PRECOND(vs() == VS_B);
  PRECOND(biggestCellRepr(vs()) == CR_UINT1);
  noneAreTrue = noneAreFalse = true;
  for (size_t i=0; i < nrValues(); i++) {
   if (d_val1[i] == 1)
    noneAreTrue  = false;
   if (d_val1[i] == 0)
    noneAreFalse = false;
  }
}

namespace calc {
class MaskChecker {
  bool      d_newMVsFound;
  //! used once in zero detect searching
  bool      d_allZero;
  size_t    d_n;

  template<class CR> Spatial* createDebugMap(
    const std::vector<bool>& mask,
    const CR *val)
  {
    Spatial *debugMap = new Spatial(VS_N,CRI_4,d_n);
    INT4    *dest     = debugMap->dest_4();

    for(size_t i=0; i < d_n; ++i) {
     dest[i]=mask[i];
     if (mask[i] == 1) {
       if (pcr::isMV(val[i])) {
         d_newMVsFound =true;
         dest[i] = 2;
        }
      }
    }
    return debugMap;
  }

public:
  MaskChecker(
    size_t n):
    d_newMVsFound(false),
    d_allZero(true),
    d_n(n)
    {}

  /*! check for MV in mask, if so return allocated spatial
   */
  template<class CR> Spatial* check(
    const std::vector<bool>& mask,
    const CR *val)
  {
   /* double  prevIdenticalValue;
    * bool    allIdentical=true;
    * pcr::setMV(prevIdenticalValue);
    */
    for(size_t i=0; i < d_n; i++)
      if (mask[i] == 1) {
        if (pcr::isMV(val[i]) )
         return createDebugMap(mask,val);
        /* else {
         *   double v;
         *   me->getCell(v,i);
         *   if ( pcr::isMV(prevIdenticalValue))
         *      prevIdenticalValue=v;
         *   if (v != prevIdenticalValue)
         *        allIdentical=false;
         *   // if (std::fabs(v) > 0.000001)
         *   if (val[i] != 0)
         *       d_allZero=false;
         * }
         */
      }
    // if (!d_allZero && allIdentical)
    //  std::cout << "ident " << prevIdenticalValue << " " << me << " " << sizeof(CR) << "\n";
    return 0;
  }

  bool newMVsFound() const { return d_newMVsFound; };
  bool allZero() const { return d_allZero; };
};
}


//! check if I have MV's on cells where the areaMask is 1 (true)
/*! if any such MV's  are found then the script's areaMask
 *  is modified and written to debugMapName
 *
 *  \param areaMask, 1 for where non-MV is expected in mask, 0 otherwise
 *  \returns
 *    0 is no MV's inside mask, a new created VS_N Spatial object if it does
 */
calc::Spatial* calc::Spatial::findMVinMask(
   const std::vector<bool>& areaMask) const
{
  CRIndex cr=allFitCRIndex(vs());

  MaskChecker c(nrValues());
  // check for MV created if we are assigning a spatial
  switch(cr) {
   case CRI_1 : return c.check(areaMask, src_1());
   case CRI_4 : return c.check(areaMask, src_4());
   case CRI_f : return c.check(areaMask, src_f());
   default: PRECOND(FALSE);
            return 0;
  }
}
