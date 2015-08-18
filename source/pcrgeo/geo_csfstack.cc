#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CSFSTACK
#include "geo_csfstack.h"
#define INCLUDED_GEO_CSFSTACK
#endif

#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_LEGEND
#include "com_legend.h"
#define INCLUDED_COM_LEGEND
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

#ifndef INCLUDED_GEO_CSFMAP
#include "geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif

#ifndef INCLUDED_GEO_DEF
#include "geo_def.h"
#define INCLUDED_GEO_DEF
#endif

#ifndef INCLUDED_GEO_UTIL
#include "geo_util.h"
#define INCLUDED_GEO_UTIL
#endif



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

template<class T>
geo::CSFStack<T>::CSFStack(std::string const& name, CSF_CR useType)

  : d_name(CSFStackName(name)), d_step(-1),
    d_minIsValid(false), d_maxIsValid(false),
    d_valueScale(VS_UNDEFINED), d_useType(useType), d_raster(0)

{
  init();
}



/*!
  \param     sn The name of the stack.
  \param     useType The cell representation used in the app.
  \exception com_OpenFileError If \a sn can't be opened.

  No data will be loaded at this point. Use the load() function for loading
  data.

  If \a sn is the name of a dynamic stack, than the disk will be scanned for
  available timesteps.

  The minimum and maximum value of the stack will be determined. This means that
  you can call the min() and max() functions without loading data in the stack.
*/
template<class T>
geo::CSFStack<T>::CSFStack(const geo::CSFStackName &name, CSF_CR useType)

  : d_name(name), d_step(-1), d_minIsValid(false), d_maxIsValid(false),
    d_valueScale(VS_UNDEFINED), d_useType(useType), d_raster(0)

{
  init();
}



/*
template<class T>
geo::CSFStack<T>::CSFStack(RasterSpace& space, CSF_VS valueScale,
         CSF_CR useType)

  : d_name(name), d_step(-1), d_minIsValid(false), d_maxIsValid(false),
    d_valueScale(valueScale), d_useType(useType)

{
  T mv;
  pcr::setMV(mv);
  d_raster = new Raster<T>((space), mv);
}
*/



template<class T>
geo::CSFStack<T>::CSFStack(const CSFStack &rhs)

  : d_name(rhs.d_name), d_step(rhs.d_step),
    d_min(rhs.d_min), d_minIsValid(rhs.d_minIsValid),
    d_max(rhs.d_max), d_maxIsValid(rhs.d_maxIsValid),
    d_valueScale(rhs.d_valueScale), d_useType(rhs.d_useType),
    d_raster(0)

{
  if(rhs.d_raster) {
    d_raster = new Raster<T>(*rhs.d_raster);
  }

  if(rhs.hasLegend()) {
    d_legend.reset(new com::Legend<INT4>(*rhs.legend()));
  }
}



template<class T>
geo::CSFStack<T>::~CSFStack()
{
  clean();
}



template<class T>
void geo::CSFStack<T>::clean()
{
  delete d_raster; d_raster = 0;
}



template<class T>
void geo::CSFStack<T>::init()
{
  //  1. Set the stackname to sn and scan the disk for available timesteps.
  //  2. Since we don't load data yet we set the d_step variable to an illegal,
  //     negative, value.

  // read a header, get info, create raster, close map

  try {

    // Determine file name(s).
    if(!d_name.scanned()) {
      d_name.scan();
    }

    com::PathName fn;
    if(d_name.isDynamic() && d_name.nrLayers()) {
      fn = d_name.fileName(*d_name.begin());
    }
    else {
      fn = d_name.baseName();
    }

    std::auto_ptr<CSFMap> map(new CSFMap(fn)); // READ

    // Value scale.
    PRECOND(map->valueScale() != VS_UNDEFINED);
    d_valueScale = map->valueScale();

    // Cell representation.
    if(d_useType == CR_UNDEFINED) {
      d_useType = ValueScale2CellRepr(d_valueScale).defaultCR();
    }
    PRECOND(d_useType != CR_UNDEFINED);
    map->useAs(d_useType);

    // Projection, min, max.
    PRECOND(map->projection() != PT_UNDEFINED);
    d_minIsValid = map->min(&d_min) ? true: false;
    d_maxIsValid = map->max(&d_max) ? true: false;
    Projection proj = map->projection() == PT_YINCT2B ? YIncrT2B : YIncrB2T;
    d_raster = new Raster<T>(map->nrRows(), map->nrCols(),
                   map->cellSize(), map->left(), map->top(), proj);

    // Legend.
    if(map->hasLegend()) {
      d_legend.reset(new com::Legend<INT4>(map->legend()));
    }

    // If stack is dynamic we have to scan the whole stack to get the extremes.
    // FIXME: Isn't the min and max of the first layer enough?
    if(d_name.isDynamic() && d_name.nrLayers()) {
      T min, max;

      CSFStackName::const_iterator it;
      for(it = d_name.begin() + 1; it != d_name.end(); ++it) {

        fn = d_name.fileName(*it);
        map.reset(new CSFMap(fn)); // READ
        map->useAs(d_useType);

        if(map->min(&min)) {
          d_min = d_minIsValid ? MIN(d_min, min) : min;
          d_minIsValid = true;
        }

        if(map->max(&max)) {
          d_max = d_maxIsValid ? MAX(d_max, max) : max;
          d_maxIsValid = true;
        }
      }
    }
  }
  catch(...)
  {
    clean();
    throw;
  }
}



/*!
  \param     t Timestep to load.
  \exception com::OpenFileError If the csf file for timestep \a t can't be
             opened.

  If the stack is static, than the static data will be loaded. In that case
  the value of \a t doesn't matter. If the stack is dynamic, than timestep
  \a t of the stack will be loaded.
*/
template<class T>
void geo::CSFStack<T>::load(size_t t)
{
  //  1. Test if the requested timestep isn't already loaded.
  //  2. Create a filename for the raster of timestep \t.
  // 11. Read the data (include file existence test)
  // 20. Set the current read timestep to t.

  if(!loaded(t)) {                                                         // 1.
    com::PathName fn;                                                      // 2.
    if(d_name.isDynamic() && d_name.nrLayers()) {
      if(isAvailable(t)) {
        fn = d_name.fileName(t);
      }
    }
    else {
      fn = d_name.baseName();
    }

    if(fn.isEmpty()) {
      // Set raster to all mv's.
      for(size_t r = 0; r < d_raster->nrRows(); ++r) {
        for(size_t c = 0; c < d_raster->nrCols(); ++c) {
          pcr::setMV(d_raster->cell(r, c));
        }
      }
    }
    else {
      // Load data from file.
      std::auto_ptr<CSFMap> map(new CSFMap(fn));  // READ
      map->useAs(d_useType);
      map->getCells(d_raster->cells());
    }

    d_step = t;
  }
}



/*!
  \brief     Returns true if the data for time step \a t are already loaded.
  \param     t Time.
  \return    True if data for \a t is loaded.
  \sa        load(size_t)

  If this is a static stack than \a t is not important.
*/
template<class T>
bool geo::CSFStack<T>::loaded(size_t t) const
{
  if(d_step < 0) {
    return false;
  }
  else if(d_name.isDynamic()) {
    return d_step == static_cast<int>(t);
  }
  else {
    return d_step != -1;
  }
}



//! Returns true if date for time step \a t is available.
/*!
  \param     t Time step.
  \return    true if data for \a t is available.
  \sa        loaded(size_t)
*/
template<class T>
bool geo::CSFStack<T>::isAvailable(size_t t) const
{
  return d_name.isAvailable(t);
}



/*!
  \return    The first timestep available in the stack. If the stack is static,
             than 0 is returned.
  \sa        lastStep()
*/
template<class T>
size_t geo::CSFStack<T>::firstStep() const
{
  return d_name.isDynamic() && d_name.nrLayers() ? *(d_name.begin()) : 0;
}



/*!
  \return    The last timestep available in the stack. If the stack is static,
             than 0 is returned.
  \sa        firstStep()
*/
template<class T>
size_t geo::CSFStack<T>::lastStep() const
{
  return d_name.isDynamic() && d_name.nrLayers() > 0 ? *(d_name.end() - 1) : 0;
}



/*!
  \return    The current raster.
  \warning   If you haven't called the load() member yet, than you can't expect
             the raster to contain useful data!

  You can expect the header info (eg Raster::nrCols()) to be filled.
*/
template<class T>
const geo::Raster<T> &geo::CSFStack<T>::raster() const
{
  return *d_raster;
}



/*!
  \param     m Pointer to value to set.
  \return    True if a minimum value was available and false if the stack
             contains missing values.
  \sa        max()
*/
template<class T>
bool geo::CSFStack<T>::min(T *m) const
{
  *m = d_min;
  return d_minIsValid;
}



/*!
  \param     m Pointer to value to set.
  \return    True if a maximum value was available and false if the stack
             contains missing values.
  \sa        min()
*/
template<class T>
bool geo::CSFStack<T>::max(T *m) const
{
  *m = d_max;
  return d_maxIsValid;
}



/*!
  \return    True if the stack only contains missing values.
*/
template<class T>
bool geo::CSFStack<T>::allMV() const
{
  return !d_minIsValid || !d_maxIsValid;
}



/*!
  \return    Name of the stack.
*/
template<class T>
const geo::CSFStackName &geo::CSFStack<T>::name() const
{
  return d_name;
}


template<class T>
const geo::RasterSpace *geo::CSFStack<T>::rasterSpace() const
{
  return d_raster ? &d_raster->space() : 0;
}



//! Returns the value scale of the data in the stack.
/*!
  \return    Value scale.
*/
template<class T>
CSF_VS geo::CSFStack<T>::valueScale() const
{
  return d_valueScale;
}



//! Returns true if the stack has a legend.
/*!
  \return    true if the stack has a legend.
  \sa        legend()
*/
template<class T>
bool geo::CSFStack<T>::hasLegend() const
{
  return d_legend.get() != 0;
}



//! Returns the legend.
/*!
  \return    Legend.
  \warning   Stack must have a legend.
  \sa        hasLegend()
*/
template<class T>
const com::Legend<INT4>* geo::CSFStack<T>::legend() const
{
  PRECOND(d_legend.get());

  return d_legend.get();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------

template class geo::CSFStack<UINT1>;
template class geo::CSFStack<INT4>;
template class geo::CSFStack<REAL4>;
template class geo::CSFStack<REAL8>;

