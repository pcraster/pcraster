#include "stddefx.h"

#ifndef INCLUDED_CALC_SPATIALIMPL
#include "calc_spatialimpl.h"
#define INCLUDED_CALC_SPATIALIMPL
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"     // for cast spatial->field
#define INCLUDED_CALC_SPATIAL
#endif

//! used for initialization of computed (new) parameter
/*! will recieve value on assignment
 */
calc::SpatialImpl::SpatialImpl(const FieldParameter& p,size_t index):
 FieldValue(p,index)
{
  pcr::setMV(d_min);
  pcr::setMV(d_max);
}

//! used for initialization of input parameter
calc::SpatialImpl::SpatialImpl(const FieldParameter& p,size_t index, Spatial *value):
 FieldValue(p,index,value)
{
  pcr::setMV(d_min);
  pcr::setMV(d_max);
}


void calc::SpatialImpl::write()
{
  if (d_fw.writeCurrentTimeStep())
    d_fw.writeMap(d_min,d_max,value()->srcValue());
}

calc::SpatialImpl::~SpatialImpl()
{
  // no bookkeeping if error state
  if (!std::uncaught_exception())
    d_fw.adjustMinMax(d_min,d_max);
}
