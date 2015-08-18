#include "stddefx.h"

#ifndef INCLUDED_CALC_NONSPATIALIMPL
#include "calc_nonspatialimpl.h"
#define INCLUDED_CALC_NONSPATIALIMPL
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

//! used for initialization of computed parameter
/*! will recieve value on assignment
 */
calc::NonSpatialImpl::NonSpatialImpl(const calc::FieldParameter& p,size_t index):
 calc::FieldValue(p,index)
{
}

//! used for initialization of input parameter
calc::NonSpatialImpl::NonSpatialImpl(const calc::FieldParameter& p,size_t index, calc::NonSpatial *value):
 calc::FieldValue(p,index,value)
{
}


void calc::NonSpatialImpl::write()
{
  PRECOND(value());
  if (d_fw.writeCurrentTimeStep())
    d_fw.writeNonSpatialToMap(value()->srcValue());
}

calc::NonSpatialImpl::~NonSpatialImpl()
{
}
