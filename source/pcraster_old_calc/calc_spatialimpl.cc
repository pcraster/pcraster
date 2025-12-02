#include "stddefx.h"
#include "calc_spatialimpl.h"
#include "com_csfcell.h"
#include "calc_spatial.h"  // for cast spatial->field

#include <exception>
#include <stdexcept>

//! used for initialization of computed (new) parameter
/*! will recieve value on assignment
 */
calc::SpatialImpl::SpatialImpl(const FieldParameter &p, size_t index) : FieldValue(p, index)
{
  pcr::setMV(d_min);
  pcr::setMV(d_max);
}

//! used for initialization of input parameter
calc::SpatialImpl::SpatialImpl(const FieldParameter &p, size_t index, Spatial *value)
    : FieldValue(p, index, value)
{
  pcr::setMV(d_min);
  pcr::setMV(d_max);
}

void calc::SpatialImpl::write()
{
  if (d_fw.writeCurrentTimeStep())
    d_fw.writeMap(d_min, d_max, value()->srcValue());
}

calc::SpatialImpl::~SpatialImpl()
{
  // no bookkeeping if error state
#if __cpp_lib_uncaught_exceptions
  if (!std::uncaught_exceptions()) {
#else
  if (!std::uncaught_exception()) {
#endif
    d_fw.adjustMinMax(d_min, d_max);
  }
}
