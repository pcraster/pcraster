#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_AXIS
#include "com_axis.h"
#define INCLUDED_COM_AXIS
#endif



template<class ValueType>
com_Axis<ValueType>::com_Axis()

  : d_min(0), d_minCutOff(0), d_max(0), d_maxCutOff(0)

{
}



template<class ValueType>
com_Axis<ValueType>::com_Axis(ValueType min, ValueType max)

  : d_min(min), d_minCutOff(min), d_max(max), d_maxCutOff(max)

{
}



template<class ValueType>
com_Axis<ValueType>::~com_Axis()
{
}



template<class ValueType>
void com_Axis<ValueType>::setMin(ValueType min, ValueType minCutOff)
{
  d_min       = min;
  d_minCutOff = minCutOff;
}



template<class ValueType>
void com_Axis<ValueType>::setMax(ValueType max, ValueType maxCutOff)
{
  d_max       = max;
  d_maxCutOff = maxCutOff;
}



template<class ValueType>
ValueType com_Axis<ValueType>::getMin() const
{
  return d_min;
}



template<class ValueType>
ValueType com_Axis<ValueType>::getMax() const
{
  return d_max;
}



template<class ValueType>
ValueType com_Axis<ValueType>::getMinCutOff() const
{
  return d_minCutOff;
}



template<class ValueType>
ValueType com_Axis<ValueType>::getMaxCutOff() const
{
  return d_maxCutOff;
}



template<class ValueType>
ValueType com_Axis<ValueType>::getRange() const
{
  return d_max - d_min;
}



template<class ValueType>
ValueType com_Axis<ValueType>::getCutOffRange() const
{
  return d_maxCutOff - d_minCutOff;
}



template<class ValueType>
void com_Axis<ValueType>::setCutOff(ValueType min, ValueType max)
{
  d_minCutOff = min;
  d_maxCutOff = max;
}
