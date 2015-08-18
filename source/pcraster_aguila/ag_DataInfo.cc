#include "ag_DataInfo.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DataInfo class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAINFO MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATAINFO MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     data Address of the data set.
*/
template<class T>
DataInfo<T>::DataInfo(
         T* data)

  : _data(data),
    _valueScale(VS_UNDEFINED)

{
  assert(_data);
}



//! Constructor.
/*!
  \param     data Address of the data set.
  \param     valueScale Value scale of the data.
*/
template<class T>
DataInfo<T>::DataInfo(
         T* data,
         CSF_VS valueScale)

  : _data(data),
    _valueScale(valueScale)

{
  assert(_data);
}



//! Constructor.
/*!
  \param     data Address of the data set.
  \param     valueScale Value scale of the data.
  \param     timeSteps Available time steps in the data set.
*/
template<class T>
DataInfo<T>::DataInfo(
         T* data,
         CSF_VS valueScale,
         dal::DataSpace const& space)

  : _data(data),
    _valueScale(valueScale),
    _space(space)

{
  assert(_data);
}



template<class T>
DataInfo<T>::DataInfo(
         T* data,
         CSF_VS valueScale,
         dal::DataSpace const& space,
         RangeDrawProps* drawProperties)

  : _data(data),
    _valueScale(valueScale),
    _space(space),
    _rangeDrawProperties(drawProperties)

{
  assert(_data);
  assert(dataIsContinuous());
}



//! dtor
template<class T>
DataInfo<T>::~DataInfo()
{
  // _data is use-only.
}



//! Returns a pointer to the data.
/*!
  \return    Pointer to the data.
  \warning   The data is writeable.
*/
template<class T>
T* DataInfo<T>::data()
{
  return _data;
}



//! Returns a pointer to the data.
/*!
  \return    Pointer to the data.
*/
template<class T>
const T* DataInfo<T>::data() const
{
  return _data;
}



//! Returns the value scale of the data.
/*!
  \return    Value scale.
*/
template<class T>
CSF_VS DataInfo<T>::valueScale() const
{
  return _valueScale;
}



template<class T>
bool DataInfo<T>::dataIsContinuous() const
{
  return _valueScale == VS_SCALAR || _valueScale == VS_DIRECTION;
}



template<class T>
dal::DataSpace const& DataInfo<T>::space() const
{
  return _space;
}



template<class T>
const RangeDrawProps* DataInfo<T>::rangeDrawProperties() const
{
  assert(dataIsContinuous() && _rangeDrawProperties);

  return _rangeDrawProperties;
}



template<class T>
bool DataInfo<T>::equals(
         DataInfo const& info) const
{
  return _data == info._data && _valueScale == info._valueScale &&
         _space == info._space;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


} // namespace ag

