#include "ag_DataObjectBase.h"

// Library headers.
#include <algorithm>
#include <functional>
#include <utility>
#include <vector>
#include <boost/tuple/tuple.hpp>

// PCRaster library headers.

// Module headers.
#include "ag_Dataset.h"



/*!
  \file
  This file contains the implementation of the DataObject class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAOBJECT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATAOBJECT MEMBERS
//------------------------------------------------------------------------------

namespace ag {

//! Constructor.
/*!
  \param     dataType Data type of data that will be managed.
*/
template<class T>
DataObjectBase<T>::DataObjectBase(
         geo::DataType dataType)

  : _manager(dataType)

{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Commented delete which dumps core. Find reason.
*/
template<class T>
DataObjectBase<T>::~DataObjectBase()
{
  clear();
}



//! Finds tuple which contains \a data.
/*!
  \param     data Pointer to data to find tuple for.
  \return    Iterator to tuple found.
*/
template<class T>
typename DataObjectBase<T>::tuple_iter DataObjectBase<T>::find(
         T const* data) const
{
  tuple_iter result = _tuples.begin();

  while(result != _tuples.end()) {

    if(boost::tuples::get<2>(*result).data() == data) {
      break;
    }

    ++result;
  }

  return result;
}



//! Finds tuple that contains \a name, \a space.
/*!
  \param     name Name of tuple to find.
  \param     space Data space of tuple to find.
  \return    Iterator to tuple found.
*/
template<class T>
typename DataObjectBase<T>::tuple_iter DataObjectBase<T>::find(
         std::string const& name,
         dal::DataSpace const& space) const
{
  tuple_iter result = _tuples.begin();

  while(result != _tuples.end()) {

    if(boost::tuples::get<0>(*result) == name &&
       boost::tuples::get<1>(*result) == space) {
      break;
    }

    ++result;
  }

  return result;
}



template<class T>
bool DataObjectBase<T>::exists(
         std::string const& name,
         dal::DataSpace const& space) const
{
  return find(name, space) != _tuples.end();
}



template<class T>
DataGuide DataObjectBase<T>::add(
         std::string const& name,
         dal::DataSpace const& space)
{
  assert(space.hasScenarios()
         ? space.dimension(space.indexOf(dal::Scenarios)).nrValues() == 1
         : true);

  if(!exists(name, space)) {
    DataInfo<T> info = openData(name, space);
    _tuples.push_back(Tuple(name, space, info));
  }

  assert(exists(name, space));

  tuple_iter it = find(name, space);
  DataGuide guide = _manager.add(boost::tuples::get<2>(*it));

  return guide;
}



template<class T>
void DataObjectBase<T>::clear()
{
  typename DataManager<T>::const_guide_iter it = _manager.guides_begin();

  while(it != _manager.guides_end()) {
    remove(*it);   // WARNING: Invalidates it.
    it = _manager.guides_begin();
  }

  // Check whether all data has indeed been removed.
  assert(_manager.size() == 0);
  // assert(nameDataInfoTuples().size() == 0);
}



//! Removes data guide \a guide from the data object.
/*!
  \param     guide Guide to remove.
  \warning   Guide \a guide must exist.

  The data referenced by \a guide is removed from memory if \a guide is the last
  guide pointing to it.
*/
template<class T>
void DataObjectBase<T>::remove(
         DataGuide const& guide)
{
  assert(isValid(guide));

  // WARNING: It is not unlikely that the guide passed in is the result of
  // dereferencing an iterator into the guide collection we are about to
  // modify. Make a copy of the object to be able to work with it, even after
  // the original is erased.
  DataGuide copyOfGuide(guide);

  // First remove the guide from the manager.
  _manager.remove(copyOfGuide);
  assert(!isValid(copyOfGuide));

  // Check if there're still data guides pointing to the data.
  if(!_manager.exists(copyOfGuide.address())) {

    // Find the tuple with the data to remove.
    tuple_iter it = find(static_cast<const T*>(copyOfGuide.address()));
    assert(it != _tuples.end());

    // Delete data and tuple.
    delete boost::tuples::get<2>(*it).data();
    _tuples.erase(it);

    // Check removal. No loose ends wanted.
    it = find(static_cast<const T*>(copyOfGuide.address()));
    assert(it == _tuples.end());
  }
}



template<class T>
size_t DataObjectBase<T>::size() const
{
  return _manager.size();
}



template<class T>
bool DataObjectBase<T>::empty() const
{
  return _manager.empty();
}



template<class T>
bool DataObjectBase<T>::isValid(
         DataGuide const& guide) const
{
  return _manager.isValid(guide);
}



template<class T>
std::string DataObjectBase<T>::name(
         DataGuide const& guide) const
{
  assert(isValid(guide));

  const_tuple_iter it = find(static_cast<const T*>(guide.address()));
  assert(it != _tuples.end());

  return boost::tuples::get<0>(*it);
}



template<class T>
T& DataObjectBase<T>::data(
         DataGuide const& guide)
{
  return _manager.data(guide);
}



template<class T>
const T& DataObjectBase<T>::data(
         DataGuide const& guide) const
{
  return _manager.data(guide);
}



template<class T>
typename DataObjectBase<T>::data_iter DataObjectBase<T>::data_begin()
{
  return _manager.data_begin();
}



template<class T>
typename DataObjectBase<T>::const_data_iter DataObjectBase<T>::data_begin() const
{
  return _manager.data_begin();
}



template<class T>
typename DataObjectBase<T>::data_iter DataObjectBase<T>::data_end()
{
  return _manager.data_end();
}



template<class T>
typename DataObjectBase<T>::const_data_iter DataObjectBase<T>::data_end() const
{
  return _manager.data_end();
}



template<class T>
typename DataObjectBase<T>::const_guide_iter DataObjectBase<T>::guides_begin() const
{
  return _manager.guides_begin();
}



template<class T>
typename DataObjectBase<T>::const_guide_iter DataObjectBase<T>::guides_end() const
{
  return _manager.guides_end();
}



template<class T>
DataGuide const& DataObjectBase<T>::dataGuide(
         geo::DataGuide const& guide) const
{
  return _manager.dataGuide(guide);
}



template<class T>
DataInfo<T> const* DataObjectBase<T>::dataInfo(
         DataGuide const& guide) const
{
  const_tuple_iter it = find(static_cast<const T*>(guide.address()));
  assert(it != _tuples.end());

  return &boost::tuples::get<2>(*it);
}



template<class T>
void DataObjectBase<T>::read(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address)
{
  for(data_iter it = this->data_begin(); it != this->data_end(); ++it) {
    dal::DataSpace dataSpaceOfData((*it).data()->dataSpace());

    if(!(space.hasScenarios() && dataSpaceOfData.hasScenarios())) {
      (*it).data()->read(space, address);
    }
    else {
      // Current data item is part of a scenario.
      size_t index = dataSpaceOfData.indexOf(dal::Scenarios);
      assert(dataSpaceOfData.dimension(index).nrCoordinates() == 1);
      std::string scenario(dataSpaceOfData.dimension(index).template value<std::string>(0));
      index = space.indexOf(dal::Scenarios);
      assert(space.dimension(index).containsExactValue(scenario));

      if(address.coordinate<std::string>(index) == scenario) {
        // Only call read if the scenario of the data equals the scenario in
        // the address.
        (*it).data()->read(space, address);
      }
    }
  }
}



template<class T>
bool DataObjectBase<T>::isRead(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  for(const_data_iter it = this->data_begin(); it != this->data_end(); ++it) {
    if(!dynamic_cast<Dataset const*>((*it).data())->isRead(space, address)) {
      return false;
    }
  }

  return true;
}

} // namespace ag



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



