#include "ag_DataManager.h"
#include <boost/bind.hpp>
#include <boost/foreach.hpp>
#include "dev_Algorithm.h"



/*!
  \file
  This file contains the implementation of the DataManager class.
*/



namespace {

template<class T>
struct addressOfDataGuideIs:
         public std::binary_function<ag::DataGuide, T const*, bool> {

  bool operator()(
         ag::DataGuide const& guide,
         T const* address) const
  {
    return guide.address() == address;
  }
};

template<class T>
struct addressOfDataInfoIs:
         public std::binary_function<ag::DataInfo<T>, T const*, bool> {

  bool operator()(
         ag::DataInfo<T> const& info,
         T const* address) const
  {
    return info.data() == address;
  }
};

} // Anonymous namespace



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

namespace ag {

//! Constructor.
/*!
  \param     dataType Data type of the data to manage.
*/
template<class T>
DataManager<T>::DataManager(
         geo::DataType dataType)

  : _dataType(dataType)

{
  assert(isConsistent());
}



//! Destructor.
/*!
*/
template<class T>
DataManager<T>::~DataManager()
{
  // Pointers are for use only.
}



//! Check consistency of data and guides.
/*!
  \return    true if everything is OK.
  \sa        isConsistent(const DataGuide&)
*/
template<class T>
bool DataManager<T>::isConsistent() const
{
  bool result = true;

  // Check if the number of guides >= the number of data items.
  if(_guides.size() < _data.size()) {
    result = false;
  }

  // Check consistency of all guides.
  BOOST_FOREACH(DataGuide const& guide, _guides) {
    if(!isConsistent(guide)) {
      result = false;
      break;
    }
  }

  // TODO make sure all data is pointed to by at least one guide.

  return result;
}



//! Checks consistency of \a guide
/*!
  \param     guide DataGuide to check.
  \return    true if \a guide is consistent.
  \sa        isConsistent()
*/
template<class T>
bool DataManager<T>::isConsistent(
         DataGuide const& guide) const
{
  assert(guide.isValid());

  bool result = true;

  // Check if data guide exists.
  if(!exists(guide)) {
    result = false;
  }
  // Check if index of data guide is within valid range.
  else if(guide.index() >= _guides.size()) {
    result = false;
  }
  // Check if index of data guide is right.
  else if(_guides[guide.index()] != guide) {
    result = false;
  }
  // Check if data of data guide exists.
  else if(!exists(guide.address())) {
    result = false;
  }

  return result;
}



template<class T>
typename DataManager<T>::guide_iter DataManager<T>::findGuide(
       DataGuide::Address const& address)
{
  return std::find_if(_guides.begin(), _guides.end(),
         boost::bind(addressOfDataGuideIs<T>(), _1,
              static_cast<T const*>(address)));
}



template<class T>
typename DataManager<T>::const_data_iter DataManager<T>::findData(
         DataGuide::Address const& address) const
{
  return std::find_if(_data.begin(), _data.end(),
         boost::bind(addressOfDataInfoIs<T>(), _1,
              static_cast<T const*>(address)));
}



template<class T>
typename DataManager<T>::data_iter DataManager<T>::findData(
         DataGuide::Address const& address)
{
  return std::find_if(_data.begin(), _data.end(),
         boost::bind(addressOfDataInfoIs<T>(), _1,
              static_cast<T const*>(address)));
}



//! Checks if data with address \a a exists.
/*!
  \param     address Address to check.
  \return    true if \a a exists.
  \sa        exists(const DataGuide&)
*/
template<class T>
bool DataManager<T>::exists(
         DataGuide::Address const& address) const
{
  return findData(address) != _data.end();
}



//! Checks if data guide \a guide exists.
/*!
  \param     guide Data guide to check.
  \return    true if \a guide exists.
  \sa        exists(DataGuide::Address)
*/
template<class T>
bool DataManager<T>::exists(
         DataGuide const& guide) const
{
  return dev::hasElement(_guides, guide);
}



template<class T>
DataGuide DataManager<T>::add(
         DataInfo<T> const& info)
{
  assert(isConsistent());

  // Let's see if we already know about the data in info.
  const_data_iter it = std::find(_data.begin(), _data.end(), info);

  if(it == _data.end()) {
    // No, store the info object for it.
    _data.push_back(info);
  }

  // It should be there now.
  assert(exists(static_cast<DataGuide::Address const&>(info.data())));

  DataGuide result;

  // Let's see if we already have a data guide for this data set.
  guide_iter guide_it = findGuide(info.data());

  if(guide_it != _guides.end()) {
    // Yes, return existing data guide.
    result = *guide_it;
  }
  else {
    // No, create a new guide for this data.
    result = DataGuide(_guides.size(),
         static_cast<DataGuide::Address>(info.data()),
         _dataType, info.valueScale());
    _guides.push_back(result);
  }

  assert(isConsistent());

  return result;
}



//! Removes \a guide from the guide collection.
/*!
  \param     guide Guide to remove.

  If \a guide is the last guide pointing to some data, then the address to
  the data is removed (data is not deleted) from the manager.
*/
template<class T>
void DataManager<T>::remove(
         DataGuide const& guide)
{
  assert(isConsistent());

  // WARNING: It is not unlikely that the guide passed in is the result of
  // dereferencing an iterator into the guide collection we are about to
  // modify. Make a copy of the object to be able to work with it, even after
  // the original is erased.
  DataGuide copyOfGuide(guide);

  // Remove the guide.
  guide_iter guide_it = _guides.erase(_guides.begin() + copyOfGuide.index());

  // Adjust indices of guides after erased one.
  while(guide_it != _guides.end()) {
    (*guide_it).setIndex((*guide_it).index() - 1);
    ++guide_it;
  }

  // Check if any guides are pointing to the data of the erased guide.
  guide_it = findGuide(copyOfGuide.address());

  if(guide_it == _guides.end()) {
    // None of the remaining guides are pointing to the data of the erased
    // guide. Remove the information about the data too.
    data_iter data_it = findData(copyOfGuide.address());
    assert(data_it != _data.end());
    (void)_data.erase(data_it);
    assert(findGuide(copyOfGuide.address()) == _guides.end());
    assert(findData(copyOfGuide.address()) == _data.end());
  }

  assert(isConsistent());
}



template<class T>
T& DataManager<T>::data(
         DataGuide const& guide)
{
  assert(exists(guide));
  assert(exists(guide.address()));

  data_iter it = findData(guide.address());
  assert(it != _data.end());

  return *(*it).data();
}



//! Returns the data pointed to by \a guide.
/*!
  \param     guide Guide to the data.
  \return    Data.
*/
template<class T>
const T& DataManager<T>::data(
         DataGuide const& guide) const
{
  assert(exists(guide));
  assert(exists(guide.address()));

  const_data_iter it = findData(guide.address());
  assert(it != _data.end());

  return *(*it).data();
}



//! Returns the number of data objects are managed.
/*!
  \return    Number of data objects.
*/
template<class T>
size_t DataManager<T>::size() const
{
  return _data.size();
}



template<class T>
bool DataManager<T>::empty() const
{
  return _data.empty();
}



//! Returns true if \a guide is consistent with the manager's internals.
/*!
  \return    true if valid.
  \warning   guide must be a valid object.
  \sa        isConsistent(), DataGuide::isValid()
*/
template<class T>
bool DataManager<T>::isValid(
         DataGuide const& guide) const
{
  return isConsistent(guide);
}



template<class T>
typename DataManager<T>::data_iter DataManager<T>::data_begin()
{
  return _data.begin();
}



template<class T>
typename DataManager<T>::const_data_iter DataManager<T>::data_begin() const
{
  return _data.begin();
}



template<class T>
typename DataManager<T>::data_iter DataManager<T>::data_end()
{
  return _data.end();
}



template<class T>
typename DataManager<T>::const_data_iter DataManager<T>::data_end() const
{
  return _data.end();
}



template<class T>
typename DataManager<T>::const_guide_iter DataManager<T>::guides_begin() const
{
  return _guides.begin();
}



template<class T>
typename DataManager<T>::const_guide_iter DataManager<T>::guides_end() const
{
  return _guides.end();
}



template<class T>
std::set<size_t> DataManager<T>::timeSteps(
         DataGuide const& guide) const
{
  assert(exists(guide));
  assert(exists(guide.address()));

  const_data_iter it = findData(guide.address());
  assert(it != _data.end());

  return dal::timeSteps((*it).space());
}



template<class T>
std::set<size_t> DataManager<T>::timeSteps() const
{
  std::set<size_t> steps, tmpSteps;

  BOOST_FOREACH(DataInfo<T> const& info, _data) {
    tmpSteps = dal::timeSteps(info.space());
    std::set_union(
         steps.begin(), steps.end(),
         tmpSteps.begin(), tmpSteps.end(),
         std::inserter(steps, steps.begin()));
  }

  return steps;
}



template<class T>
const DataGuide& DataManager<T>::dataGuide(
         geo::DataGuide const& guide) const
{
  assert(guide.index() < _guides.size());
  return _guides[guide.index()];
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

} // namespace ag
