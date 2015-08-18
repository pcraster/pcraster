#include "ag_Dataset.h"

// Library headers.

// PCRaster library headers.
#include "dal_DataSpaceAddressMapper.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the Dataset class.
*/



namespace ag {

//------------------------------------------------------------------------------

/*
class DatasetPrivate
{
public:

  DatasetPrivate()
  {
  }

  ~DatasetPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATASET MEMBERS
//------------------------------------------------------------------------------

Dataset::Dataset(
         std::string const& name,
         dal::DataSpace const& space)

  : _name(name),
    _source(name, space),
    _localToWorldMapper(0),
    _globalToLocalMapper(0)

{
  assert(!name.empty());

  initialiseLocalToWorldMapper(_source.dataSpace());
  initialiseGlobalToLocalMapper(_source.dataSpace());
}



Dataset::~Dataset()
{
  delete _localToWorldMapper;
  delete _globalToLocalMapper;
}



// void Dataset::setDataSpace(
//          dal::DataSpace const& space)
// {
//   _space = space;
// 
//   if(!_localToWorldMapper) {
//     initialiseDataSpaceAddressMapper();
//   }
// }



std::string const& Dataset::name() const
{
  return _name;
}



void Dataset::initialiseLocalToWorldMapper(
         dal::DataSpace const& space)
{
  delete _localToWorldMapper;
  _localToWorldMapper = new dal::DataSpaceAddressMapper(space);
}



void Dataset::initialiseGlobalToLocalMapper(
         dal::DataSpace const& space)
{
  delete _globalToLocalMapper;
  _globalToLocalMapper = new dal::DataSpaceAddressMapper(space);
}



dal::DataSpace const& Dataset::dataSpace() const
{
  return _source.dataSpace();
}


dal::DataSpaceAddressMapper const& Dataset::localToWorldMapper() const
{
  assert(_localToWorldMapper);

  return *_localToWorldMapper;
}



dal::DataSpaceAddressMapper& Dataset::localToWorldMapper()
{
  assert(_localToWorldMapper);

  return *_localToWorldMapper;
}



dal::DataSpaceAddressMapper const& Dataset::globalToLocalMapper() const
{
  assert(_globalToLocalMapper);

  return *_globalToLocalMapper;
}



dal::DataSpaceAddressMapper& Dataset::globalToLocalMapper()
{
  assert(_globalToLocalMapper);

  return *_globalToLocalMapper;
}



dal::DataSpaceAddress Dataset::localAddress(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  dal::DataSpaceAddress result(address);

  // Trim address to data space of data source.
  result = _source.dataSpace().trim(space, result);

  // Translate global coordinates to local coordinates.
  result = globalToLocalMapper().destination(result);

  return result;
}



dal::DataSource& Dataset::dataSource()
{
  return _source;
}



dal::DataSource const& Dataset::dataSource() const
{
  return _source;
}



bool Dataset::isRead(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  return isRead(localAddress(space, address));
}



void Dataset::setSelectedValue(
         REAL4 value)
{
  if(!hasSelectedValue() || !dal::comparable(selectedValue(), value)) {
    _selectedValue = value;

    // Make sure new data is read by invalidating the current data.
    _addressRead = dataSource().dataSpace().address();
  }
}



REAL4 Dataset::selectedValue() const
{
  assert(hasSelectedValue());

  return boost::any_cast<REAL4>(_selectedValue);
}



bool Dataset::hasSelectedValue() const
{
  return !_selectedValue.empty();
}



void Dataset::unsetSelectedValue()
{
  if(hasSelectedValue()) {
    _selectedValue = boost::any();

    // Make sure new data is read by invalidating the current data.
    _addressRead = dataSource().dataSpace().address();
  }
}



/// boost::any const& Dataset::selectedValue() const
/// {
///   return _selectedValue;
/// }



dal::DataSpaceAddress const& Dataset::addressRead() const
{
  return _addressRead;
}



void Dataset::setAddressRead(
         dal::DataSpaceAddress const& address)
{
  _addressRead = address;
}



void Dataset::setExtremes(
         boost::any const& min,
         boost::any const& max)
{
  _min = min;
  _max = max;
}



bool Dataset::allMV() const
{
  assert((_min.empty() && _max.empty()) || (!_min.empty() && !_max.empty()));
  return _min.empty();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

