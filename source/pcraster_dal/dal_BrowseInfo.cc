#ifndef INCLUDED_DAL_BROWSEINFO
#include "dal_BrowseInfo.h"
#define INCLUDED_DAL_BROWSEINFO
#endif

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the BrowseInfo class.
*/

namespace {

} // Anonymous namespace



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BROWSEINFO MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BROWSEINFO MEMBERS
//------------------------------------------------------------------------------

BrowseInfo::BrowseInfo()

  : _datasetType(NR_DATASET_TYPES),
    _typeId(TI_NR_TYPES),
    _valueScale(VS_NOTDETERMINED)

{
}



BrowseInfo::BrowseInfo(
         std::string const& name,
         DataSpace const& space,
         DatasetType datasetType,
         TypeId typeId,
         CSF_VS valueScale,
         std::string const& driverName)

  : _name(name),
    _space(space),
    _datasetType(datasetType),
    _typeId(typeId),
    _valueScale(valueScale),
    _driverName(driverName)

{
}



BrowseInfo::BrowseInfo(
         BrowseInfo const& rhs)

  : _name(rhs._name),
    _space(rhs._space),
    _datasetType(rhs._datasetType),
    _typeId(rhs._typeId),
    _valueScale(rhs._valueScale),
    _driverName(rhs._driverName)

{
}



BrowseInfo& BrowseInfo::operator=(
         BrowseInfo const& rhs)
{
  if(this != &rhs) {
    _name = rhs._name;
    _space = rhs._space;
    _datasetType = rhs._datasetType;
    _typeId = rhs._typeId;
    _valueScale = rhs._valueScale;
    _driverName = rhs._driverName;
  }

  return *this;
}



BrowseInfo::~BrowseInfo()
{
}



std::string const& BrowseInfo::name() const
{
  return _name;
}



DataSpace const& BrowseInfo::space() const
{
  return _space;
}



DatasetType BrowseInfo::datasetType() const
{
  return _datasetType;
}



TypeId BrowseInfo::typeId() const
{
  return _typeId;
}



CSF_VS BrowseInfo::valueScale() const
{
  return _valueScale;
}



std::string const& BrowseInfo::driverName() const
{
  return _driverName;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

