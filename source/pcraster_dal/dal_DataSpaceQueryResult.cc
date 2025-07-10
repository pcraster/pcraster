#include "dal_DataSpaceQueryResult.h"



/*!
  \file
  This file contains the implementation of the DataSpaceQueryResult class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASPACEQUERYRESULT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATASPACEQUERYRESULT MEMBERS
//------------------------------------------------------------------------------

DataSpaceQueryResult::DataSpaceQueryResult()

   

{
}



DataSpaceQueryResult::DataSpaceQueryResult(
         std::string const& name,
         DatasetType datasetType,
         DataSpace const& space,
         DataSpaceAddress const& address)

  : d_name(name),
    d_datasetType(datasetType),
    d_space(space),
    d_address(address)

{
}



DataSpaceQueryResult::~DataSpaceQueryResult()
{
}



DataSpaceQueryResult::operator bool() const
{
  return d_datasetType != NR_DATASET_TYPES;
}



std::string const& DataSpaceQueryResult::name() const
{
  return d_name;
}



DatasetType DataSpaceQueryResult::datasetType() const
{
  return d_datasetType;
}



DataSpace const& DataSpaceQueryResult::space() const
{
  return d_space;
}



DataSpaceAddress const& DataSpaceQueryResult::address() const
{
  return d_address;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

