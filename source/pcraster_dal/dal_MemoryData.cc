#include "dal_MemoryData.h"



/*!
  \file
  This file contains the implementation of the MemoryData class.
*/



namespace dal {

//------------------------------------------------------------------------------

/*
class MemoryDataPrivate
{
public:

  MemoryDataPrivate()
  {
  }

  ~MemoryDataPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYDATA MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYDATA MEMBERS
//------------------------------------------------------------------------------

MemoryData::MemoryData()
{
}



//! Copy constructor.
MemoryData::MemoryData(
         MemoryData const& /* rhs */)
{
}



MemoryData::~MemoryData()
{
}



//! Assignment operator.
MemoryData& MemoryData::operator=(
         MemoryData const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

