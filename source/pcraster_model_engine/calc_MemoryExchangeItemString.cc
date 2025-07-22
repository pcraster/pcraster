#include "stddefx.h"
#include "calc_MemoryExchangeItemString.h"
#include <cstring>


/*!
  \file
  This file contains the implementation of the MemoryExchangeItemString class.
*/



namespace calc {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYEXCHANGEITEMSTRING MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYEXCHANGEITEMSTRING MEMBERS
//------------------------------------------------------------------------------

MemoryExchangeItemString::MemoryExchangeItemString()
{
}

MemoryExchangeItemString::MemoryExchangeItemString(
  std::string const& name,
  size_t memoryId,
  std::string const& value):
    MemoryExchangeItem(name,memoryId),
    d_value(value)
{
}



MemoryExchangeItemString::~MemoryExchangeItemString()
{
}


void* MemoryExchangeItemString::rawValue() const
{
  return (void *)d_value.c_str();
}

//! copy c_str() representation of value into dest.
void MemoryExchangeItemString::beMemCpySrc(void *dest) const
{
 const char *src = d_value.c_str();
 std::strcpy((char *)dest, src);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc

