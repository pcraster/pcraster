#include "stddefx.h"
#include "calc_MemoryExchangeItemField.h"
#include "calc_field.h"
#include <utility>

/*!
  \file
  This file contains the implementation of the MemoryExchangeItemField class.
*/


namespace calc
{

// Code that is private to this module.
namespace detail
{

}  // namespace detail

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYEXCHANGEITEMFIELD MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF MEMORYEXCHANGEITEMFIELD MEMBERS
//------------------------------------------------------------------------------

MemoryExchangeItemField::MemoryExchangeItemField()
{
}

MemoryExchangeItemField::MemoryExchangeItemField(std::string const &name, size_t memoryId,
                                                 std::shared_ptr<Field> value)
    : MemoryExchangeItem(name, memoryId), d_value(std::move(value))
{
}

MemoryExchangeItemField::~MemoryExchangeItemField()
{
}

void *MemoryExchangeItemField::rawValue() const
{
  return (void *)d_value->src();
}

//! copy Field buffer into into dest.
void MemoryExchangeItemField::beMemCpySrc(void *dest) const
{
  d_value->beMemCpySrc(dest);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

}  // namespace calc
