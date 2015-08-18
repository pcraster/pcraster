#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEMFIELD
#include "calc_MemoryExchangeItemField.h"
#define INCLUDED_CALC_MEMORYEXCHANGEITEMFIELD
#endif

// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif


/*!
  \file
  This file contains the implementation of the MemoryExchangeItemField class.
*/



namespace calc {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYEXCHANGEITEMFIELD MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYEXCHANGEITEMFIELD MEMBERS
//------------------------------------------------------------------------------

MemoryExchangeItemField::MemoryExchangeItemField()
{
}

MemoryExchangeItemField::MemoryExchangeItemField(
  std::string const& name,
  size_t memoryId,
  boost::shared_ptr<Field> value):
    MemoryExchangeItem(name,memoryId),
    d_value(value)
{
}


MemoryExchangeItemField::~MemoryExchangeItemField()
{
}

void* MemoryExchangeItemField::rawValue() const
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

} // namespace calc

