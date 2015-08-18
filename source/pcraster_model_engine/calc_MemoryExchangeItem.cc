#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEM
#include "calc_MemoryExchangeItem.h"
#define INCLUDED_CALC_MEMORYEXCHANGEITEM
#endif

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the MemoryExchangeItem class.
*/



namespace calc {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYEXCHANGEITEM MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYEXCHANGEITEM MEMBERS
//------------------------------------------------------------------------------

MemoryExchangeItem::MemoryExchangeItem()
{
}

MemoryExchangeItem::MemoryExchangeItem(
  std::string const& name,
  size_t id):
  d_name(name),
  d_id(id)
{}

MemoryExchangeItem::~MemoryExchangeItem()
{
}

size_t MemoryExchangeItem::id() const
{
  return d_id;
}

std::string const& MemoryExchangeItem::name() const
{
 return d_name;
}

//! if base instantiated 0 return
void* MemoryExchangeItem::rawValue() const
{
 return 0;
}

//! should not be called, only sub classed ones (guard by rawValue !=0)
void MemoryExchangeItem::beMemCpySrc(void *) const
{
 PRECOND(false);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc

