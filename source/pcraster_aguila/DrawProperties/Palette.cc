#ifndef INCLUDED_PALETTE
#include "Palette.h"
#define INCLUDED_PALETTE
#endif

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Palette class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PALETTE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PALETTE MEMBERS
//------------------------------------------------------------------------------

Palette::Palette()

  : std::vector<QColor>()

{
}



Palette::~Palette()
{
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

