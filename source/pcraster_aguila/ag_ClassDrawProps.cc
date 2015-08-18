#include "ag_ClassDrawProps.h"



/*!
  \file
  This file contains the implementation of the ClassDrawProps class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     p Palette.
*/
ag::ClassDrawProps::ClassDrawProps(
         std::string const& title,
         com::RawPalette const* p)

  : DrawProps(title, p)

{
}



ag::ClassDrawProps::ClassDrawProps(const ClassDrawProps& properties)

  : DrawProps(properties)

{
}



//! Destructor.
/*!
  The palette is for use only and is not deleted here.
*/
ag::ClassDrawProps::~ClassDrawProps()
{
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


