#include "pcrxsd_StringDomInput.h"

/*!
  \file
  This file contains the implementation of the StringDomInput class.
*/


namespace pcrxsd
{

// Code that is private to this module.
namespace detail
{

}  // namespace detail

//------------------------------------------------------------------------------
// DEFINITION OF STATIC STRINGDOMINPUT MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STRINGDOMINPUT MEMBERS
//------------------------------------------------------------------------------

StringDomInput::StringDomInput(std::string const &contents, bool validate, EntityResolverType erType)
    : DOMInput(erType)
{
  setString(contents);
  setValidate(validate);
}

StringDomInput::~StringDomInput()
{
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

}  // namespace pcrxsd
