#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h"
#endif

#ifndef  INCLUDED_QDOM
#include <qdom.h>
#define  INCLUDED_QDOM
#endif

#ifndef INCLUDED_PCRXML_STRINGCONV
#include "pcrxml_stringconv.h"
#define INCLUDED_PCRXML_STRINGCONV
#endif

/*!
  \file
  This file contains the implementation of the String class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC STRING MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STRING MEMBERS
//------------------------------------------------------------------------------

//! ctor from DOM Tree
pcrxml::String::String(const QDomNode& owningElement, const std::string& name, bool required):
  Attribute(owningElement,name,required)
{
  if (!present())
    return;
  d_value = inputValueStr(owningElement, name);
}

//! ctor from a string
pcrxml::String::String(const std::string& value):
  Attribute(true),
  d_value(value)
{
}

//! ctor from a string
pcrxml::String::String(const char* value):
  Attribute(true),
  d_value(value)
{
}

//! default ctor, attribute not present
pcrxml::String::String():
  Attribute(false)
{
}

/* identical to  String  operator=(const String&) by ctor
pcrxml::String& pcrxml::String::operator=(const std::string& value)
{
  setPresent(true);
  d_value=value;
  return *this;
}
*/

//! dtor
pcrxml::String::~String()
{
}

std::string pcrxml::String::attrValueStr() const
{
  return value();
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



