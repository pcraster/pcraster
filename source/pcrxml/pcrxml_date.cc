#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_DATE
#include "pcrxml_date.h"
#endif



/*!
  \file
  This file contains the implementation of the Date class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATE MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::Date::Date(const QDomNode& owningElement, const std::string& name, bool required):
   String(owningElement,name,required)
{
}

//! ctor
pcrxml::Date::Date()
{
}

//! dtor
pcrxml::Date::~Date()
{
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



