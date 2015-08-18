#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_DOCTYPE
#include "pcrxml_doctype.h"
#endif



/*!
  \file
  This file contains the implementation of the DocType class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOCTYPE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DOCTYPE MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::DocType::DocType(const std::string& documentElementName):
  d_documentElementName(documentElementName)
{
}

//! dtor
pcrxml::DocType::~DocType()
{
}

//! string with xml version and DOCTYPE notation
std::string pcrxml::DocType::asString() const
{
  std::string str("<?xml version='1.0' encoding='ascii'?>\n");
  str+="<!DOCTYPE "+d_documentElementName+
       " PUBLIC '-//PCRaster//Application 0.01//EN' 'pcraster.dtd'>\n"; // ext ID
  /* internal DTD
   * str+="[\n"+d_pcrInternalDtd;
   * str+="]\n\n";
   */
  return str;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



