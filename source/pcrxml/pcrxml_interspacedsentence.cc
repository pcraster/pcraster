#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_INTERSPACEDSENTENCE
#include "pcrxml_interspacedsentence.h"
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif


/*!
  \file
  This file contains the implementation of the InterSpacedSentence class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC INTERSPACEDSENTENCE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF INTERSPACEDSENTENCE MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::InterSpacedSentence::InterSpacedSentence(const QDomNode& owningElement, const std::string& name, bool required):
  String(owningElement,name,required)
{
  validate();
}

pcrxml::InterSpacedSentence& pcrxml::InterSpacedSentence::operator=(const std::string& value)
{
  setPresent(true);
  d_value=value;
  validate();
  return *this;
}

//! ctor
pcrxml::InterSpacedSentence::InterSpacedSentence()
{
}

//! dtor
pcrxml::InterSpacedSentence::~InterSpacedSentence()
{
}

void pcrxml::InterSpacedSentence::validate() const
{
  if (!present())
    return;
  std::string testCopy(value());
  com::removeFrontEndSpace(testCopy);
  if (testCopy != value() || testCopy.empty())
    throw com::BadStreamFormat("not a pcrxml::InterSpacedSentence");
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



