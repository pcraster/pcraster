#include "stddefx.h"
#include "pcrxml_interspacedsentence.h"
#include "com_strlib.h"
#include "com_exception.h"


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



