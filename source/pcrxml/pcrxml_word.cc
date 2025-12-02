#include "stddefx.h"
#include "pcrxml_word.h"
#include "com_strlib.h"
#include "com_exception.h"

/*!
  \file
  This file contains the implementation of the Word class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC WORD MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF WORD MEMBERS
//------------------------------------------------------------------------------

//! construct from element
pcrxml::Word::Word(const QDomNode &owningElement, const std::string &name, bool required)
    : String(owningElement, name, required)
{
  if (!present()) {
    return;
  }
  std::string testCopy(value());
  com::removeAllSpace(testCopy);
  if (testCopy != value() || testCopy.empty()) {
    throw com::BadStreamFormat("not a pcrxml::Word");
  }
}

//! ctor from a string
pcrxml::Word::Word(const std::string &value) : String(value)
{
}

//! ctor from a string
pcrxml::Word::Word(const char *value) : String(value)
{
}

//! ctor
pcrxml::Word::Word()
{
}

//! dtor
pcrxml::Word::~Word()
{
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
