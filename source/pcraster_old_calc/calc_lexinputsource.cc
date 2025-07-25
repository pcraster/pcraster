#include "stddefx.h"
#include "calc_lexinputsource.h"
#include "csf.h"
#include "com_exception.h"
#include "com_pathname.h"
#include "calc_quote.h"
#include "calc_lexinputsourcetextscript.h"
#include "calc_positiontext.h"

#include <memory>

/*!
  \file
  This file contains the implementation of the LexInputSource class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC LEXINPUTSOURCE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LEXINPUTSOURCE MEMBERS
//------------------------------------------------------------------------------

calc::LexInputSource::LexInputSource(const std::string& fileName)
{
  d_fileName = std::make_shared<std::string>(fileName);
}

calc::LexInputSource::LexInputSource()
{
}


calc::LexInputSource::~LexInputSource()
{
}


//! Install script given as option (-f)
/*! tests wether it is a plain script
 */
calc::LexInputSource *calc::createLexInputSourceFromFile(const com::PathName& pn)
{
  MAP *m = nullptr;
  if ( (m = Mopen(pn.toString().c_str(),M_READ)) != nullptr) {
    Mclose(m);
    throw com::Exception(quote(pn.toString())+" is a map, not a script file");
    // argscalc/test28
  }
  ResetMerrno();

  return new LexInputSourceTextScript(pn);
}

calc::PositionText* calc::LexInputSource::createPositionText(size_t lineNr, size_t charNr)const
{
  return new PositionText(d_fileName,lineNr,charNr);
}
