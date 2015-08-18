#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LEXINPUTSOURCE
#include "calc_lexinputsource.h"
#define INCLUDED_CALC_LEXINPUTSOURCE
#endif

// Library headers.
// PCRaster library headers.
#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
// Module headers.
#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif
#ifndef INCLUDED_CALC_LEXINPUTSOURCETEXTSCRIPT
#include "calc_lexinputsourcetextscript.h"
#define INCLUDED_CALC_LEXINPUTSOURCETEXTSCRIPT
#endif
#ifndef INCLUDED_CALC_POSITIONTEXT
#include "calc_positiontext.h"
#define INCLUDED_CALC_POSITIONTEXT
#endif

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
  d_fileName.reset(new std::string(fileName));
}

calc::LexInputSource::LexInputSource()
{
}


calc::LexInputSource::~LexInputSource()
{
}


//! Install script given as option (-f)
/*!
 */
calc::LexInputSource *calc::createLexInputSourceFromFile(const com::PathName& pn)
{
  MAP *m;
  if ( (m = Mopen(pn.toString().c_str(),M_READ)) != NULL) {
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
