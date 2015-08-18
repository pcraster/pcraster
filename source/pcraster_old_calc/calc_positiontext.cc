#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_POSITIONTEXT
#include "calc_positiontext.h"
#define INCLUDED_CALC_POSITIONTEXT
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

// Module headers.
#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif

/*!
  \file
  This file contains the implementation of the PositionText class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC POSITIONTEXT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF POSITIONTEXT MEMBERS
//------------------------------------------------------------------------------

calc::PositionText::PositionText(
    StringSharedPtr fileName,
    int lineNr,int charNr):
  d_fileName(fileName),d_lineNr(lineNr),d_charNr(charNr)
{
}

calc::PositionText::PositionText(const PositionText& pt):
 Position(pt),
 d_fileName(pt.d_fileName),
 d_lineNr(pt.d_lineNr),
 d_charNr(pt.d_charNr)
{
}

calc::PositionText::~PositionText()
{
}

//! generate error message that is prefixed with the position
/*!
   \throws calc::PosException
 */
void calc::PositionText::throwError(const std::string& inMsg) const
{
  // clean up and make sure it ends with a new line
  std::string msg(inMsg);
  com::removeFrontEndSpace(msg);
  msg+='\n';

#ifdef DEBUG_DEVELOP
  /* only automic initizialization does this
   * meaning, we have an program generated
   * symbol, not an user symbol
   * So this indicates a programming error
   */
  POSTCOND(d_lineNr > 0 && d_charNr > 0);
#endif
  throw PosException(*d_fileName,d_lineNr,d_charNr,msg);
}

calc::PositionText*  calc::PositionText::createClone() const
{
  return new PositionText(*this);
}

//! return format: "line 'LineNr:ColNr'"
std::string calc::PositionText::text() const
{
  std::ostringstream dp;
  dp << "line '" << d_lineNr << ":" << d_charNr << "'";
  return dp.str();
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



