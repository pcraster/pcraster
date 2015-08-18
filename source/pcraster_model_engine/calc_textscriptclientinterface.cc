#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_TEXTSCRIPTCLIENTINTERFACE
#include "calc_textscriptclientinterface.h"
#define INCLUDED_CALC_TEXTSCRIPTCLIENTINTERFACE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_COMPLETEPARSER
#include "calc_completeparser.h"
#define INCLUDED_CALC_COMPLETEPARSER
#endif
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif


/*!
  \file
  This file contains the implementation of the TextScriptClientInterface class.
*/



namespace calc {

//------------------------------------------------------------------------------

/*
class TextScriptClientInterfacePrivate
{
public:

  TextScriptClientInterfacePrivate()
  {
  }

  ~TextScriptClientInterfacePrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEXTSCRIPTCLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TEXTSCRIPTCLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------

TextScriptClientInterface::TextScriptClientInterface(
    const std::string& scriptFileOrContents,
    bool asFile):
     ClientInterface(scriptFileOrContents,asFile)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
TextScriptClientInterface::TextScriptClientInterface(
         TextScriptClientInterface const& rhs)

  : Base(rhs)

{
}
*/



TextScriptClientInterface::~TextScriptClientInterface()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
TextScriptClientInterface& TextScriptClientInterface::operator=(
         TextScriptClientInterface const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/

//! parse textual version of model.
ASTScript* TextScriptClientInterface::createScriptAndAnalyzeNoContext()
{
  std::auto_ptr<ASTScript> script;
  if (d_asFile) {
   CompleteParser<ASTScript,com::PathName> sp(d_scriptFileOrContents);
   script.reset(sp.parseScript());
  } else {
   CompleteParser<ASTScript,std::string> sp(d_scriptFileOrContents);
   script.reset(sp.parseScript());
  }
  script->analyzeNoContext();
  return script.release();
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

