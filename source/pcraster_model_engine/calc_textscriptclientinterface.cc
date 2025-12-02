#include "stddefx.h"
#include "calc_textscriptclientinterface.h"
#include "calc_completeparser.h"
#include "calc_astscript.h"

/*!
  \file
  This file contains the implementation of the TextScriptClientInterface class.
*/


namespace calc
{

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

TextScriptClientInterface::TextScriptClientInterface(const std::string &scriptFileOrContents,
                                                     bool asFile)
    : ClientInterface(scriptFileOrContents, asFile)
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
ASTScript *TextScriptClientInterface::createScriptAndAnalyzeNoContext()
{
  std::unique_ptr<ASTScript> script;
  if (d_asFile) {
    CompleteParser<ASTScript, com::PathName> sp(d_scriptFileOrContents);
    script.reset(sp.parseScript());
  } else {
    CompleteParser<ASTScript, std::string> sp(d_scriptFileOrContents);
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


}  // namespace calc
