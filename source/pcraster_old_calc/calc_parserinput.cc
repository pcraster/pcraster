#include "stddefx.h"
#include "calc_parserinput.h"
#include "tokens.h"
#include "ATokenBuffer.h"
#include "lexgrammar.h"
#include "calc_lexinput.h"



/*!
  \file
  This file contains the implementation of the ParserInput class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ParserInputPrivate
{
public:

  ParserInputPrivate()
  {
  }

  ~ParserInputPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PARSERINPUT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PARSERINPUT MEMBERS
//------------------------------------------------------------------------------

calc::ParserInput::ParserInput(const com::PathName& scriptFile)
{
 init();
 try {
  d_lexInput    = new LexInput();
  d_lexInput->installFileScript(scriptFile);

  d_lexer       = new LexGrammar(*d_lexInput);
  d_tokenBuffer = new ANTLRTokenBuffer(d_lexer);
 } catch(...) {
   clean();
   throw;
 }
}

calc::ParserInput::ParserInput(const std::string& script)
{
 init();
 try {
  d_lexInput    = new LexInput();
  d_lexInput->installStringScript(script.c_str());

  d_lexer       = new LexGrammar(*d_lexInput);
  d_tokenBuffer = new ANTLRTokenBuffer(d_lexer);
 } catch(...) {
   clean();
   throw;
 }
}

//! initialize with existing lexInput
calc::ParserInput::ParserInput(LexInput& lexInput)
{
 init();
 try {
  d_lexer       = new LexGrammar(lexInput);
  d_tokenBuffer = new ANTLRTokenBuffer(d_lexer);
 } catch(...) {
   clean();
   throw;
 }
}


calc::ParserInput::~ParserInput()
{
  clean();
}

void calc::ParserInput::clean()
{
  delete d_tokenBuffer;
  delete d_lexer;
  delete d_lexInput;
  init();
}

void calc::ParserInput::init()
{
  d_tokenBuffer=nullptr;
  d_lexer=nullptr;
  d_lexInput=nullptr;
}

ANTLRTokenBuffer *calc::ParserInput::tokenBuffer() const
{
  return d_tokenBuffer;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



