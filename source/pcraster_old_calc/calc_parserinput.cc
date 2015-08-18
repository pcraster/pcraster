#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_PARSERINPUT
#include "calc_parserinput.h"
#define INCLUDED_CALC_PARSERINPUT
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_TOKENS
#include "tokens.h"
#define INCLUDED_TOKENS
#endif
#ifndef INCLUDED_ATOKENBUFFER
#include "ATokenBuffer.h"
#define INCLUDED_ATOKENBUFFER
#endif

// Module headers.
#ifndef INCLUDED_LEXGRAMMAR
#include "lexgrammar.h"
#define INCLUDED_LEXGRAMMAR
#endif
#ifndef INCLUDED_CALC_LEXINPUT
#include "calc_lexinput.h"
#define INCLUDED_CALC_LEXINPUT
#endif



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
  d_tokenBuffer=0;
  d_lexer=0;
  d_lexInput=0;
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



