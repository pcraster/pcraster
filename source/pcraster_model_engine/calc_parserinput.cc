#include "stddefx.h"
#include "calc_parserinput.h"
#include "tokens.h"
#include "ATokenBuffer.h"
#include "lexgrammar.h"
#include "calc_lexinput.h"
#include "calc_lexinputcreator.h"

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

calc::ParserInput::ParserInput(const com::PathName &scriptFile)
{
  init();
  try {
    d_lexInput = new LexInput();
    // oohps must be in this order
    d_lexInput->installFileScript(scriptFile);
    setup();
  } catch (...) {
    clean();
    throw;
  }
}

calc::ParserInput::ParserInput(const std::string &script)
{
  init();
  try {
    d_lexInput = new LexInput();
    // oohps must be in this order
    d_lexInput->installStringScript(script.c_str());
    setup();
  } catch (...) {
    clean();
    throw;
  }
}

//! initialize with by creator
calc::ParserInput::ParserInput(const LexInputCreator &lic)
{
  init();
  try {
    d_lexInput = lic.createLexInput();
    setup();
  } catch (...) {
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
  d_tokenBuffer = nullptr;
  d_lexer = nullptr;
  d_lexInput = nullptr;
}

void calc::ParserInput::setup()
{
  PRECOND(d_lexInput);
  d_lexer = new LexGrammar(*d_lexInput);
  d_tokenBuffer = new ANTLRTokenBuffer(d_lexer);
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
