#ifndef INCLUDED_CALC_PARSERINPUT
#define INCLUDED_CALC_PARSERINPUT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  class PathName;
}

class ANTLRTokenBuffer;

namespace calc {

class LexGrammar;
class LexInputCreator;
class LexInput;


//! Input source for class Parser
/*!
   Combines a class LexInput and the LexGrammar
*/
class ParserInput
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ParserInput&           operator=           (const ParserInput&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ParserInput               (const ParserInput&);

  LexInput         *d_lexInput;
  LexGrammar       *d_lexer;
  ANTLRTokenBuffer *d_tokenBuffer;

  void              init                     ();
  void              setup                    ();
  void              clean                    ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ParserInput               (const com::PathName& scriptFile);
                   ParserInput               (const std::string& script);
                   ParserInput               (const LexInputCreator& lic);

  /* virtual */   ~ParserInput               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ANTLRTokenBuffer *tokenBuffer              () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
