#ifndef INCLUDED_CALC_MODELPARSER
#define INCLUDED_CALC_MODELPARSER

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.


// Module headers.
#ifndef INCLUDED_PARSER
#include "tokens.h"
#include "Parser.h"
#define INCLUDED_PARSER
#endif


namespace calc {
  // ModelParser declarations.
  class ParserInput;
  class Script;
}


namespace calc {


//! Interface to the ANLTR generated class Parser, that consumes entire model in one time
class ModelParser : public Parser
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ModelParser&           operator=           (const ModelParser&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ModelParser               (const ModelParser&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ModelParser               (ParserInput& pi,
                                              Script& script);

  /* virtual */    ~ModelParser              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
