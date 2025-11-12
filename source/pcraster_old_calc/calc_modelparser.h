#ifndef INCLUDED_CALC_MODELPARSER
#define INCLUDED_CALC_MODELPARSER

#include "stddefx.h"
#include "tokens.h"
#include "Parser.h"


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

  /* virtual */    ~ModelParser              () override;

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
