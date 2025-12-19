#ifndef INCLUDED_CALC_GENERATEPOINTCODEBODY
#define INCLUDED_CALC_GENERATEPOINTCODEBODY

#include "stddefx.h"
#include "calc_parset.h"

#include <iostream>


namespace calc {
  // GeneratePointCodeBody declarations.
  class ASTNode;
}



namespace calc {

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

// defined in calc_pointcodebodygenerator.cc
void generatePointCodeBody(
      std::ostream& s,
      ASTNode*      code,
      const ParSet& vContents);

} // namespace calc

#endif
