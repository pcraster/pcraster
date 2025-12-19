#ifndef INCLUDED_CALC_REWRITEPARSEDAST
#define INCLUDED_CALC_REWRITEPARSEDAST

#include "stddefx.h"



namespace calc {
  // rewriteParsedAST declarations.
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

void checkAndRewriteParsedAST(ASTNode *n);


} // namespace calc

#endif
