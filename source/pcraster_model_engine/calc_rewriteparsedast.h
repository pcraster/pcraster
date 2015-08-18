#ifndef INCLUDED_CALC_REWRITEPARSEDAST
#define INCLUDED_CALC_REWRITEPARSEDAST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



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
