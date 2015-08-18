#ifndef INCLUDED_CALC_CFGCREATOR
#define INCLUDED_CALC_CFGCREATOR

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.

namespace calc {
  // CFGCreator declarations.
  class ASTNode;
  class CFGNode;
}


namespace calc {

//! creates and auto deletes
struct ScopedCFG {
  CFGNode *cfg;
  ScopedCFG(ASTNode *n);
  ScopedCFG(CFGNode *n);
 ~ScopedCFG();
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

CFGNode *createCFG(ASTNode *n);
CFGNode *createCFG(const std::vector<ASTNode *>& nodeVector);


} // namespace calc

#endif
