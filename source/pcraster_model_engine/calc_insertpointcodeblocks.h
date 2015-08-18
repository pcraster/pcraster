#ifndef INCLUDED_CALC_INSERTPOINTCODEBLOCKS
#define INCLUDED_CALC_INSERTPOINTCODEBLOCKS

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.



namespace calc {
  // insertPointCodeBlocks declarations.
  class CFGNode;
  class ASTNode;
  class PointCodeBlock;
  class ASTSymbolTable;
}



namespace calc {



std::vector<PointCodeBlock *> insertPointCodeBlocks(
  ASTSymbolTable const& symbols,
  ASTNode *a,
  CFGNode *aCFG);

void   createPointCodeBlockDll(
       std::ostream& s,
       const std::vector<PointCodeBlock *>& l);

} // namespace calc

#endif
