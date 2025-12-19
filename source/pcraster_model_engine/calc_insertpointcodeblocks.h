#ifndef INCLUDED_CALC_INSERTPOINTCODEBLOCKS
#define INCLUDED_CALC_INSERTPOINTCODEBLOCKS

#include "stddefx.h"

#include <vector>
#include <iostream>



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
