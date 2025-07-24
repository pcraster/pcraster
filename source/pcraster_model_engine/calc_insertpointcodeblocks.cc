#include "stddefx.h"
#include "calc_insertpointcodeblocks.h"
#include "calc_pointcodeblockreplacer.h"
#include "calc_usedefanalyzer.h"

namespace calc {

}

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! insert PointCodeBlock nodes and replace statements with them in \a a
/*!
 * \par a AST that is modified
 * \par aCFG CFG of a that will have its lastUse attrs updated
 *
 * on return aCFG is no longer valid, since a is updated.
 * the lastUse attrs are also invalid
 *
 */
std::vector<calc::PointCodeBlock *> calc::insertPointCodeBlocks(
    ASTSymbolTable const& symbols,
    ASTNode *a,
    CFGNode *aCFG)
{
  // needed on the (larger) global cfg in computing PointCodeBlock::d_output
  //  with newLiveDefSet on the sub cfg of the PointCodeBlock
  setLastUse(aCFG);

  PointCodeBlockReplacer pcbr(symbols);
  a->accept(pcbr);


  return pcbr.list();
}
