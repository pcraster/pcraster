#ifndef INCLUDED_CALC_BLOCKENTRANCE
#define INCLUDED_CALC_BLOCKENTRANCE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif



namespace calc {
  // BlockEntrance declarations.
}



namespace calc {


class BasicBlock;

/*!
   node that is visited when entering a BasicBlock
   More than one ASTNode may point to this node in a CFGNode setup.
   In compiler terms this is the begin of a BasicBlock, like a label in C
*/
class BlockEntrance : public ASTNode
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  BlockEntrance&           operator=           (const BlockEntrance& rhs);

  //! Copy constructor.
                   BlockEntrance               (const BlockEntrance& rhs);

  //! not owned
  BasicBlock*      d_block;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BlockEntrance               (BasicBlock* block);

  /* virtual */    ~BlockEntrance              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void accept(ASTVisitor& v);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  BasicBlock*      block               () const;

  BlockEntrance   *createClone                 () const;

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
