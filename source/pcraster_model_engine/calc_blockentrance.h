#ifndef INCLUDED_CALC_BLOCKENTRANCE
#define INCLUDED_CALC_BLOCKENTRANCE

#include "stddefx.h"
#include "calc_astnode.h"



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
  BasicBlock*      d_block{};

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BlockEntrance               (BasicBlock* block);

  /* virtual */    ~BlockEntrance              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void accept(ASTVisitor& v) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  BasicBlock*      block               () const;

  BlockEntrance   *createClone                 () const override;

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
