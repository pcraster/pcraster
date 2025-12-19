#ifndef INCLUDED_CALC_BASICBLOCK
#define INCLUDED_CALC_BASICBLOCK

#include "stddefx.h"
#include "calc_astnode.h"



namespace calc {
  // BasicBlock declarations.
}



namespace calc {


class BlockEntrance;
class JumpNode;
class ASTNodeList;

/*!
 * formally a maximal basic block, but BasicBlock will do as name.
 * A basic block consists of a BlockEntrance, statements as an ASTNodeList and JumpNode
 */
class BasicBlock : public ASTNode
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  BasicBlock&           operator=           (const BasicBlock& rhs);


  BlockEntrance*   d_blockEntrance;
  ASTNodeList*     d_statements{nullptr};
  JumpNode*        d_jumpNode;


protected:
  //! Copy constructor. for createClone
                   BasicBlock               (const BasicBlock& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BasicBlock               (const Position*    beginOfBlock,
                                             BlockEntrance*    transferredBlockEntrance,
                                             ASTNode*          transferredStatements,
                                             JumpNode*         transferredJumpNode);

  /* virtual */    ~BasicBlock              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void               transferPushBack       (ASTNode *n);
  void       accept                 (ASTVisitor& v) override;
  //! will call v.enter"BasicBlockSubClass"(this)
  virtual void       callEnter              (ASTVisitor& v)=0;
  //! will call v.jumpOut"BasicBlockSubClass"(this)
  virtual void       callJump               (ASTVisitor& v)=0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  BlockEntrance*   blockEntrance       () const;
  ASTNode*         statements          () const;
  JumpNode*        jumpNode            () const;

  virtual bool     hasBackBranch       () const=0;

  void        addDeleteOnForward       (const std::string& parName) const;

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
