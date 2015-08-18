#ifndef INCLUDED_CALC_JUMPNODE
#define INCLUDED_CALC_JUMPNODE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif


namespace calc {
  // JumpNode declarations.
}



namespace calc {

class BasicBlock;
class RunTimeEnv;

/*!
   node that is visited an the end of a BasicBlock, within this node the
   (conditional) jump is made.
   A JumpNode has 2 d_succ (next) nodes to visit in a CFGNode setup.
   In compiler terms this is just before the end like before the label in C
*/
class JumpNode : public ASTNode
{
public:
  typedef          std::set<std::string>  DeletesOnForward;
private:

  //! Assignment operator. NOT IMPLEMENTED.
  JumpNode&           operator=           (const JumpNode& rhs);

  //! Copy constructor. NOT IMPLEMENTED
                   JumpNode               (const JumpNode& rhs);

  //! this is jump at end of this block, ptr not owned
  BasicBlock*      d_block;

  /*! these parameter will have their value deleted in DataTable
      when this JumpNode does take the forward branch.
   */
  DeletesOnForward d_deletesOnForward;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   JumpNode               (BasicBlock* block);

  /* virtual */    ~JumpNode              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  virtual void       accept               (ASTVisitor& v);

  void               addDeleteOnForward   (const std::string& parName);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  BasicBlock*             block               () const;
  const DeletesOnForward& deletesOnForward    () const;
  void                    deleteForwards      (RunTimeEnv& rte) const;

  JumpNode               *createClone         () const;

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
