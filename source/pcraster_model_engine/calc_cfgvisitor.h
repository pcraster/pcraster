#ifndef INCLUDED_CALC_CFGVISITOR
#define INCLUDED_CALC_CFGVISITOR

#include "stddefx.h"
#include "calc_astvisitor.h"



namespace calc {
  // CFGVisitor declarations.
  class CFGNode;
}


namespace calc {



//! Visit and execute in CFG order
/*!
 * CFG is a chain instead of a tree like an AST
 */
class CFGVisitor: private ASTVisitor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CFGVisitor&           operator=           (const CFGVisitor&);

  //! Copy constructor. NOT IMPLEMENTED.
                   CFGVisitor               (const CFGVisitor&);

  void             visitStat                (ASTStat*    s) override;
  void             visitExpr                (BaseExpr*    e) override;
  void             visitNonAssExpr          (NonAssExpr* e) override;

  //! what branch is taken? Forward (default) or Back
  bool             d_takeBackBranch{false};

  CFGNode*         d_cfg;
  CFGNode*         d_current{nullptr};


protected:
  void             setTakeBackBranch   (bool takeBackBranch);
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CFGVisitor               (CFGNode* cfg);

  /* virtual */    ~CFGVisitor              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual void visit                        ();

  void         reset                        ();
  void         visitCurrent                 ();
  void         advance                      ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool             takeBackBranch      () const;
  ASTNode*         current             () const;

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
