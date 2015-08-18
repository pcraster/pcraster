#ifndef INCLUDED_CALC_CFGVISITOR
#define INCLUDED_CALC_CFGVISITOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif


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

  void             visitStat                (ASTStat*    s);
  void             visitExpr                (BaseExpr*    e);
  void             visitNonAssExpr          (NonAssExpr* e);

  //! what branch is taken? Forward (default) or Back
  bool             d_takeBackBranch;

  CFGNode*         d_cfg;
  CFGNode*         d_current;


protected:
  void             setTakeBackBranch   (bool takeBackBranch);
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CFGVisitor               (CFGNode* cfg);

  /* virtual */    ~CFGVisitor              ();

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
