#ifndef INCLUDED_CALC_ASTCFGTESTER
#define INCLUDED_CALC_ASTCFGTESTER

#include "stddefx.h"
#include "calc_astnode.h"
#include "calc_cfgnode.h"

#include <memory>


namespace calc {
  // ASTCFGTester declarations.
}



namespace calc {

//! Only used in testing, for simple cleanup
class ASTCFGTester
{

private:

  typedef std::unique_ptr<ASTNode> A;
  typedef std::unique_ptr<CFGNode> C;

  A d_ast;
  C d_cfg;

  //! Assignment operator. NOT IMPLEMENTED.
  ASTCFGTester&           operator=           (const ASTCFGTester& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ASTCFGTester               (const ASTCFGTester& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ASTCFGTester               (ASTNode *a);

  /* virtual */    ~ASTCFGTester              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void                setAST                  (ASTNode *a);
  void                setCFG                  (CFGNode *c);
  void                rebuildCFG              ();
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ASTNode*            ast                     () const;
  CFGNode*            cfg                     () const;

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
