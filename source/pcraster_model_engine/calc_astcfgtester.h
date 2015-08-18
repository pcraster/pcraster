#ifndef INCLUDED_CALC_ASTCFGTESTER
#define INCLUDED_CALC_ASTCFGTESTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif
#ifndef INCLUDED_CALC_CFGNODE
#include "calc_cfgnode.h"
#define INCLUDED_CALC_CFGNODE
#endif

namespace calc {
  // ASTCFGTester declarations.
}



namespace calc {

//! Only used in testing, for simple cleanup
class ASTCFGTester
{

private:

  typedef std::auto_ptr<ASTNode> A;
  typedef std::auto_ptr<CFGNode> C;

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
