#ifndef INCLUDED_CALC_NONASSEXPR
#define INCLUDED_CALC_NONASSEXPR



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
  // NonAssExpr declarations.
}



namespace calc {



//! A non assigned expression
/*!
   Most expressions (e.g. ASTExpr) have their return value(s) used in an assignment
   or as input to an other expression. A NonAssExpr is an expression that
   has its single return value evaluated in another way.
   Like the condition RepeatUntil::d_condition, or the fileoutput construction from the previous
   pcrcalc version.
*/
class NonAssExpr : public ASTNode
{

private:

  NonAssExpr&           operator=           (NonAssExpr const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   NonAssExpr               (NonAssExpr const& rhs);

  //! the expression
  ASTNode*         d_expr;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   NonAssExpr               (ASTNode *transferedExpr);

  /* virtual */    ~NonAssExpr              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void              accept            (ASTVisitor& v);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ASTNode*          createClone        ()const;
  ASTNode*          expr               ()const;

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
