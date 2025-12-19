#ifndef INCLUDED_CALC_NONASSEXPR
#define INCLUDED_CALC_NONASSEXPR

#include "stddefx.h"
#include "calc_astnode.h"



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

  /* virtual */    ~NonAssExpr              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void              accept            (ASTVisitor& v) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  ASTNode*          createClone        ()const override;
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
