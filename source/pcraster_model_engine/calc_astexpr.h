#ifndef INCLUDED_CALC_ASTEXPR
#define INCLUDED_CALC_ASTEXPR

#include "stddefx.h"
#include "calc_baseexpr.h"


namespace calc {
  // ASTExpr declarations.
}

namespace calc {

class Operator;
class RunTimeEnv;
class ASTNodeVector;


//! expression holding a fixed operation
//!
/*!
 * Fixed operation means that:
 *  - the operation type is known at compile time
 *  - is fully treated with fixed built-in rules for type building and execution.
 *  - needs no specific state in the ASTNode except for knowing its arguments
 */
class ASTExpr : public BaseExpr
{

private:
  friend class ASTTestFactory;

  //! Assignment operator.
  ASTExpr&           operator=           (const ASTExpr& rhs);

  //! Copy constructor.
                   ASTExpr               (const ASTExpr& rhs);

  //! the "fixed" operator
  const Operator&       d_op;

                   ASTExpr               (const std::string& opNameAsParsed, const Operator& op);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
                   ASTExpr               (const Position *pos, const Operator& op);

          ~ASTExpr               () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             transferFunctionArgs  (ASTNodeVector *args) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const Operator&       op                () const override;
  void                  exec              (RunTimeEnv* rte)  const override;

  ASTExpr*              createClone       () const override;

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
