#ifndef INCLUDED_CALC_ASTEXPR
#define INCLUDED_CALC_ASTEXPR

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif

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

  virtual         ~ASTExpr               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             transferFunctionArgs  (ASTNodeVector *args);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const Operator&       op                () const;
  void                  exec              (RunTimeEnv* rte)  const;

  ASTExpr*              createClone       () const;

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
