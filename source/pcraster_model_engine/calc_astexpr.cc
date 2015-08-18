#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTEXPR
#include "calc_astexpr.h"
#define INCLUDED_CALC_ASTEXPR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif

#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif

/*!
  \file
  This file contains the implementation of the ASTExpr class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ASTExprPrivate
{
public:

  ASTExprPrivate()
  {
  }

  ~ASTExprPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTEXPR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTEXPR MEMBERS
//------------------------------------------------------------------------------

/*!
 * \param opNameAsParsed the name of the operator as parsed, like the operator
 *   in the user preffered syntax (or,|| etc).
 * \param op the operator
 */
calc::ASTExpr::ASTExpr(
  const std::string& opNameAsParsed, const Operator& op):
  BaseExpr(opNameAsParsed), d_op(op)
{
  setNrReturns(op.nrResults());
}

calc::ASTExpr::ASTExpr(
  const Position *pos, const Operator& op):
  BaseExpr(pos,op.name()),
  d_op(op)
{
  setNrReturns(op.nrResults());
}

calc::ASTExpr::~ASTExpr()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::ASTExpr& calc::ASTExpr::operator=(const ASTExpr& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! Copy constructor. used in createClone()
calc::ASTExpr::ASTExpr(const ASTExpr& rhs):
  BaseExpr(rhs),
  d_op(rhs.d_op)
{
}

const calc::Operator& calc::ASTExpr::op() const
{
  return d_op;
}

calc::ASTExpr* calc::ASTExpr::createClone() const
{
  return new ASTExpr(*this);
}

void calc::ASTExpr::exec(RunTimeEnv* rte)  const
{
  op().exec(rte,nrArgs());
}

//! transfer all nodes of \a args to  argument list for this function
/*
 * transferArg or transferFunctionArgs must NEVER be called after this call
 * since we do some rewriting for cover/min/max
 *
 * \param al is transfered upon return
 */
void calc::ASTExpr::transferFunctionArgs(ASTNodeVector *al)
{
  // variable number of arguments in point operations
  // by AST rewritting

  bool rewrite;
  switch(op().opCode()) {
    case OP_COVER: case OP_MIN: case OP_MAX:
     rewrite = al->size()>2; break;
    default : rewrite=false;
  }

  if (!rewrite) {
      BaseExpr::transferFunctionArgs(al);
  } else {
      // rewrite  f(a0,a1,a2,...,aN)
      //  as f(f(f(a0,a1),a2) ...),aN), cover needs that order
      //  most-outer f is this object

      // transfer all of args into created expr tree
      std::deque<ASTNode *>  args = al->release();
      delete al;

       // f(a0,a1)
       ASTExpr *prev =new ASTExpr(op().name(),op());
       prev->transferArg(args[0]);
       prev->transferArg(args[1]);
       // f(f(...),a2)
       for(size_t a=2; a < args.size()-1; ++a) {
         ASTExpr *e = new ASTExpr(op().name(),op());
         e->transferArg(prev);
         e->transferArg(args[a]);
         prev=e;
       }
       // most-outer f
       transferArg(prev);
       transferArg(args.back()); // aN
 }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
