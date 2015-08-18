#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
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
  This file contains the implementation of the BaseExpr class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class BaseExprPrivate
{
public:

  BaseExprPrivate()
  {
  }

  ~BaseExprPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC BASEEXPR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BASEEXPR MEMBERS
//------------------------------------------------------------------------------

/*!
 * \param opNameAsParsed the name of the operator as parsed, like the operator
 *   in the user preffered syntax (or,|| etc).
 * \param op the operator
 */
calc::BaseExpr::BaseExpr(
  const std::string& opNameAsParsed):
  ASTId(opNameAsParsed),
  d_args(new ASTNodeVector())
{
  setNrReturns(0);
}

calc::BaseExpr::BaseExpr(
  const Position *pos,
  const std::string& opName):
  ASTId(Id(opName,pos)),
  d_args(new ASTNodeVector())
{
  setNrReturns(0);
}

calc::BaseExpr::~BaseExpr()
{
  delete d_args;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::BaseExpr& calc::BaseExpr::operator=(const BaseExpr& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! Copy constructor. used in createClone()
calc::BaseExpr::BaseExpr(const BaseExpr& rhs):
  ASTId(rhs),
  d_args(rhs.d_args->createClone())
{
}


void calc::BaseExpr::accept(ASTVisitor& v)
{
   v.visitExpr(this);
}

size_t calc::BaseExpr::nrArgs() const
{
  return d_args->size();
}

calc::ASTNode* calc::BaseExpr::arg(size_t i) const
{
  return d_args->at(i);
}

calc::ASTNodeVector* calc::BaseExpr::args() const
{
  return d_args;
}

//! add argument to end of argument list
void calc::BaseExpr::transferArg(ASTNode *arg) {
  d_args->transferPushBack(arg);
}

std::vector<calc::DataType> calc::BaseExpr::dataTypeArgs() const
{
 std::vector<DataType> da;
 for(size_t i=0;i<nrArgs(); ++i)
   da.push_back(arg(i)->returnDataType());
 return da;
}

//! transfer all nodes of \a args to  argument list for this function
/*
 * transferArg or transferFunctionArgs must NEVER be called after this call
 * since we do some rewriting for cover/min/max
 *
 * \param al is transfered upon return
 */
void calc::BaseExpr::transferFunctionArgs(ASTNodeVector *al)
{
  delete d_args;
  d_args = al;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
