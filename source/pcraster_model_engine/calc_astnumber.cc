#include "stddefx.h"
#include "calc_astnumber.h"
#include "com_strconv.h"
#include "appargs.h"  // AppInputDirection
#include "calc_nonspatial.h"
#include "calc_astvisitor.h"
#include "calc_datatype.h"
#include "calc_datatypeclash.h"
#include "calc_id.h"

/*!
  \file
  This file contains the implementation of the ASTNumber class.
*/


//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTNUMBER MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF ASTNUMBER MEMBERS
//------------------------------------------------------------------------------

void calc::ASTNumber::init()
{
  PRECOND(isNumber());
  d_value = toNumber();
  PRECOND(nrReturns() == 1);
  returnDataType(0) = DataType(vsOfNumber(d_value), ST_NONSPATIAL);
}

calc::ASTNumber::ASTNumber()
{
}

calc::ASTNumber::ASTNumber(const Id &id) : ASTId(id), d_strRepr(id.name())
{
  init();
}

calc::ASTNumber::ASTNumber(const std::string &name) : ASTId(name), d_strRepr(name)
{
  init();
}

//! ctor cast like "scalar(v)"
calc::ASTNumber::ASTNumber(const Id &castFunctionName, VS castDestination, const Id &v)
    : ASTId(v), d_strRepr(castFunctionName.name())
{
  init();
  try {
    returnDataType(0).restrict(DataType(castDestination, false));
  } catch (VSClash) {
    /* pcrcalc/test41a,test42 */
    std::ostringstream msg;
    msg << "Illegal conversion applied: '" << value() << "' is not a valid '"
        << toString(castDestination) + "' value";
    castFunctionName.posError(msg);
  }
  if (castDestination == VS_D)  // pcrcalc/test71
    d_value = AppInputDirection(d_value);
}

calc::ASTNumber::~ASTNumber()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::ASTNumber& calc::ASTNumber::operator=(const ASTNumber& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! Copy constructor.
calc::ASTNumber::ASTNumber(const ASTNumber &rhs)
    : ASTId(rhs), d_strRepr(rhs.d_strRepr), d_value(rhs.d_value)
{
}

void calc::ASTNumber::accept(ASTVisitor &v)
{
  v.visitNumber(this);
}

calc::Field *calc::ASTNumber::createNonSpatial() const
{
  return new NonSpatial(vs(), value());
}

std::string calc::ASTNumber::qName() const
{
  return "'" + d_strRepr + "'";
}

calc::ASTNumber *calc::ASTNumber::createClone() const
{
  return new ASTNumber(*this);
}

double calc::ASTNumber::value() const
{
  return d_value;
}

VS calc::ASTNumber::vs() const
{
  return returnDataType(0).vs();
}

std::string calc::ASTNumber::strRepr() const
{
  return d_strRepr;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
