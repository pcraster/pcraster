#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif

// Library headers.

// PCRaster library headers.

#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif

#ifndef INCLUDED_APPARGS
# include "appargs.h"    // AppInputDirection
#define INCLUDED_APPARGS
#endif

// Module headers.

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif

#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif
#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif



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
  d_value=toNumber();
  PRECOND(nrReturns()==1);
  returnDataType(0)=DataType(vsOfNumber(d_value),ST_NONSPATIAL);
}

calc::ASTNumber::ASTNumber()
{
}


calc::ASTNumber::ASTNumber(
  const Id& id):
   ASTId(id),
   d_strRepr(id.name())
{
  init();
}

calc::ASTNumber::ASTNumber(
  const std::string& name):
   ASTId(name),
   d_strRepr(name)
{
  init();
}

//! ctor cast like "scalar(v)"
calc::ASTNumber::ASTNumber(
    const Id& castFunctionName,
    VS        castDestination,
    const Id &v):
   ASTId(v),
   d_strRepr(castFunctionName.name())
{
  init();
  try {
    returnDataType(0).restrict(DataType(castDestination, false));
  } catch (VSClash) {
   /* pcrcalc/test41a,test42 */
   std::ostringstream msg;
   msg << "Illegal conversion applied: '" <<
    value() <<"' is not a valid '"<<toString(castDestination)+"' value";
   castFunctionName.posError(msg);
  }
  if (castDestination == VS_D) // pcrcalc/test71
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
calc::ASTNumber::ASTNumber(const ASTNumber& rhs):
  ASTId(rhs),
  d_strRepr(rhs.d_strRepr),
  d_value(rhs.d_value)
{
}

void calc::ASTNumber::accept(ASTVisitor& v)
{
  v.visitNumber(this);
}

calc::Field* calc::ASTNumber::createNonSpatial() const
{
  return new NonSpatial(vs(), value());
}

std::string calc::ASTNumber::qName() const
{
  return "'"+d_strRepr+"'";
}

calc::ASTNumber* calc::ASTNumber::createClone() const
{
  return new ASTNumber(*this);
}

double calc::ASTNumber::value() const
{ return d_value; }

VS calc::ASTNumber::vs() const
{ return returnDataType(0).vs(); }

std::string calc::ASTNumber::strRepr() const
{ return d_strRepr; }

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



