#include "stddefx.h"


#ifndef INCLUDED_CALC_CONSTANT
#include "calc_constant.h"
#define INCLUDED_CALC_CONSTANT
#endif

#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
#include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
# include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif

#ifndef INCLUDED_APPARGS
# include "appargs.h"    // AppInputDirection
#define INCLUDED_APPARGS
#endif

calc::Constant::Constant(
  const Symbol& name)
 : FieldExpr(name),
   d_strRepr(name.name()),
   d_value(com::fromString<double>(name.name())),
   d_type(vsOfNumber(d_value),ST_NONSPATIAL)
{
}

//! ctor cast like "scalar(v)"
calc::Constant::Constant(
    const Symbol& castFunctionName,
    VS            castDestination,
    const Symbol &v):
   FieldExpr(castFunctionName),
   d_strRepr(castFunctionName.name()),
   d_value(v.toNumber()),
   d_type(vsOfNumber(d_value),ST_NONSPATIAL)
{
    try {
     d_type.restrictUser(castDestination, false);
    } catch (SyntaxVsClash) {
     /* pcrcalc/test41a,test42 */
     std::ostringstream msg;
     msg << "Illegal conversion applied: '" <<
      value() <<"' is not a valid '"<<toString(castDestination)+"' value";
     posError(msg);
    }
    if (castDestination == VS_D) // pcrcalc/test71
     d_value = AppInputDirection(d_value);
}

calc::FieldType& calc::Constant::restrictType()
{
  return d_type;
}

void calc::Constant::buildTypes()
{
  // doesn't have to do anything, type already set when created
}

void calc::Constant::buildTypesRecursive(VS resultVsSet)
{
  if (isSubset(resultVsSet,vs())) // expr is polymorphic
    d_type.restrictSystem(resultVsSet,false);
}

void calc::Constant::prepareExecution()
{
}

void calc::Constant::skipExecution()
{
}

void calc::Constant::execute(FieldStack& f)
{
  f.push(new NonSpatial(d_type.vs(),d_value));
}

void calc::Constant::print(InfoScript& i)const
{
  d_type.print(i,qName());
}

bool calc::Constant::isConstant() const
{
  return true;
}

std::string calc::Constant::qName() const
{
  return "'"+d_strRepr+"'";
}
