#include "stddefx.h"

#ifndef INCLUDED_CALC_FIELDTYPE
#include "calc_fieldtype.h"
#define INCLUDED_CALC_FIELDTYPE
#endif

#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif


#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

calc::SyntaxArgumentError::SyntaxArgumentError(
  const std::string& s): d_s(s)
{
}

calc::SyntaxVsClash::SyntaxVsClash(
  const std::string& oldVs,
  const std::string& newVs):
  d_oldVs(oldVs),d_newVs(newVs)
{
}

calc::SyntaxStClash::SyntaxStClash(
  const std::string& oldSt,
  const std::string& newSt):
 d_oldSt(oldSt),d_newSt(newSt)
{
}

calc::FieldType::FieldType(VS vs, ST st) :
   d_vs(vs),d_stType(st)
  //! can there be switched between spatial and non-spatial
{
  // also in case of ST_DERIVED start with non-spatial state
  d_spatial = (d_stType == ST_SPATIAL);
}

calc::FieldType::FieldType(const calc::Operator& o):
  d_vs(o.vs()),d_stType(o.st())
{
  // also in case of ST_DERIVED start with non-spatial state
  d_spatial = (d_stType == ST_SPATIAL);
}

//! Set a new vs
/*! newVsSet can have 1 or more Vs's, newVsSet should always
    be a subset of the current Vs set. <BR>
    returns the new vs (set)
 */
VS calc::FieldType::setVs(VS newVs)
{
  PRECOND(isSubset(newVs,d_vs));
  d_vs = intersect(d_vs,newVs);
  POSTCOND(d_vs != VS_UNKNOWN);
  return d_vs;
}

//! this can return a set > 2 !
VS calc::FieldType::vs() const 
{ return d_vs; }

static void argVsError(
  const calc::Operator&o,
  VS allowedVs,
  VS currentVs,
  int nr,       /* wrong argument nr */
  int offsetArg)
{
  std::ostringstream msg;
    /* CW a position range in the script would be nice, now
     * just the operator itself
     * pcrcalc/test10c
     */
  /* pcrcalc/test10[bc] */
  msg << o.strArg(nr+offsetArg) <<": type is ";
  msg << toString(currentVs) << ", legal type is "<< toString(allowedVs);
  throw calc::SyntaxArgumentError(msg.str());
}

static void argStError(
  const calc::Operator& o,
  int nr,       /* wrong argument nr        */
  int offsetArg)
{
  std::string legal, illegal;

  /* SYNTAX_OPER never have a strict spatial type
     and table-types are already catched by the vs check
     so the only conflict possible is nonspatial vs. spatial
     in functions 
   */
   if (o.argSt(nr) == ST_SPATIAL)
   { legal = "spatial"; illegal = "non-spatial"; }
   else
   {
     PRECOND(o.argSt(nr) == ST_NONSPATIAL);
          illegal = "spatial"; legal = "non-spatial";
   }
  std::ostringstream msg;
  msg <<  "argument nr. "<<nr+1+offsetArg;
  msg <<" of function '"<<o.name()<<"' is "<<illegal<<" only "<<legal<<" allowed";
  throw calc::SyntaxArgumentError(msg.str());
}

void calc::FieldType::restrictSystem(VS vsNewPossible, bool spatialByArgs)
{
#ifdef DEBUG
  POSTCOND(vs() != VS_UNKNOWN);
  if (spatialByArgs)
    POSTCOND(d_stType != ST_NONSPATIAL);
  else
    POSTCOND(d_stType != ST_SPATIAL);
#endif
  setVs(intersect(vs(),vsNewPossible));
  if (!d_spatial) // promote or keep
    d_spatial = spatialByArgs;
}

//! return true is spatial promotion occured
// only called on assignment!
bool calc::FieldType::restrictUser(VS vsNewPossible, bool spatialByAssignment)
{
  VS newVs = intersect(vs(),vsNewPossible);
  if (newVs == VS_UNKNOWN)
    throw calc::SyntaxVsClash(toString(vs()),toString(vsNewPossible));
  setVs(newVs);
  POSTCOND(vs() != VS_UNKNOWN);

  if (d_stType == ST_NONSPATIAL && spatialByAssignment)
    throw calc::SyntaxStClash("nonspatial","spatial");
        // NOTE assigning nonspatial to spatial is solved run-time
  bool promotion=false;
  if (!d_spatial) {  // promote or keep
    promotion = spatialByAssignment;
    d_spatial = spatialByAssignment;
  }
  return promotion;
}

void calc::FieldType::restrictArg(const calc::Operator& o, int argNr, int offsetArg)
{
  /* narrow possible types by restriction on the operand */
  VS newVs = intersect(vs(), o.argVs(argNr));
  if (newVs == VS_UNKNOWN)
    argVsError(o,o.argVs(argNr),vs(),argNr,offsetArg);
  setVs(newVs);

  /* check spatial type pcrcalc/test{258,346} */
  if ((o.argSt(argNr) == ST_NONSPATIAL &&  d_spatial) ||
      (o.argSt(argNr) == ST_SPATIAL    && !d_spatial) )
        argStError(o,argNr,offsetArg);
}

bool  calc::FieldType::spatialDerived() const
{
  return d_stType == ST_DERIVED;
}
bool  calc::FieldType::spatial() const
{
  return d_spatial;
}

void calc::FieldType::print(calc::InfoScript& i, const std::string& tag) const
{
  std::string str = (d_spatial ? "spatial" : "nonSpatial");
  i.stream() << "\n<A HREF=\"#" << tag << "\" onmouseover=showtype(\"" 
    << str << "\")>" << tag << "</A>"  ;
}

void calc::FieldType::print(calc::InfoScript& i, const calc::Operator& o) const
{
 print(i,o.name());
}

void calc::FieldType::print(calc::InfoScript& i) const
{
  i.stream() << (d_spatial ? "spatial" : "nonSpatial") << "<BR>";
}
