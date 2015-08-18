#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_POINTCODEBODYGENERATOR
#include "calc_pointcodebodygenerator.h"
#define INCLUDED_CALC_POINTCODEBODYGENERATOR
#endif
#ifndef INCLUDED_CALC_GENERATEPOINTCODEBODY
#include "calc_generatepointcodebody.h"
#define INCLUDED_CALC_GENERATEPOINTCODEBODY
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

// Module headers.
#ifndef INCLUDED_CALC_CR
#include "calc_cr.h"
#define INCLUDED_CALC_CR
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif
#ifndef INCLUDED_CALC_CFGCREATOR
#include "calc_cfgcreator.h"
#define INCLUDED_CALC_CFGCREATOR
#endif
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif

/*!
  \file
  This file contains the implementation of the PointCodeBodyGenerator class.
*/



//------------------------------------------------------------------------------

namespace calc {

static char cellUnionField(VS s)
{
  DEVELOP_PRECOND(CRI_1==0);   /* UINT1 */
  DEVELOP_PRECOND(CRI_4==1);   /* INT4 */
  DEVELOP_PRECOND(CRI_f==2);   /* REAL4 */
  const char *unionMembers="bif";
  return unionMembers[allFitCRIndex(s)];
}

static const char* crType(const ASTNode* n)
{
  VS s=n->returnDataType().vs();

  DEVELOP_PRECOND(CRI_1==0);   /* UINT1 */
  DEVELOP_PRECOND(CRI_4==1);   /* INT4 */
  DEVELOP_PRECOND(CRI_f==2);   /* REAL4 */
  const char *types[]= { "UINT1 ","INT4 ","REAL4 " };
  return types[allFitCRIndex(s)];
}

static std::string mvTest(const std::set<std::string>& names)
{
  std::vector<std::string> s;
  for(std::set<std::string>::const_iterator i=names.begin();
      i != names.end(); ++i) {
    std::string a(*i);
    if (com::isDouble(a))
      continue; // skip MV check on numbers
    if (a.find("].f[0]") != std::string::npos)
      continue; // skip MV check on non-spatials
    if (a.find("_f<point::") == 0)
      continue; // skip MV check on inliners
    std::ostringstream str;
    str << "pcr::isMV(" << a << ")";
    s.push_back(str.str());
  }
  if (s.empty())
    return "0";
  return com::join(s,"|");
}

} // namespace calc



//------------------------------------------------------------------------------
// DEFINITION OF STATIC POINTCODEBODYGENERATOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF POINTCODEBODYGENERATOR MEMBERS
//------------------------------------------------------------------------------

calc::PointCodeBodyGenerator::PointCodeBodyGenerator(
    CFGNode*      cfg,
    const ParSet& vContents):
     CFGVisitor(cfg),
     d_curr(0)
{
  size_t n=0;
  for(ParSet::const_iterator i=vContents.begin(); i!=vContents.end(); ++i) {
    std::ostringstream s;
    ASTPar *p=*i;
    PRECOND(!p->returnDataType().stEither());
    s << "v[" << n << "]." << cellUnionField(p->returnDataType().vs());
    if (p->returnDataType().stSpatial())
       s << "[i]";
    else
       s << "[0]";
    d_parNames.insert(std::make_pair(p->name(),s.str()));
    n++;
  }

  d_loop << "for(size_t i=0; i<n; ++i ) { " << std::endl;
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::PointCodeBodyGenerator::PointCodeBodyGenerator(PointCodeBodyGenerator const& rhs)
  : Base(rhs)
{
}
*/

calc::PointCodeBodyGenerator::~PointCodeBodyGenerator()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::PointCodeBodyGenerator& calc::PointCodeBodyGenerator::operator=(PointCodeBodyGenerator const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void calc::PointCodeBodyGenerator::visitStat(ASTStat* )
{
}

//! define result temporary in stream and return name of temporary
std::string calc::PointCodeBodyGenerator::tmpDef(const BaseExpr* e) const
{
  std::ostringstream result;
  result << "tmp" << e;
  (*d_curr) << crType(e) << result.str() << ";" << std::endl;
  return result.str();
}

void calc::PointCodeBodyGenerator::visitExpr(BaseExpr* e)
{
  selectPart(e->returnDataType(0));

  reverseTop(e->nrArgs());

  if(e->op().opCode()== OP_IFTHENELSE)
    doIfThenElse(e);
  else
    doExpr(e);

}
/*
     arg1 = top(0).value;
     arg2 = top(1).value;
   if (expr == ifthenelse or has.domainCheck (OF ass)) {
     mvtest is on union of arg1.names,arg2.names;
     generate tmp-ass = f(arg1,arg2);
   } else
     new stack-item
      value= f(arg1,arg2)
      names= union;
   }
   ONDUIDELIJK IN CODE WAT WELLE STACK SITUATIE NODIG HEEFT
*/

void calc::PointCodeBodyGenerator::visitAss(ASTAss*  a)
{
  PRECOND(a->nrPars()==1);
  ASTPar *lhs= a->par(0);

  selectPart(lhs->returnDataType());

  if (d_parNames.count(lhs->name())==0) {
    // first assignment to local
    if (d_args.back().expr()) {
      // must be assigned an inliner
      (*d_curr) << "// forced inline assignment "
                << a->shortPosText() << std::endl;
      std::string name=tmpDef(d_args.back().expr());
      assignment(name,"0");
      d_parNames.insert(std::make_pair(lhs->name(),name));
      pop();
    } else {
      // do not assign but remember that
      // this local is shadowed by pop() value
      d_parNames.insert(std::make_pair(lhs->name(),pop()));
    }
  } else {
    // p = tmp0x74834738
    (*d_curr) << "// " << a->shortPosText() << std::endl;
    assignment(par(lhs),"0");
    pop();
  }
}

void calc::PointCodeBodyGenerator::assignment(
    const std::string& result,
    const std::string& domainCheck) const
{
    (*d_curr) << "if( (" << mvTest(d_args.back().names()) << ")||"
              << domainCheck << ")" << std::endl
              << " pcr::setMV(" << result << ");" << std::endl
              << "else" << std::endl
              << " " << result << "= " << d_args.back().value() << ";" << std::endl;
}

void calc::PointCodeBodyGenerator::doExpr(
    BaseExpr* e)
{
  std::string dc(domainCheck(e->op().domainIll(),templateArg(e)));

  // replace args by inliner f
  popArgsPushResult(f(e),e->nrArgs());
  d_args.back().setExpr(e);

  if (dc != "0" || e->returnDataType().stNonSpatial()) {
    // can not inline, due to domain print
    // do not want to inline due to nonspatial, print in d_ns
    (*d_curr) << "// " << e->shortPosText() << std::endl;
    std::string r(tmpDef(e));
    assignment(r,dc);

    // replace the inliner by the result
    pop();
    push(r);
  }
}

void calc::PointCodeBodyGenerator::doIfThenElse(
    BaseExpr* e)
{
   // ifthenelse in calc_pointcodedllheader.h
   (*d_curr) << "// " << e->shortPosText() << std::endl;
   std::string r(tmpDef(e));
   (*d_curr) << "_ifthenelse<"<< crType(e) << ">("
       << r << ","
       << arg(0) << "," << arg(1) << "," << arg(2) << ");" << std::endl;
   pop();
   pop();
   pop();
   push(r);
}

//! rhs side visit
void calc::PointCodeBodyGenerator::visitPar(ASTPar*  p)
{
  push(par(p));
}
void calc::PointCodeBodyGenerator::visitNumber(ASTNumber*  n)
{
  std::ostringstream s;
  s<< n->value();
  push(s.str());
}

void calc::PointCodeBodyGenerator::print(std::ostream& s) const
{
  s << d_ns.str()      << std::endl;
  s << d_loop.str()    << std::endl;
  s << "} // end loop" << std::endl;
}

//----------------------------------------------------------------------------
// TOOLS
//----------------------------------------------------------------------------


std::string calc::PointCodeBodyGenerator::par(ASTPar *p)
{
  PRECOND(d_parNames.count(p->name()));
  return d_parNames[p->name()];
}


std::string calc::PointCodeBodyGenerator::domainCheck(
    DomainIll d,
    const std::string& templateArg) const
{
  std::ostringstream s;
  switch(d) {
    case onlyDomainIll:  /*!< unary argument has domain check */
      s << "_odi< " << templateArg << " >(" << arg(0) << ")";
      return s.str();
   case rightDomainIll: /*!< right (2nd) argument has domain check */
      s << "_rdi<  " << templateArg << " >(" << arg(1)<< ")";
      return s.str();
   case combDomainIll:  /*!< combination of 1st and 2nd arg may be illegal, pow only */
      s << "_cdi<  " << templateArg << " >(" << arg(0) << "," << arg(1) << ")";
      return s.str();
   case noDomainIll:
      return "0";
  };
  PRECOND(false);
  return s.str();
}

std::string calc::PointCodeBodyGenerator::f(
    const BaseExpr* e) const
{
  std::ostringstream f;
  // _f< start at pos 0 is significat to filter in mvTest
  f << "_f<" << templateArg(e) << ">(";
  switch(e->nrArgs()) {
    case 1: f << arg(0); break;
    case 2: f << arg(0) << "," << arg(1); break;
    default: PRECOND(FALSE);
  }
  f << ")";
  return f.str();
}

std::string calc::PointCodeBodyGenerator::templateArg(
    const BaseExpr* e) const
{
  std::string templateType;
  switch(e->op().execType()) {
    case EXEC_TYPE_POLY:
    case EXEC_TYPE_DIFF_BIN: templateType=crType(e->arg(0)); break;
    case EXEC_TYPE_DIFF_UN : /* Result,Input  */
                        templateType=crType(e)+std::string(",")
                                    +crType(e->arg(0));
                        break;
    case EXEC_TYPE_SAME_UN :
    case EXEC_TYPE_SAME_BIN: templateType=crType(e); break;
      default    : POSTCOND(FALSE);
  }

  std::ostringstream t;
  t << "point::"<< e->op().implName() << "<" << templateType << "> ";
  return t.str();
}

std::string calc::PointCodeBodyGenerator::pop()
{
  std::string s(d_args.back().value());
  d_args.pop_back();
  return s;
}

//! reverse the top so arg() returns the correct one
void calc::PointCodeBodyGenerator::reverseTop(size_t nrArgs)
{
  std::reverse(d_args.end()-nrArgs,d_args.end());
}

const std::string& calc::PointCodeBodyGenerator::arg(size_t argNr) const
{
  PRECOND(d_args.size() > argNr);
  // pushed backwards
  return (d_args.end()-argNr-1)->value();
}

void calc::PointCodeBodyGenerator::push(const std::string& argName)
{
  d_args.push_back(argName);
}

void calc::PointCodeBodyGenerator::popArgsPushResult(
    const std::string& function,
    size_t             nrArgs)
{

    PointCodeSI s(function);
    std::set<std::string> names;
    for(size_t i=0; i < nrArgs; ++i) {
      names.insert(
          d_args.back().names().begin(),
          d_args.back().names().end());
      pop();
    }
    // if args then names not empty
    PRECOND((nrArgs==0) == names.empty());
    s.setNames(names);
    d_args.push_back(s);
}

void calc::PointCodeBodyGenerator::selectPart(const DataType& dt)
{
  PRECOND(!dt.stEither());
  std::ostringstream*  newCurr;
  if (dt.stSpatial())
    newCurr= &d_loop;
  else
    newCurr= &d_ns;

  if (newCurr!=d_curr && !d_args.size()) {
    /*! generate code for current stack (sub-expr)
     * always a transfer from d_ns -> d_loop
     */
   //  PRECOND(newCurr == &d_loop);
   //  PRECOND(d_args.back().expr());
  }

  d_curr=newCurr;
  // (*d_curr) << "// " << dt << std::endl;
}

//! set value of d_expr
void calc::PointCodeSI::setExpr(BaseExpr* expr)
{
  d_expr=expr;
}

//! get value of d_expr
calc::BaseExpr* calc::PointCodeSI::expr() const
{
  return d_expr;
}

calc::PointCodeSI::PointCodeSI(
    const std::string& name):
  d_expr(0),
  d_value(name)
{
  d_names.insert(name);
}

//! set value of d_names
void calc::PointCodeSI::setNames(const std::set<std::string>& names)
{
  d_names=names;
}

//! get value of d_names
const std::set<std::string>& calc::PointCodeSI::names() const
{
  return d_names;
}

//! set value of d_value
void calc::PointCodeSI::setValue(const std::string& value)
{
  d_value=value;
}

//! get value of d_value
const std::string& calc::PointCodeSI::value() const
{
  return d_value;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


void calc::generatePointCodeBody(
      std::ostream& s,
      ASTNode*      code,
      const ParSet& vContents)
{
  ScopedCFG n(code);
  PointCodeBodyGenerator pcbg(n.cfg,vContents);
  pcbg.visit();
  pcbg.print(s);
}
