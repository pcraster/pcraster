#header<<
#include "stddefx.h"
#ifdef BORLANDC
 #pragma warn -8004
#endif
#include <math.h>
#include <string>
#include <vector>

/* TODO
 * vragen/opmerkingen na bestudering ANTLR 2.7.x docs:
 *
 * gebruik huidige flex  code
 *  -> /tmp/antlr-2.7.2/examples/cpp/flexLexer
 *  - schrijf parser om:
 * laat AST dumpen, kijk naar AST viewer.
 *  aandachts punten:
 *  gebruik bestande Nodes?
 *  /tmp/antlr-2.7.2/examples/cpp/heteroAST
 * - kan alles met AST transformaties
 * - hoe werken excepties?
 * heeft aangemaakte C++ code de throws clause?
 *
 */

// see known problems M19:
#define PURIFY(r,s)

// all that is in return stat's
// to Parser.h compiling
namespace calc {
  class ASTNumber;
  class ASTId;
  class ASTNodeVector;
  class ASTNodeList;
  class  ASTNode;
  class  ASTPar;
  class  ASTExpr;
  class  ASTStat;
  class  ASTAss;

  class  ASTScript;

  struct ParsReportMoment;
  class  Position;
  class  Operator;
  class  RunSettings;
}

#ifndef INCLUDED_CALC_IDLIST
#include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif
#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif
#ifndef INCLUDED_CALC_POSITION
#include "calc_position.h" // setPriority
#define INCLUDED_CALC_POSITION
#endif

#include "ATokPtr.h"
>>

<<

#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif
#ifndef INCLUDED_CALC_RUNSETTINGS
#include "calc_runsettings.h"
#define INCLUDED_CALC_RUNSETTINGS
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_CALC_ASTDEFINITION
#include "calc_astdefinition.h"
#define INCLUDED_CALC_ASTDEFINITION
#endif
#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
#ifndef INCLUDED_CALC_NONASSEXPR
#include "calc_nonassexpr.h"
#define INCLUDED_CALC_NONASSEXPR
#endif
#ifndef INCLUDED_CALC_ASTEXPR
#include "calc_astexpr.h"
#define INCLUDED_CALC_ASTEXPR
#endif
#ifndef INCLUDED_CALC_LINKINEXPR
#include "calc_linkinexpr.h"
#define INCLUDED_CALC_LINKINEXPR
#endif
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_DYNAMICSECTION
#include "calc_dynamicsection.h"
#define INCLUDED_CALC_DYNAMICSECTION
#endif
#ifndef INCLUDED_CALC_REPEATUNTIL
#include "calc_repeatuntil.h"
#define INCLUDED_CALC_REPEATUNTIL
#endif
#ifndef INCLUDED_CALC_IDLIST
#include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
/*
#ifndef  INCLUDED_CALC_INDEXTABLE
#include "calc_indextable.h"
#define  INCLUDED_CALC_INDEXTABLE
#endif
#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif
#ifndef INCLUDED_CALC_ARRAYDEFVECTOR
#include "calc_arraydefvector.h"
#define INCLUDED_CALC_ARRAYDEFVECTOR
#endif
#ifndef INCLUDED_CALC_FOREACH
#include "calc_foreach.h"
#define INCLUDED_CALC_FOREACH
#endif
#ifndef INCLUDED_CALC_STDOUTSTAT
#include "calc_stdoutstat.h"
#define INCLUDED_CALC_STDOUTSTAT
#endif
#ifndef INCLUDED_CALC_WRITEINFO
#include "calc_writeinfo.h"
#define INCLUDED_CALC_WRITEINFO
#endif
*/

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif
#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif

#ifndef INCLUDED_LEXGRAMMAR
#include "lexgrammar.h"
#define INCLUDED_LEXGRAMMAR
#endif

using namespace calc;

typedef calc::LexToken ANTLRToken;

void Parser::notImplemented(const ANTLRTokenPtr at) const
{
  genId(at).posError("Sorry, this language construct is not implemented");
}

calc::Id Parser::genId(const ANTLRTokenPtr at) const
{
    return calc::Id(mytoken(at)->stringVal(),mytoken(at)->position());
};

const calc::Position* Parser::position(const ANTLRTokenPtr at) const
{
    return mytoken(at)->position();
}

calc::ASTExpr* Parser::createExpr(const Position       *pos,
                                   const calc::Operator *op) const
{
  PRECOND(op);
  return new calc::ASTExpr(pos,*op);
}

calc::ASTExpr* Parser::createExpr(const ANTLRTokenPtr   at,
                                  const calc::Operator *op) const
{
  PRECOND(op);
  return new calc::ASTExpr(mytoken(at)->position(),*op);
}

const calc::Operator* Parser::tokenOp(const ANTLRTokenPtr at) const
{
    PRECOND(calc::major2op(mytoken(at)->opCode()));
    return calc::major2op(mytoken(at)->opCode());
}

//! current token saus this is an exp where we expect a file
void Parser::expectFile(const std::string& fileDescr) {
    calc::Id id(genId(LT(1)));
    id.posError("Expression illegal here, expecting name of "+fileDescr);
}

void Parser::checkParseError() {
   const ANTLRTokenPtr at(LT(1));
   calc::Id pe(genId(at));
   std::ostringstream msg;
   if (mytoken(at)->stringVal().empty()) // no keyword or symbol end-of-input
     msg << "Syntax error: more characters expected at this point";
   else {
     bool kw=mytoken(at)->isKeyword();
     // pcrcalc/test34
     msg  << "Syntax error at " << (kw ? "keyword ":"symbol ") << pe.qName();
     if (kw)
      msg  << "\n Keywords can not be used as names for files or variables"
           << "\n Keywords must be placed in a specific order";
   }
   pe.posError(msg);
}


void Parser::illegalFunc(const calc::ASTPar& s) const {
  // pcrcalc/test41
  s.posError("function '"+s.name()+"' illegal here");
}

const calc::Operator* Parser::expectFunction(const calc::Id& id) const {
  const calc::Operator* o = calc::opName2op(id.name());
  if (!o)
    id.posError("'"+id.name()+"' is not a function");
  return o;
}

const calc::ASTPar& Parser::expectId(const calc::ASTPar& par) const {
  if (0 /*par.isArray() */)
      par.posError("Illegal construct");
  // return Id(par.name(),par.position());
  return par;
}

calc::ASTNumber* Parser::createCastedASTNumber(
    const ANTLRTokenPtr   convF,
    const calc::Id& nr) const
{
  calc::Id s(genId(convF));
  std::ostringstream name;
  name << s.name() << "(" << nr.name() << ")";
  s.setName(name.str());
  return new calc::ASTNumber(s, tokenOp(convF)->vs(),nr);
}
>>

// TOK_DOLLAR only used in lexer when sealing
#token TOK_DOLLAR

#token TOK_EOF

#token TOK_INTERFACE
#token TOK_BINDING
#token TOK_AREAMAP
#token TOK_TIMER
#token TOK_MODEL
#token TOK_REPORT
#token TOK_DYNAMIC
#token TOK_INITIAL
#token TOK_FILEOUTPUT
#token TOK_OBJECT
#token TOK_FOREACH
#token TOK_REPEAT
#token TOK_UNTIL
#token TOK_IF
#token TOK_IN
#token TOK_EXCEPT
#token TOK_AND
#token TOK_OR

#token TOK_ELSE
#token TOK_THEN
#token TOK_COMMA
#tokclass THEN_GROUP { TOK_THEN TOK_COMMA }
#tokclass ELSE_GROUP { TOK_ELSE TOK_COMMA }
// #token TOK_LDD
// #token TOK_DIRECTIONAL
// #token TOK_SCALAR
// #token TOK_BOOLEAN
// #token TOK_NOMINAL
// #token TOK_ORDINAL

#token TOK_COMP
#token TOK_ASSOP
#token TOK_MOMENT_RANGE
#token TOK_ENDTIME
#token TOK_INT
#token TOK_FLOAT

#token TOK_FDIV
#token TOK_IDIV
#token TOK_MOD
#token TOK_EQ
#token TOK_GE
#token TOK_GT
#token TOK_ID
#token TOK_LE
#token TOK_LT
#token TOK_NE
#token TOK_NOT
// note XOR is somewhere in pccts defined
#token TOK_XOR
#token TOK_LP
#token TOK_RP
#token TOK_LB
#token TOK_RB
#token TOK_LC
#token TOK_RC
#token TOK_STAR
#token TOK_PLUS
#token TOK_MINUS
#token TOK_IS
#token TOK_SC
#token TOK_COL
#token TOK_COLON
#token TOK_ILLEGAL
#token TOK_ILLEGALID
// #token TOK_TIMEOUTPUT
#token TOK_CONV_F
// #token TOK_INDEX_F
#token TOK_POW
#tokclass  ADD_GROUP    { TOK_PLUS TOK_MINUS }
#tokclass  MULT_GROUP    { TOK_STAR TOK_MOD TOK_IDIV TOK_FDIV }
#tokclass  ID_GROUP    { TOK_ID TOK_REFERENCE }

/*!
    \brief
      Transform ParserInput to an tree of ASTNode objects.

    All objects are returned as std::auto_ptr of an ASTNode or its subclasses
 */
class Parser {
<<
public:

    typedef std::auto_ptr<calc::ASTNumber   > AP_ASTNumber;
    typedef std::auto_ptr<calc::ASTId       > AP_ASTId;
    typedef std::auto_ptr<calc::ASTNodeVector > AP_ASTNodeVector;
    typedef std::auto_ptr<calc::ASTNodeList > AP_ASTNodeList;
    typedef std::auto_ptr<calc::ASTNode     > AP_ASTNode;
    typedef std::auto_ptr<calc::ASTPar      > AP_ASTPar;
    typedef std::auto_ptr<calc::ASTExpr     > AP_ASTExpr;
    typedef std::auto_ptr<calc::ASTStat     > AP_ASTStat;
    typedef std::auto_ptr<calc::ASTAss      > AP_ASTAss;

private:
    // not const due to LT(1) use
    void                     expectFile(const std::string& fileDescr);
    void                     checkParseError();

    void                     illegalFunc(const calc::ASTPar& s)   const;
    const calc::Operator*    expectFunction(const calc::Id& id)   const;
    const calc::ASTPar&      expectId   (const calc::ASTPar& par) const;

    calc::Id                 genId(const ANTLRTokenPtr at) const;
    void                     notImplemented(const ANTLRTokenPtr at) const;
    const calc::Position* position(const ANTLRTokenPtr at) const;
    calc::ASTExpr*            createExpr(const calc::Position  *pos,
                                   const calc::Operator *op) const;
    calc::ASTExpr*            createExpr(const ANTLRTokenPtr   at,
                                   const calc::Operator *op) const;

    const calc::Operator*    tokenOp(const ANTLRTokenPtr at) const;
    calc::ASTNumber*         createCastedASTNumber(
                                     const ANTLRTokenPtr   convF,
                                     const calc::Id& nr) const;
public:
    //! ALWAYS AS FIRST STATEMENT! should go in ctor, if we could set the Ctor
    void initialize() {
          ANTLRParser::init();
    };
>>

/*********/
/* Model */
/*********/

model>[std::auto_ptr<calc::ASTScript> ret]:
      << $ret.reset(new ASTScript());
         AP_ASTNode b;
      >>
       { TOK_INTERFACE interfaceSection[$ret.get()] }
       { TOK_BINDING   bindingSection[$ret.get()]   }
        { areaMapSection[$ret.get()] } { timer[$ret.get()] }
       body>[b] TOK_EOF
      <<
          if (!b.get())
            throw com::Exception("script contains no code");
          $ret->transferCode(b.release());
       >>
      ;
      exception
       default:
         << checkParseError(); >>

/*********************/
/* external bindings */
/*********************/

// external binding, ;-terminator is optional
externalBindings[calc::ASTNodeVector& l]:
        << AP_ASTNode e; >>
        ( bindingDefinition>[e]
          << l.transferPushBack(e.release()); >>
          { TOK_SC }
        )* TOK_EOF
        ;
        exception
         default:
         << checkParseError(); >>

// in script binding, ;-terminator is required
bindingSection[calc::ASTScript *script]:
         << AP_ASTNode e; >>
         ( bindingDefinition>[e]
            <<
               if (e.get())
                 script->transferOneBinding(e.release());
               // else index/array stuf
            >>
          TOK_SC )*
         ;

bindingDefinition>[Parser::AP_ASTNode ret]:
         << AP_ASTNode e; >>
         parWithIndeces>[calc::ASTPar p]
          TOK_IS bindingRHS[p]>[$ret]
         ;

// index/array stuff still in modelastscript.g
// LATER bindingRHS mag ook een expr zijn
//       dat evalueert tot een constante b.v. sqrt(2)
bindingRHS[const calc::ASTPar& par]>[Parser::AP_ASTNode ret]:
         << AP_ASTNode n; calc::Id right; >>
        (
           qid>[right] << n.reset(new calc::ASTPar(right)); >>
           { TOK_LP << illegalFunc(right); >> }
        | number>[right]
          << n.reset(new calc::ASTNumber(right)); >>
        | convF:TOK_CONV_F TOK_LP number>[right]
         TOK_RP
           // add numeric binding with a typecast
           // pcrcalc/test42
          << n.reset(createCastedASTNumber(convF,right)); >>
        )
        <<
          if (n.get())
           $ret.reset(new ASTAss(expectId($par),n.get()));
          // else index/array stuff
        >>
        ;

interfaceSection[calc::ASTScript *script]:
         (  id:TOK_ID TOK_LC
            << // define here, to create new one each repition:
               ASTDefinition ib;
               ib.setName(genId(id));
            >>
            ( << calc::Id value; >>
              key:TOK_ID TOK_IS { nrOrId>[value] } TOK_SC
              << ib.add(genId(key),value); >>
            )*
           TOK_RC
           { TOK_SC }
           << script->addInterfaceDefinition(ib); >>
         )*
         ;

areaMapSection[calc::ASTScript *script]: << AP_ASTId s; >>
         TOK_AREAMAP nrOrPar>[s] TOK_SC
         << script->transferAreaMap(s.release()); >>
         ;

timer[calc::ASTScript *script]:
       << AP_ASTId eTimerStartOrTss,eTimerEnd,eTimerStep; >>
          TOK_TIMER
            nrOrPar>[eTimerStartOrTss]
           { nrOrPar>[eTimerEnd]
             nrOrPar>[eTimerStep]
            <<
              script->transferTimerSection(
               eTimerStartOrTss.release(),
               eTimerEnd.release(),
               eTimerStep.release());
            >>
           } TOK_SC
        (reportMomentDef[script])*
        ;


body>[Parser::AP_ASTNode ret]:
          << AP_ASTNodeList initial;
             std::auto_ptr<DynamicSection> dynamic;
          >>
        { // TOK_(INITIAL/MODEL) is optional, simple list of stats
          // only is also processed here
          { TOK_INITIAL | TOK_MODEL } statementList>[initial]
        }
        {  << AP_ASTNodeList dynamicStatements; >>
           td:TOK_DYNAMIC statementList>[dynamicStatements]
            <<   dynamic.reset(new calc::DynamicSection(
                   position(td), dynamicStatements.release()));
            >>
        }
          << if (!dynamic.get()) {
               // no dynamic body is initial
               $ret=initial;
            } else {
              if (!initial.get()) {
                // no initial body is dynamic
               $ret=dynamic;
              } else { // both
               AP_ASTNodeList body(new calc::ASTNodeList());
               body->transferPushBack(initial.release());
               body->transferPushBack(dynamic.release());
               $ret=body;
              }
             }
          >>
        ;

/*****************/
/* REPORT MOMENT */
/*****************/
// the defs below the timer
reportMomentDef[calc::ASTScript *script]:
          << std::vector<calc::ParsReportMoment> m; >>
           reportId:TOK_ID
             TOK_IS reportMoments[m] TOK_SC
           << script->addReportDefinition(Report(genId(reportId),m));
           >>
        ;

reportMoments[std::vector<calc::ParsReportMoment> &m]
        :    << calc::ParsReportMoment p; >>
              reportMoment[p]
              << $m.push_back(p); >>
            ( TOK_COMMA reportMoment[p]
              << $m.push_back(p); >>
            )*
        ;

reportMoment[calc::ParsReportMoment& p]
             : << $p.start=0; $p.step=0;$p.end=0; >>
          (
           ( start:TOK_INT << $p.start = mytoken(start)->integerVal(); >>
             (( reportMomentStep>[$p.step] range:TOK_MOMENT_RANGE
                  << $p.end = mytoken(range)->integerVal(); >>
                 ) |
             { range2:TOK_MOMENT_RANGE
                  << $p.end = mytoken(range2)->integerVal(); >>
             }
             )
             << try {
                  $p.check();
                } catch (com::Exception& msg) {
                     genId(start).posError(msg.messages());
                }
             >>
           )
           | TOK_ENDTIME << $p.start= -1;  >>
          )
          ;

reportMomentStep>[int step]
         : TOK_PLUS v:TOK_INT
           << $step = mytoken(v)->integerVal(); >>
         ;


/*****************/
/* FOREACH STUFF */
/*****************/

idList > [ calc::IdList i ]
        :
           TOK_LP
            firstId:TOK_ID            << $i.push_back(genId(firstId)); >>
            ( TOK_COMMA nextId:TOK_ID << $i.push_back(genId(nextId));  >>
            )*
           TOK_RP
        ;

statementList>[Parser::AP_ASTNodeList ret]
        : << AP_ASTNode s; >>
          ( (foreach>[s]|repeat>[s]|statement>[s])
            <<
               if (!$ret.get())
                 $ret.reset(new calc::ASTNodeList());
               $ret->transferPushBack(s.release());
            >>
          )*
        ;

statement>[Parser::AP_ASTStat ret]:
         << $ret.reset(new calc::ASTStat()); >>
         (
         ( ni:TOK_FILEOUTPUT expr>[AP_ASTNode r_expr]
          << notImplemented(ni);
            $ret->transferStat(r_expr.release()); 
          >>
         | { TOK_REPORT
            << $ret->setReportParsed(true); >>
            {
              TOK_LP
              (
               rv:TOK_ID << $ret->setReportById(genId(rv)); >>
               | << std::vector<calc::ParsReportMoment> inSitu;
                    calc::Id pos(genId(LT(1)));
                  >>
                 reportMoments[inSitu]
               <<
                  char buf[8];
                  static int  inSituNr=0; // harmless static
                  sprintf(buf,"%d",inSituNr++);
                  pos.setName(buf);
                  $ret->transferReportInSitu(
                     new calc::Report(pos, inSitu));
                >>
              )
              TOK_RP
            }
          } assignment>[AP_ASTAss a]
          <<
             $ret->transferStat(a.release());
          >>
          ) | ( TOK_OBJECT objectName:TOK_ID TOK_IS expr>[AP_ASTNode r_expr]
                 <<
                     // rewrite to single expr, no assignment
                     LinkInExpr *lie=dynamic_cast<LinkInExpr *>(r_expr.get());
                     if (!lie)
                       r_expr->posError("libraryName::ClassName construct expected");
                     lie->setAsConstructor(ASTPar(genId(objectName)));
                     $ret->transferStat(r_expr.release());
                 >>
              )
          )
          statementSep
        ;

foreach>[Parser::AP_ASTStat ret]:
         << // calc::ForEach *f=0;
            calc::Id iter;
             AP_ASTNodeList forEachBody;
             calc::IdList in,except,s;
             bool ascending=true; 
         >>
          posF:TOK_FOREACH
          iterId:TOK_ID
          TOK_IN ( inId:TOK_ID << in.push_back(genId(inId)); >>
              | idList>[in]
             )
          { TOK_EXCEPT (
                  exceptId:TOK_ID << except.push_back(genId(exceptId)); >>
                 | idList>[except]
                )
          }
          TOK_LC
              << // f = 0;
                 notImplemented(posF);
                 //new calc::ForEach(posF.get(),inclIn,
                 //genId(iterId),in,except,s,ascending);
                 //inclIn->addStatement(f);
              >>
              statementList>[forEachBody]
           TOK_RC
        ;

repeat>[Parser::AP_ASTNode ret]:
         << RepeatUntil *r(0);
            AP_ASTNode condition;
            AP_ASTNodeList repeatBody;
          >>
          posR:TOK_REPEAT
          TOK_LC
              statementList>[repeatBody]
          TOK_RC TOK_UNTIL expr>[condition] TOK_SC
              <<
                 r=new RepeatUntil(position(posR),repeatBody.release());
                 $ret.reset(r);
                 r->transferCondition(new NonAssExpr(condition.release()));
              >>
        ;

/**************************/
/* STATEMENTS/ASSIGNMENTS */
/**************************/

assignment>[Parser::AP_ASTAss ret]:
       << calc::ASTPar par;
          bool         swap=false;
          AP_ASTNode   right;
          $ret.reset(new calc::ASTAss());
       >>
       parWithIndeces>[par]
       << $ret->addPar(par); >>
      (
        TOK_COMMA parWithIndeces>[par]
        << $ret->addPar(par); >>
      )*
      assignmentTail[par,swap]>[right]
      << $ret->transferRhs(right.release());
         if (swap)
           $ret->swap01();
      >>
     ;
     exception
         default:
         << /* see pcrcalc/test34 without this no cleaning? */;
            checkParseError();
         >>

assignmentTail[const calc::ASTPar& lhs, bool& swap]>[Parser::AP_ASTNode ret]
          :(ass:TOK_ASSOP expr>[AP_ASTNode right]
             <<
                // e.g. lhs += expr
              const calc::Operator* op = tokenOp(ass);
              AP_ASTExpr e(createExpr(ass,op));
              e->transferArg(new calc::ASTPar(lhs));
              e->transferArg(right.release());
              $ret=e;
             >>
           )|(TOK_IS (
             (fid1:TOK_ID TOK_COMMA fid2:TOK_ID
                //  the awfull relic  m1,m2=fid1,fid2(.....)
                //  for lddcreate, lddcreatedem etc. (old style MRF)
                //   here we check for a needed swap to standardize
                //    the order, as the new style MRF has a fixed order.
                  <<
                     calc::Id sym1(genId(fid1));
                     const calc::Operator* op1 = expectFunction(sym1);
                     calc::Id sym2(genId(fid2));
                     const calc::Operator* op2 = expectFunction(sym2);
                  >>
                  TOK_LP exprList>[AP_ASTNodeVector args] TOK_RP
                  <<
                    if (op1->opCode() != otherOneOfMRF(op2->opCode()) ) {
                      /* pcrcalc/test11[bc] */
                      sym1.position()->throwError("Functions "
                       +quote(op1->name())+" and "+quote(op2->name())
                       +" can not be combined");
                    }
                    swap = oneOfMrfIsStackTop(op1->opCode());
                    if (swap) {
                     std::swap(op1,op2);
                     std::swap(sym1,sym2);
                    }
                    std::auto_ptr<ASTExpr>
                      e(new calc::ASTExpr(
                                     sym1.position(),
                                     calc::oneOf2Mrf(op1->opCode())));
                    e->transferFunctionArgs(args.release());
                    $ret=e;
                  >>
              ) | expr>[AP_ASTNode right]
              << $ret=right; >>
             )) // end TOK_IS ( expr | .. )
           ;

statementSep: TOK_SC
            | TOK_EOF
            ;

endOfInput  : (TOK_SC)* TOK_EOF
            ;
     exception
         default:
         << checkParseError(); >>

/******************************/
/* expr: creating a ASTExpr   */
/******************************/

expr>[Parser::AP_ASTNode ret]
    : << AP_ASTNode right; >>
      xor_expr>[$ret]
      (
        opS:TOK_OR xor_expr>[right]
      <<
         AP_ASTExpr e(createExpr(opS,calc::major2op(OP_OR_)));
         e->transferArg($ret.release());
         e->transferArg(right.release());
         $ret=e;
      >>
      )*
    ;
     exception
         default:
         << /* see pcrcalc/test34 without this no cleaning? */;
            checkParseError();
         >>

xor_expr>[Parser::AP_ASTNode ret]
    : << AP_ASTNode right; >>
      and_expr>[$ret]
      (
        opS:TOK_XOR and_expr>[right]
      <<
         AP_ASTExpr e(createExpr(opS,calc::major2op(OP_XOR_)));
         e->transferArg($ret.release());
         e->transferArg(right.release());
         $ret=e;
      >>
      )*
    ;
and_expr>[Parser::AP_ASTNode ret]
    : << AP_ASTNode right; >>
      eq_expr>[$ret]
      (
        opS:TOK_AND eq_expr>[right]
      <<
         AP_ASTExpr e(createExpr(opS,calc::major2op(OP_AND_)));
         e->transferArg($ret.release());
         e->transferArg(right.release());
         $ret=e;
      >>
      )*
    ;

eq_expr>[Parser::AP_ASTNode ret]
    : << AP_ASTNode right; >>
      comp_expr>[$ret]
      (
        opS:TOK_EQ comp_expr>[right]
      <<
         AP_ASTExpr e(createExpr(opS,tokenOp(opS)));
         e->transferArg($ret.release());
         e->transferArg(right.release());
         $ret=e;
      >>
      )*
    ;
comp_expr>[Parser::AP_ASTNode ret]
    : << AP_ASTNode right; >>
      add_expr>[$ret]
      (
        opS:TOK_COMP add_expr>[right]
      <<
         AP_ASTExpr e(createExpr(opS,tokenOp(opS)));
         e->transferArg($ret.release());
         e->transferArg(right.release());
         $ret=e;
      >>
      )*
    ;
add_expr>[Parser::AP_ASTNode ret]
    : << AP_ASTNode right;
      >>
      mult_expr>[$ret]
      (
        opS:ADD_GROUP mult_expr>[right]
      <<
         const calc::Operator* op = calc::major2op(
             opS->getType() == TOK_PLUS ? OP_BADD:OP_BMIN);
         AP_ASTExpr e(createExpr(opS,op));
         e->transferArg($ret.release());
         e->transferArg(right.release());
         $ret=e;
      >>
      )*
    ;
mult_expr>[Parser::AP_ASTNode ret]
    : << AP_ASTNode right;
      >>
      pow_expr>[$ret]
      (
        opS:MULT_GROUP pow_expr>[right]
      <<
         AP_ASTExpr e(createExpr(opS,tokenOp(opS)));
         e->transferArg($ret.release());
         e->transferArg(right.release());
         $ret=e;
      >>
      )*
    ;
    exception
      default:
      <<
         checkParseError();
      >>

pow_expr>[Parser::AP_ASTNode ret]
    : << AP_ASTNode right; >>
      sign_expr>[$ret]
      (
        opS:TOK_POW sign_expr>[right]
      <<
         AP_ASTExpr e(createExpr(opS,calc::major2op(OP_POW)));
         e->transferArg($ret.release());
         e->transferArg(right.release());
         $ret=e;
      >>
      )*
    ;
sign_expr>[Parser::AP_ASTNode ret]
    : << std::vector<calc::Id> signs;
      >>
     (
      s:ADD_GROUP << signs.push_back(genId(s)); >>
     )* not_expr>[$ret]
     <<
       for(int i = (int)(signs.size()-1);i>=0; i--) {
        const calc::Operator* op = calc::major2op(
            signs[i].name() == "+" ? OP_UADD:OP_UMIN);
        AP_ASTExpr e(createExpr(signs[i].position(),op));
        e->transferArg($ret.release());
        $ret=e;
       }
     >>
    ;
not_expr>[Parser::AP_ASTNode ret]
    : << std::vector<calc::Id> signs;
      >>
     ( s:TOK_NOT << signs.push_back(genId(s)); >>
     )* misc_expr>[$ret]
     <<
       for(int i = (int)(signs.size()-1);i>=0; i--) {
        AP_ASTExpr e(createExpr(signs[i].position(), calc::major2op(OP_NOT_)));
        e->transferArg($ret.release());
        $ret=e;
       }
     >>
    ;

//! other expr not part of std. mathematical repertoire
misc_expr>[Parser::AP_ASTNode ret]:
      TOK_LP expr>[$ret] TOK_RP
    | << MAJOR_CODE c=OP_IFTHEN;
         AP_ASTNode condn,truen,falsen;
      >>
      opS:TOK_IF TOK_LP expr>[condn]
      THEN_GROUP expr>[truen]
      { ELSE_GROUP expr>[falsen]  << c=OP_IFTHENELSE; >>
      } TOK_RP
      <<
         AP_ASTExpr e(createExpr(opS,calc::major2op(c)));
         e->transferArg(condn.release());
         e->transferArg(truen.release());
         if (c==OP_IFTHENELSE)
           e->transferArg(falsen.release());
         $ret=e;
      >>
    | convF:TOK_CONV_F TOK_LP
      (
        << calc::Id nr; >>
        number>[nr] TOK_RP
        <<  $ret.reset(createCastedASTNumber(convF,nr)); >>
      |
        << AP_ASTNode a; >>
        expr>[a] TOK_RP
        <<
          AP_ASTExpr e(createExpr(convF,tokenOp(convF)));
          e->transferArg(a.release());
          $ret=e;
        >>
      )
    | << calc::Id nr; >>
      number>[nr]
      << $ret.reset(new calc::ASTNumber(nr)); >>
    | // a parameter as a reference
      r:TOK_REFERENCE
      <<
         $ret.reset(new calc::ASTPar(genId(r)));
      >>
    | theId:TOK_ID
       <<
              // empty node list by default
              AP_ASTNodeVector args(new calc::ASTNodeVector());
       >>
      (
       TOK_LP  // <-- theId is a function name
           <<
              const calc::Operator *o=expectFunction(genId(theId));
           >>
           { exprList>[args] }
       TOK_RP
        <<
          AP_ASTExpr e(createExpr(theId,o));
          e->transferFunctionArgs(args.release());
          $ret=e;
        >>
      | TOK_2COL             // <-- theId is a modellink
        nameAfter:TOK_ID      // name of method
      <<
         calc::Id nameBefore = genId(theId);
         std::string strArg;
      >>
      modelLinkArgs[strArg]>[args]
      <<
          std::auto_ptr<LinkInExpr> e(new LinkInExpr(nameBefore,genId(nameAfter),strArg));
          // Hack a method with no args, yields a 0-ptr!
          if (!args.get())
              args.reset(new calc::ASTNodeVector());
          e->transferFunctionArgs(args.release());
          $ret=AP_ASTNode(e.release());
      >>
      | // <-- if not a function or method then theId is a par with opt. indices
        <<
          calc::ASTPar p(genId(theId));
        >>
         ( arrayIndex>[calc::Id a] << p.pushBackIndex(a); >> )*
        <<
           $ret.reset(new calc::ASTPar(p));
        >>
      )
    ;
    exception
      default:
      <<
         checkParseError();
      >>

modelLinkArgs[std::string& strArg]>[Parser::AP_ASTNodeVector ret]
    : TOK_LP
    {  ref:TOK_REFERENCE << strArg = genId(ref).name(); >>
       {  TOK_COMMA }
    }
    { exprList>[$ret] }
      TOK_RP
    ;

/* arguments of functions, 1 or more expr's */
exprList>[Parser::AP_ASTNodeVector ret]
    :<<
       $ret.reset(new calc::ASTNodeVector());
       AP_ASTNode e;
      >>
     expr>[e] << $ret->transferPushBack(e.release()); >>
     ( TOK_COMMA expr>[e] << $ret->transferPushBack(e.release()); >>
     )*
    ;

/*************/
/* PARAMETER */
/*************/
parWithIndeces>[calc::ASTPar p]
        : << calc::Id a;  >>
        (
         i:TOK_ID         << $p = calc::ASTPar(genId(i)); >>
         ( arrayIndex>[a] << $p.pushBackIndex(a);    >>
         )*
        | r:TOK_REFERENCE << $p = calc::ASTPar(genId(r)); >>
        )
        ;

arrayIndex>[calc::Id index]
        : TOK_LB id:TOK_ID << $index = genId(id); >> TOK_RB
        ;

// a (possible quoted) id
qid > [calc::Id s] :  i:TOK_ID << $s = genId(i); >>
                      |  r:TOK_REFERENCE << $s = genId(r); >>
                      ;

/*************/
/* NUMBERS   */
/*************/
number > [calc::Id s]
    : << calc::Id signS,valS; >>
      { sign>[signS]
      }
       unsignedNumber>[valS]
       <<
          if (signS.name().empty())
           $s = valS;
          else {
           // TODO er zouden spaties tussen signS en valS kunnen zitten!
           signS.setName(signS.name()+valS.name());
           $s = signS;
          }
       >>
    ;

unsignedNumber > [calc::Id s]
    : v1:TOK_INT
      << $s = genId(v1); >>
    | v2:TOK_FLOAT
      << $s = genId(v2); >>
    ;

sign > [calc::Id s]
    : v1:TOK_PLUS
      << $s =genId(v1); >>
    | v2:TOK_MINUS
      << $s =genId(v2); >>
    ;

/***********************************/
/* single nr or par as part of AST */
/***********************************/
nrOrPar>[Parser::AP_ASTId n]:
    << calc::Id i; >>
      number>[i] << $n.reset(new ASTNumber(i));  >>
    | qid   >[i] << $n.reset(new ASTPar(i));     >>
    ;
/***********************************/
/* single nr or par as part of AST */
/***********************************/
nrOrId>[calc::Id n]:
    << calc::Id i; >>
      number>[i] << $n=i; >>
    | qid   >[i] << $n=i; >>
    ;
}
