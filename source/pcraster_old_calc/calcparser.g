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
  class Constant;
  struct ParsReportMoment;
  class ParsIndex;
  class ParsPar;
  class DefPar;
  class UsePar;
  struct ConstructPar;
  class  StatementBlock;
  class  WriteInfo;
  class  IdList;
  class  FieldExpr;
  class  Operator;
  class  TimerValue;
  class  Script;
  class  RunSettings;
}

#ifndef INCLUDED_CALC_FIELDEXPRARGS
#include "calc_fieldexprargs.h"
#define INCLUDED_CALC_FIELDEXPRARGS
#endif
#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#include "ATokPtr.h"
>>

<<

#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif
#ifndef INCLUDED_CALC_ARRAYDEFVECTOR
#include "calc_arraydefvector.h"
#define INCLUDED_CALC_ARRAYDEFVECTOR
#endif
#ifndef INCLUDED_CALC_ASSIGNMENT
#include "calc_assignment.h"
#define INCLUDED_CALC_ASSIGNMENT
#endif
#ifndef INCLUDED_CALC_BRANCHEXPRIMPL
#include "calc_branchexprimpl.h"
#define INCLUDED_CALC_BRANCHEXPRIMPL
#endif
#ifndef INCLUDED_CALC_CONSTANT
#include "calc_constant.h"
#define INCLUDED_CALC_CONSTANT
#endif
#ifndef INCLUDED_CALC_CONSTRUCTPAR
#include "calc_constructpar.h"
#define INCLUDED_CALC_CONSTRUCTPAR
#endif
#ifndef INCLUDED_CALC_DEFPAR
#include "calc_defpar.h"
#define INCLUDED_CALC_DEFPAR
#endif
#ifndef INCLUDED_CALC_DOUBLEASS
#include "calc_doubleass.h"
#define INCLUDED_CALC_DOUBLEASS
#endif
#ifndef INCLUDED_CALC_DYNAMICSECTION
#include "calc_dynamicsection.h"
#define INCLUDED_CALC_DYNAMICSECTION
#endif
#ifndef INCLUDED_CALC_FIELDLEAF
#include "calc_fieldleaf.h"
#define INCLUDED_CALC_FIELDLEAF
#endif
#ifndef INCLUDED_CALC_FIELDNRPARAMETER
#include "calc_fieldnrparameter.h"
#define INCLUDED_CALC_FIELDNRPARAMETER
#endif
#ifndef  INCLUDED_CALC_INDEXTABLE
#include "calc_indextable.h"
#define  INCLUDED_CALC_INDEXTABLE
#endif
#ifndef INCLUDED_CALC_FOREACH
#include "calc_foreach.h"
#define INCLUDED_CALC_FOREACH
#endif
#ifndef INCLUDED_CALC_REPEATUNTIL
#include "calc_repeatuntil.h"
#define INCLUDED_CALC_REPEATUNTIL
#endif
#ifndef INCLUDED_CALC_IDLIST
#include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif
#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif
#ifndef INCLUDED_CALC_LOOKUPEXPR
#include "calc_lookupexpr.h"
#define INCLUDED_CALC_LOOKUPEXPR
#endif
#ifndef INCLUDED_CALC_MODELLINKINIT
#include "calc_modellinkinit.h"
#define INCLUDED_CALC_MODELLINKINIT
#endif
#ifndef INCLUDED_CALC_MODELLINKMETHODEXPR
#include "calc_modellinkmethodexpr.h"
#define INCLUDED_CALC_MODELLINKMETHODEXPR
#endif
#ifndef INCLUDED_CALC_MODELLINKMETHODSTATEMENT
#include "calc_modellinkmethodstatement.h"
#define INCLUDED_CALC_MODELLINKMETHODSTATEMENT
#endif

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif
#ifndef INCLUDED_CALC_PARSINDEX
#include "calc_parsindex.h"
#define INCLUDED_CALC_PARSINDEX
#endif
#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif
#ifndef INCLUDED_CALC_SCRIPT
#include "calc_script.h"
#define INCLUDED_CALC_SCRIPT
#endif
#ifndef INCLUDED_CALC_STACKINPUT
#include "calc_stackinput.h"
#define INCLUDED_CALC_STACKINPUT
#endif
#ifndef INCLUDED_CALC_STDOUTSTAT
#include "calc_stdoutstat.h"
#define INCLUDED_CALC_STDOUTSTAT
#endif
#ifndef INCLUDED_CALC_TIMEINPUTEXPR
#include "calc_timeinputexpr.h"
#define INCLUDED_CALC_TIMEINPUTEXPR
#endif
#ifndef INCLUDED_CALC_TIMEOUTPUT
#include "calc_timeoutput.h"
#define INCLUDED_CALC_TIMEOUTPUT
#endif
#ifndef INCLUDED_CALC_TIMERVALUE
#include "calc_timervalue.h"
#define INCLUDED_CALC_TIMERVALUE
#endif
#ifndef INCLUDED_CALC_USEPAR
#include "calc_usepar.h"
#define INCLUDED_CALC_USEPAR
#endif
#ifndef INCLUDED_CALC_WRITEINFO
#include "calc_writeinfo.h"
#define INCLUDED_CALC_WRITEINFO
#endif

#ifndef INCLUDED_LEXGRAMMAR
#include "lexgrammar.h"
#define INCLUDED_LEXGRAMMAR
#endif

typedef calc::LexToken ANTLRToken;

#define NEW_EXPR(opTokenPtr,op,args) \
         ( new calc::BranchExprImpl(element(opTokenPtr),(op),(args)))

calc::Script *Parser::script() const
{
  return d_script;
}

calc::Element Parser::element(const ANTLRTokenPtr at) const
{
    return calc::Element(script(),mytoken(at)->position());
}

calc::Symbol Parser::symbol(const ANTLRTokenPtr at) const
{
    return calc::Symbol(script(), mytoken(at));
}

const calc::Operator& Parser::tokenOp(const ANTLRTokenPtr at) const
{
    return calc::major2op(mytoken(at)->opCode());
}

//! current token saus this is an exp where we expect a file
void Parser::expectFile(const std::string& fileDescr) {
    symbol(LT(1)).posError("Expression illegal here, expecting name of "+fileDescr);
}

void Parser::checkParseError() {
   const ANTLRTokenPtr at(LT(1));
   calc::Symbol p=symbol(at);
   // pcrcalc/test34
   std::ostringstream msg;
   bool kw=mytoken(at)->isKeyword();
   msg  << "Syntax error at " << (kw ? "keyword ":"symbol ") << p.qName();
   if (kw)
    msg  << "\n Keywords can not be used as names for files or variables"
         << "\n Keywords must be placed in a specific order";
   p.posError(msg.str());
}

void Parser::errorFuncInDynamic(const calc::Symbol& s) const {
  s.posError("function "+s.qName()+
            " is only legal in the dynamic section");
}

void Parser::illegalFunc(const calc::Symbol& s) const {
  // pcrcalc/test41
  s.posError("function "+s.qName()+" illegal here");
}

void Parser::notAFunc(const calc::Symbol& s) const {
  s.posError(s.qName()+" is not a function");
}

calc::Constant Parser::createCastedConstant(
    const ANTLRTokenPtr   convF,
    const calc::Symbol& nr) const
{
  calc::Symbol s(symbol(convF));
  std::ostringstream name;
  name << s.name() << "(" << nr.name() << ")";
  s.setName(name.str());
  return calc::Constant(s, tokenOp(convF).vs(),nr);
}
>>

// TOK_DOLLAR only used in lexer when sealing
#token TOK_DOLLAR

#token TOK_EOF

#token TOK_BINDING
#token TOK_AREAMAP
#token TOK_TIMER
#token TOK_MODEL
#token TOK_REPORT
#token TOK_DYNAMIC
#token TOK_INITIAL
#token TOK_FILEOUTPUT
#token TOK_MODELLINK
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
#token TOK_LDD
#token TOK_DIRECTIONAL
#token TOK_SCALAR
#token TOK_BOOLEAN
#token TOK_NOMINAL
#token TOK_ORDINAL

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
#token TOK_TIMEOUTPUT
#token TOK_TIMEINPUT
#token TOK_CONV_F
#token TOK_LOOKUP_F
#token TOK_TIMEIN_F
#token TOK_INDEX_F
#token TOK_POW
#tokclass  ADD_GROUP    { TOK_PLUS TOK_MINUS }
#tokclass  MULT_GROUP    { TOK_STAR TOK_MOD TOK_IDIV TOK_FDIV }
#tokclass  ID_GROUP    { TOK_ID TOK_REFERENCE }

class Parser {
<<
private:
    calc::Script *d_script;


    // not const due to LT(1) use
    void                     expectFile(const std::string& fileDescr);
    void                     checkParseError();

    void                     errorFuncInDynamic(const calc::Symbol& s) const;
    void                     illegalFunc(const calc::Symbol& s) const;
    void                     notAFunc(const calc::Symbol& s) const;

    calc::Element            element(const ANTLRTokenPtr at) const;
    calc::Symbol             symbol(const ANTLRTokenPtr at) const;
    const calc::Operator&    tokenOp(const ANTLRTokenPtr at) const;
    calc::Constant           createCastedConstant(
                                     const ANTLRTokenPtr   convF,
                                     const calc::Symbol& nr) const;

public:
    //! ALWAYS AS FIRST STATEMENT! should go in ctor, if we could set the Ctor
    void initialize(calc::Script& script) {
          d_script = &script;
          ANTLRParser::init();
    };

    //! script  where parse will add to
    calc::Script *script() const;
>>

externalBindings[calc::RunSettings& rs]:
               (
                left:TOK_ID TOK_IS
                 << calc::Symbol s; >>
                 ( number>[s]
                 | qid>[s]
                 )
                  <<
                   rs.add(symbol(left), s);
                  >>
                 { TOK_SC }
              )* TOK_EOF
        ;
        exception
         default:
         <<
            checkParseError();
         >>

model  : { TOK_MODEL }
           codeBlocks[script()] TOK_EOF
         | modelProlog modelCode TOK_EOF
       ;
       exception
         default:
         <<
            checkParseError();
         >>

modelProlog : { TOK_BINDING bindingSection } { areaMapSection } { timer }
            ;
modelCode : { ( TOK_INITIAL | TOK_MODEL ) codeBlocks[script()] }
        {
          pos:TOK_DYNAMIC
          <<
              calc::Element p(element(pos));
              if (!script()->isDynamicModel()) // pcrcalc/test8a
               p.posError("There is a dynamic section but no timer section");
              // create and add it to the script
              calc::DynamicSection *n= new calc::DynamicSection(p,script());
              script()->addStatement(n);
          >>
          codeBlocks[n]
        }
        ;

bindingSection    : ( bindingDefinition )*
          << script()->evaluateBindings(); >>
        ;


areaMapSection : << calc::Symbol s; >>
         TOK_AREAMAP qid>[s] TOK_SC
         <<
           // can be binded symbol test46
           script()->setAreaMap(s);
         >>
           ;

timer  : << calc::TimerValue start,end,step; >>
        TOK_TIMER
         ((
            timerValue>[start]
            timerValue>[end]
            timerValue>[step]
            <<
              calc::checkTimerSection(start,end,step);
              script()->setTimer(start.value(),end.value(),step.value());
            >>
          ) | tssID:TOK_ID
              <<
                 script()->setTimer(symbol(tssID));
              >>
        ) TOK_SC
        ( reportMomentDef)*
        ;

timerValue > [calc::TimerValue t]:
        <<  calc::Symbol sym; >>
            ( number>[sym] | qid>[sym] )
        <<
            $t = calc::TimerValue(sym);
        >>
        ;

reportMomentDef : << std::vector<calc::ParsReportMoment> m; calc::Symbol s;>>
           reportId:TOK_ID << s = symbol(reportId); >>
             TOK_IS reportMoments[m] TOK_SC
           << script()->addReport(
               new calc::ReportDefinition(s,m,(int)script()->nrTimeSteps()));
           >>
        ;


bindingDefinition: parWithIndeces[script()]>[calc::DefPar p] TOK_IS bindingRight[p] TOK_SC
         ;

bindingRight[const calc::DefPar& par ]
        : << calc::Symbol right; >>
           qid>[right]
         <<
           if ($par.isArray())
              $par.posError("Illegal construct");
           script()->addBinding($par,right);
         >>
         { TOK_LP << illegalFunc(right); >> }
        | << calc::Symbol nr; >>
           number>[nr]
           <<
             if ($par.isArray())
                 $par.posError("Illegal construct");
             script()->addBinding($par,nr);
         >>
        | << calc::Symbol nr; >>
         convF:TOK_CONV_F TOK_LP number>[nr]
         <<
           if ($par.isArray())
               $par.posError("Illegal construct");
         >>
         TOK_RP
         << // add numeric binding with a typecast
             (void)createCastedConstant(convF,nr); // pcrcalc/test42
             script()->addBinding($par,nr, tokenOp(convF).vs());
         >>
        | << std::vector<calc::ParsIndex *> indDef; >>
           TOK_LB
           << // par = [ index1, index2 ]
           if ($par.isArray())
               $par.posError("Illegal construct");
           >>
          indecesDef[indDef]
           TOK_RB
          << // array definition
             script()->addSymbol(new calc::ArrayDefinition(par,indDef));
          >>
        |<< calc::Symbol qId; >>
         indF:TOK_INDEX_F
         TOK_LP qid>[qId]
              { ~TOK_RP << expectFile("index-table"); >> }
         TOK_RP
         <<
           calc::BindedSymbol indexTableFile(qId);
           indexTableFile.setInputFilePath();
           if (!par.isArray()) {
            // pcrcalc/test275
            calc::Symbol f = symbol(indF);
            f.posError("Function "+f.qName()+" only allowed on array parameters");
           }
           calc::IndexTable *p =
              dynamic_cast<calc::IndexTable *>
                     (par.block()->findSymbol(&indexTableFile,VS_INDEXTABLE,false));
           bool firstUse = !p;
           if (firstUse) {
             try {
               expectedFileType(indexTableFile.externalName(),VS_INDEXTABLE);
              } catch (com::Exception& msg) {
               indexTableFile.posError(msg.messages());
              }
              p = new calc::IndexTable(indexTableFile,par.descriptor());
           }
           // add these 2 symbols in this order; above func does a
           // more type specific (check in index type) test
           // (see test/pcrcalc277)
           try { script()->addSymbol(
                       par.indexTable(p,true,tokenOp(indF)));
           } catch (com::Exception& msg) {
             indexTableFile.posError(msg.messages());
           }
           if (firstUse) // pcrcalc/test280b
             script()->addSymbol(p);
         >>
        ;

indecesDef[std::vector<calc::ParsIndex* >& indDef]
        : << calc::ParsIndex *p;>>
                  indexDef>[p]
            << indDef.push_back(p); >>
            ( TOK_COMMA indexDef>[p]
              << indDef.push_back(p); >>
                        )*
        ;

indexDef>[calc::ParsIndex *index ]
        : << bool on=true; >>
          {
            TOK_PLUS
            |
            TOK_MINUS << on=false; >>
          }
          name:TOK_ID
          (
           (
            TOK_IS (
                 qid > [calc::Symbol extName]
              <<
                 $index = new calc::ParsIndexName(on,symbol(name),extName);
              >>
                |
             idList>[ calc::IdList set_list ]
              << $index = new calc::ParsIndexSet(on,symbol(name),set_list); >>
            )
           )
           | << $index = new calc::ParsIndexName(on,symbol(name)); >>
          )
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
             << try { $p.check();
                } catch (com::Exception& msg) {
                     element(start).posError(msg.messages());
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
        : << std::vector<calc::Symbol> list; calc::Symbol s;>>
           TOK_LP
            firstId:TOK_ID      << list.push_back(symbol(firstId)); >>
            ( TOK_COMMA nextId:TOK_ID << list.push_back(symbol(nextId));  >>
            )*
           TOK_RP
          << $i = calc::IdList(list); >>
        ;

codeBlocks[calc::StatementBlock *inclIn]
        : (
             foreach[$inclIn]
           | repeat[$inclIn]
           |(
             << calc::InnerStatementBlock *b =
                   new calc::InnerStatementBlock(element(LT(1)), inclIn);
                inclIn->addStatement(b);
             >>
             codeBlock[b] ( codeBlock[b] )*
            )
          )*
                ;

codeBlock[calc::StatementBlock *inclIn]:
          << bool hasReport=false;
             const calc::Report *r=0;
             static int  inSituNr=0; // harmless static
          >>
          { TOK_REPORT
            <<
              hasReport=true;
              script()->setReportFound();
              std::vector<calc::ParsReportMoment> inSitu;
             >>
            {
              TOK_LP
              (
               v:TOK_ID << r = script()->findReport(symbol(v)); >>
               | reportMoments[inSitu]
               <<
                  char buf[8];
                  sprintf(buf,"%d",inSituNr++);
                  calc::Symbol s(script(),buf,0);
                  calc::ReportDefinition *rd;
                  rd = new calc::ReportDefinition(
                         script()->generatedSymbol("reportMoments",buf),
                         inSitu,(int)script()->nrTimeSteps());
                  r=rd;
                  script()->addReport(rd);
                >>
              )
              TOK_RP
            }
          } assignment[$inclIn,calc::WriteInfo(script(),hasReport,r,inclIn->inDynamic())]
        | TOK_FILEOUTPUT expr[$inclIn]>[calc::FieldExpr *r_expr] statementSep
          <<
             inclIn->addStatement(new calc::StdoutStatement(r_expr));
              >>
        | << calc::FieldExprArgs args;
             std::string strArg;
           >>
         TOK_MODELLINK modelId:TOK_ID TOK_IS modelName:TOK_ID modelLinkArgs[$inclIn,strArg,args] statementSep
                 << inclIn->addStatement(new
                    calc::ModelLinkInit(symbol(modelId),symbol(modelName),strArg,args));
                 >>
        ;

foreach[calc::StatementBlock *inclIn]
        : << calc::ForEach *f=0;
             calc::Symbol iter;
             calc::IdList in,except,s; bool ascending=true;  >>
          posF:TOK_FOREACH
          iterId:TOK_ID
          TOK_IN (  inId:TOK_ID << in = symbol(inId); >>
              | idList>[in]
             )
          { TOK_EXCEPT (
                  exceptId:TOK_ID << except = symbol(exceptId); >>
                 | idList>[except]
                )
          }
          TOK_LC
              << f =
                 new calc::ForEach(element(posF),inclIn,
                 symbol(iterId),in,except,s,ascending);
                 inclIn->addStatement(f);
              >>
              codeBlocks[f]
           TOK_RC
        ;

repeat[calc::StatementBlock *inclIn]:
          << calc::RepeatUntil *ru=0;
             calc::FieldExpr   *condition;
          >>
          posR:TOK_REPEAT
          TOK_LC
              << ru = new calc::RepeatUntil(element(posR),inclIn);
                 inclIn->addStatement(ru);
              >>
              codeBlocks[ru]
          TOK_RC TOK_UNTIL expr[ru]>[condition] TOK_SC
              << {
                 calc::FieldExprArgs args(1);
                 args[0] = condition;
                 ru->addCondition(
                  new calc::BranchExprImpl(
                       *(args[0]),calc::major2op(OP_TEST_UNTIL),args));
                } >>
        ;

/**************************/
/* STATEMENTS/ASSIGNMENTS */
/**************************/

assignment[calc::StatementBlock *inclIn,const calc::WriteInfo& w]
     :
      parWithIndeces[inclIn]>[calc::UsePar par]
      assignmentTail[$inclIn,w,par]
      statementSep
     ;
     exception
         default:
         << /* see pcrcalc/test34 without this no cleaning? */;
            checkParseError();
         >>

assignmentTail[calc::StatementBlock *inclIn,
               const calc::WriteInfo& w,
                     calc::UsePar&   par]
          : << calc::FieldExprArgs args; >>
          TOK_IS
          (
           TOK_TIMEOUTPUT TOK_LP exprList[$inclIn,args] TOK_RP
           <<
            inclIn->addStatement(new calc::Timeoutput(w,par,args));
           >>
           | expr[$inclIn]>[calc::FieldExpr *r_expr]
            <<
            inclIn->addStatement(
                new calc::Assignment(inclIn,w,par,r_expr));
            >>
          )
            | ass:TOK_ASSOP expr[$inclIn]>[calc::FieldExpr *r_expr]
          <<
            const calc::Operator& op = tokenOp(ass);
              calc::FieldExprArgs args(2);
              try {
                args[0] = new calc::FieldLeaf(par);
                args[1] = r_expr;
              } catch (...) {
                delete r_expr;
                throw;
              }
              calc::FieldExpr *newExpr= NEW_EXPR(ass,op,args);
              inclIn->addStatement(
                  new calc::Assignment(inclIn,w,par,newExpr));
              >>
        | << calc::FieldExprArgs args;
               std::vector<calc::UsePar> leftPars;
               leftPars.push_back(par);
           >>
            (
               TOK_COMMA parWithIndeces[inclIn]>[calc::UsePar par2]
               <<
                  leftPars.push_back(par2);
               >>
            )+
                TOK_IS
          theId:TOK_ID // <- either method or one of double funcs
          (
          (
            TOK_2COL
                methodId:TOK_ID      // name of method
            << calc::FieldExprArgs args;
               calc::Symbol modelName = symbol(theId);
               std::string strArg;
             >>
             modelLinkArgs[$inclIn,strArg,args]
             << inclIn->addStatement(new
                    calc::ModelLinkMethodStatement(
                         inclIn,w,
                         symbol(theId),symbol(methodId),
                         leftPars, strArg,args));
             >>
         )
         |
         (
              TOK_COMMA  // it is a1,a2 = f1,f2(.....)
              fid2:TOK_ID
              << calc::Symbol sym1 = symbol(theId);
                     const calc::Operator& op1 = calc::funcName2op(sym1.name());
                 if (op1.opCode() == OP_NOP) notAFunc(sym1);
                 calc::Symbol sym2 = symbol(fid2);
                     const calc::Operator& op2 = calc::funcName2op(sym2.name());
                 if (op2.opCode() == OP_NOP) notAFunc(sym2);
              >>
            TOK_LP exprList[$inclIn,args] TOK_RP
            <<
            // CW TODO check if leftPars.size() == 1
            // syntax error otherwise
            inclIn->addStatement(new
                calc::DoubleAssignment(inclIn,leftPars[0],w,
                    leftPars[0],leftPars[1],sym1,op1,op2,args));
            // can not do this in the syntax anymore
            // WrongDoubleAssignment(func.pos,*(func.op),*(f2.op))
              >>
         )
         )
           ;

statementSep    : TOK_SC
        | TOK_EOF
            ;

/******************************/
/* expr: creating a FieldExpr */
/******************************/

expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << calc::FieldExpr *right; >>
      xor_expr[$inclIn]>[$ret]
      (
        opS:TOK_OR xor_expr[$inclIn]>[right]
      << calc::FieldExprArgs args(2);
         args[0] = $ret;
         args[1] = right;
         $ret = NEW_EXPR(opS,calc::major2op(OP_OR),args);
      >>
      )*
    ;
     exception
         default:
         << /* see pcrcalc/test34 without this no cleaning? */;
            checkParseError();
         >>

xor_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << calc::FieldExpr *right; >>
      and_expr[$inclIn]>[$ret]
      (
        opS:TOK_XOR and_expr[$inclIn]>[right]
      << calc::FieldExprArgs args(2);
         args[0] = $ret;
         args[1] = right;
         $ret = NEW_EXPR(opS,calc::major2op(OP_XOR),args);
      >>
      )*
    ;
and_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << calc::FieldExpr *right; >>
      eq_expr[$inclIn]>[$ret]
      (
        opS:TOK_AND eq_expr[$inclIn]>[right]
      << calc::FieldExprArgs args(2);
         args[0] = $ret;
         args[1] = right;
         $ret = NEW_EXPR(opS,calc::major2op(OP_AND),args);
      >>
      )*
    ;

eq_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << calc::FieldExpr *right; >>
      comp_expr[$inclIn]>[$ret]
      (
        opS:TOK_EQ comp_expr[$inclIn]>[right]
      << calc::FieldExprArgs args(2);
         args[0] = $ret;
         args[1] = right;
         $ret = NEW_EXPR(opS,tokenOp(opS),args);
      >>
      )*
    ;
comp_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << calc::FieldExpr *right; >>
      add_expr[$inclIn]>[$ret]
      (
        opS:TOK_COMP add_expr[$inclIn]>[right]
      << calc::FieldExprArgs args(2);
         args[0] = $ret;
         args[1] = right;
         $ret = NEW_EXPR(opS,tokenOp(opS),args);
      >>
      )*
    ;
add_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << calc::FieldExpr *right;
      >>
      mult_expr[$inclIn]>[$ret]
      (
        opS:ADD_GROUP mult_expr[$inclIn]>[right]
      << calc::FieldExprArgs args(2);
         args[0] = $ret;
         args[1] = right;
         const calc::Operator& op = calc::major2op(
             opS->getType() == TOK_PLUS ? OP_BADD:OP_BMIN);
         $ret = NEW_EXPR(opS,op,args);
      >>
      )*
    ;
mult_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << calc::FieldExpr *right; >>
      pow_expr[$inclIn]>[$ret]
      (
        opS:MULT_GROUP pow_expr[$inclIn]>[right]
      << calc::FieldExprArgs args(2);
         args[0] = $ret;
         args[1] = right;
         $ret   = NEW_EXPR(opS,tokenOp(opS),args);
      >>
      )*
    ;
pow_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << calc::FieldExpr *right; >>
      sign_expr[$inclIn]>[$ret]
      (
        opS:TOK_POW sign_expr[$inclIn]>[right]
      << calc::FieldExprArgs args(2);
         args[0] = $ret;
         args[1] = right;
         $ret = NEW_EXPR(opS,calc::major2op(OP_POW),args);
      >>
      )*
    ;
sign_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << std::vector<calc::Symbol> signs;
      >>
     (
      s:ADD_GROUP << signs.push_back(symbol(s)); >>
     )* not_expr[$inclIn]>[$ret]
     <<
       /* solve Nominal/Ordinal not negative here
          something like this
       if ($ret->isConstant()) {
         Constant *c;
         for(size_t i = 0; i < signs.size(); ++i)
            if (signs[i].name() == "-")
              c->setValue(-c->value();
       } else  */
       for(int i = (int)(signs.size()-1);i>=0; i--) {
        calc::FieldExprArgs args(1);
        args[0] = $ret;
        const calc::Operator& op = calc::major2op(
            signs[i].name() == "+" ? OP_UADD:OP_UMIN);
        $ret = new calc::BranchExprImpl(signs[i],op,args);
       }
     >>
    ;
not_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : << std::vector<calc::Symbol> signs;
      >>
     ( s:TOK_NOT << signs.push_back(symbol(s)); >>
     )* misc_expr[$inclIn]>[$ret]
     <<
       for(int i = (int)(signs.size()-1);i>=0; i--) {
        calc::FieldExprArgs args(1);
        args[0] = $ret;
        $ret = new calc::BranchExprImpl(signs[i],calc::major2op(OP_NOT),args);
       }
     >>
    ;

//! other expr not part of std. mathematical repertoire
misc_expr[calc::StatementBlock *inclIn]>[calc::FieldExpr *ret]
    : TOK_LP expr[$inclIn]>[$ret] TOK_RP
    | << MAJOR_CODE c=OP_IF; calc::FieldExpr *f;
         calc::FieldExprArgs args;
      >>
      opS:TOK_IF TOK_LP expr[$inclIn]>[f] << args.push_back(f); >>
      THEN_GROUP expr[$inclIn]>[f]    << args.push_back(f); >>
      { ELSE_GROUP expr[$inclIn]>[f]  << c=OP_IF_ELSE; args.push_back(f); >>
      } TOK_RP
      <<
         $ret = NEW_EXPR(opS,calc::major2op(c),args);
      >>
    | convF:TOK_CONV_F TOK_LP
      (
        << calc::Symbol nr; >>
        number>[nr] TOK_RP
        <<  $ret = new calc::Constant(createCastedConstant(convF,nr)); >>
      |
        << calc::FieldExpr *e; >>
        expr[$inclIn]>[e] TOK_RP
        <<
          calc::FieldExprArgs args(1);
          args[0] = e;
          $ret = NEW_EXPR(convF,tokenOp(convF),args);
        >>
      )
    | << calc::FieldExprArgs args; >>
      lookF:TOK_LOOKUP_F
      TOK_LP    parWithIndeces[$inclIn]>[calc::UsePar table]
      TOK_COMMA exprList[$inclIn,args]
      TOK_RP
      <<
         try {
           $ret =
            new calc::LookupExpr(element(lookF),tokenOp(lookF),table,args);
         } catch (...) {
           calc::cleanUp(args);
           throw;
         }
      >>
    | << calc::FieldExprArgs args; >>
      tssF:TOK_TIMEIN_F
       TOK_LP parWithIndeces[$inclIn]>[calc::UsePar tss] TOK_COMMA
              exprList[$inclIn,args] TOK_RP
      <<
         try {
          // pcrcalc/test37
          if (!inclIn->inDynamic())
            errorFuncInDynamic(symbol(tssF));
           $ret =
             new calc::TimeinputExpr(element(tssF),tokenOp(tssF),tss,args);
         } catch (...) {
           calc::cleanUp(args);
           throw;
         }
      >>
    | << calc::Symbol nr; >>
      number>[nr]
      << $ret = new calc::Constant(nr); >>
    | // a parameter as a reference
      r:TOK_REFERENCE
      <<
         calc::UsePar cpar(calc::ConstructPar(inclIn,symbol(r)));
         $ret = new calc::FieldLeaf(cpar);
      >>
    | << calc::Symbol stackSuffix; >>
      indF:TOK_TIMEINPUT
         TOK_LP
       {
         ~ID_GROUP
         << // pcrcalc/test257
            expectFile("map stack");
         >>
       }
       qid>[stackSuffix]
       {
          ~TOK_RP
            << // pcrcalc/test257a
                   expectFile("map stack");
             >>
        }
         TOK_RP
      <<
          calc::Symbol fSym(symbol(indF));
          const calc::Operator& fOp = calc::funcName2op(fSym.name());
          if (!inclIn->inDynamic())
                errorFuncInDynamic(fSym);
          $ret = new calc::StackInput(fSym, calc::BindedSymbol(stackSuffix),
                       fOp.opCode()==OP_TIMEINPUTSPARSE);
      >>
    | theId:TOK_ID
      (
       TOK_LP  // <-- theId is a function name
           << calc::FieldExprArgs args;
              calc::Symbol name = symbol(theId);
              const calc::Operator& o = calc::funcName2op(name.name());
          if (o.opCode() == OP_NOP)
            notAFunc(name);
              if (!inclIn->inDynamic())
            switch(o.opCode()) {
             case OP_TIMEINPUT:
             case OP_TIME:
             case OP_TIMESLICE:
                errorFuncInDynamic(name);
             default: ;
           }
           >>
           { exprList[$inclIn,args] }
       TOK_RP
        <<
         $ret = new calc::BranchExprImpl(name,o,args);
        >>
      | TOK_2COL             // <-- theId is a modellink
        methodId:TOK_ID      // name of method
      << calc::FieldExprArgs args;
         calc::Symbol modelName = symbol(theId);
          std::string strArg;
      >>
      modelLinkArgs[$inclIn,strArg,args]
      << $ret = new calc::ModelLinkMethodExpr(modelName,symbol(methodId),
                                         strArg,args);
      >>
      | // <-- if not a function or method then theId is a par with opt. indices
        << 
          calc::ConstructPar p(inclIn,symbol(theId));
        >>
         ( arrayIndex>[calc::Symbol a] << p.d_index.push_back(a); >> )*
        <<
           calc::UsePar pp(p);
           $ret = new calc::FieldLeaf(pp);
        >>
      )
    ;

modelLinkArgs[calc::StatementBlock *inclIn, std::string& strArg, calc::FieldExprArgs& args]
    : TOK_LP
    {  ref:TOK_REFERENCE << strArg = symbol(ref).name(); >>
       {  TOK_COMMA }
    }
    { exprList[$inclIn,args] }
      TOK_RP
    ;

/* arguments of functions, 1 or more expr's */
exprList[calc::StatementBlock *inclIn, calc::FieldExprArgs& args]
    :<< calc::FieldExpr *e; >>
     expr[$inclIn]>[e] << args.push_back(e); >>
     ( TOK_COMMA expr[$inclIn]>[e] << args.push_back(e); >>
     )*
    ;

/*************/
/* PARAMETER */
/*************/
parWithIndeces[calc::StatementBlock *s]>[calc::ConstructPar p]
        : << calc::Symbol a; $p.d_block=s; >>
        (
         i:TOK_ID  << $p.d_name = symbol(i); >>
         ( arrayIndex>[a] << $p.d_index.push_back(a); >>
         )*
        | r:TOK_REFERENCE << $p.d_name = symbol(r); >>
        )
        ;

arrayIndex>[calc::Symbol index]
        : TOK_LB id:TOK_ID << $index = symbol(id); >> TOK_RB
        ;

// a (possible quoted) id
qid > [calc::Symbol s] :  i:TOK_ID << $s = symbol(i); >>
                      |  r:TOK_REFERENCE << $s = symbol(r); >>
                      ;

/*************/
/* NUMBERS   */
/*************/
number > [calc::Symbol s]
    : << calc::Symbol signS,valS; >>
      { sign>[signS]
      }
       unsignedNumber>[valS]
       <<
          if (signS.empty())
           $s = valS;
          else {
           // TODO er zouden spaties tussen signS en valS kunnen zitten!
           signS.setName(signS.name()+valS.name());
           $s = signS;
          }
       >>
    ;

unsignedNumber > [calc::Symbol s]
    : v1:TOK_INT
      << $s = symbol(v1); >>
    | v2:TOK_FLOAT
      << $s = symbol(v2); >>
        ;
sign > [calc::Symbol s]
    :   v1:TOK_PLUS
      << $s =symbol(v1); >>
      |
       v2:TOK_MINUS
       << $s =symbol(v2); >>
        ;
}
