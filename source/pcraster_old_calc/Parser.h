/*
 * Parser: P a r s e r  H e a d e r 
 *
 * Generated from: calcparser.g
 *
 * Terence Parr, Russell Quong, Will Cohen, and Hank Dietz: 1989-1998
 * Parr Research Corporation
 * with Purdue University Electrical Engineering
 * with AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR19
 */

#ifndef Parser_h
#define Parser_h

#ifndef ANTLR_VERSION
#define ANTLR_VERSION 13319
#endif

#include "AParser.h"


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
class Parser : public ANTLRParser {
public:
	static  const ANTLRChar *tokenName(int tk);
protected:
	static  ANTLRChar *_token_tbl[];
private:
#line 354 "calcparser.g"

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
	static SetWordType setwd1[77];
	static SetWordType setwd2[77];
	static SetWordType setwd3[77];
	static SetWordType err1[12];
	static SetWordType setwd4[77];
	static SetWordType setwd5[77];
	static SetWordType setwd6[77];
	static SetWordType ADD_GROUP_set[12];
	static SetWordType setwd7[77];
	static SetWordType MULT_GROUP_set[12];
	static SetWordType THEN_GROUP_set[12];
	static SetWordType ELSE_GROUP_set[12];
	static SetWordType setwd8[77];
	static SetWordType ID_GROUP_set[12];
	static SetWordType err7[12];
	static SetWordType setwd9[77];
	static SetWordType setwd10[77];
	static SetWordType setwd11[77];
private:
	void zzdflthandlers( int _signal, int *_retsignal );

public:
	Parser(ANTLRTokenBuffer *input);
	void externalBindings(int *_retsignal, calc::RunSettings& rs );
	void model(int *_retsignal);
	void modelProlog(int *_retsignal);
	void modelCode(int *_retsignal);
	void bindingSection(int *_retsignal);
	void areaMapSection(int *_retsignal);
	void timer(int *_retsignal);
	 calc::TimerValue   timerValue(int *_retsignal);
	void reportMomentDef(int *_retsignal);
	void bindingDefinition(int *_retsignal);
	void bindingRight(int *_retsignal, const calc::DefPar& par  );
	void indecesDef(int *_retsignal, std::vector<calc::ParsIndex* >& indDef );
	 calc::ParsIndex *   indexDef(int *_retsignal);
	void reportMoments(int *_retsignal, std::vector<calc::ParsReportMoment> &m );
	void reportMoment(int *_retsignal, calc::ParsReportMoment& p );
	 int   reportMomentStep(int *_retsignal);
	  calc::IdList    idList(int *_retsignal);
	void codeBlocks(int *_retsignal, calc::StatementBlock *inclIn );
	void codeBlock(int *_retsignal, calc::StatementBlock *inclIn );
	void foreach(int *_retsignal, calc::StatementBlock *inclIn );
	void repeat(int *_retsignal, calc::StatementBlock *inclIn );
	void assignment(int *_retsignal, calc::StatementBlock *inclIn,const calc::WriteInfo& w );
	void assignmentTail(int *_retsignal, calc::StatementBlock *inclIn,
               const calc::WriteInfo& w,
                     calc::UsePar&   par );
	void statementSep(int *_retsignal);
	 calc::FieldExpr *  expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  xor_expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  and_expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  eq_expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  comp_expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  add_expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  mult_expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  pow_expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  sign_expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  not_expr(int *_retsignal, calc::StatementBlock *inclIn );
	 calc::FieldExpr *  misc_expr(int *_retsignal, calc::StatementBlock *inclIn );
	void modelLinkArgs(int *_retsignal, calc::StatementBlock *inclIn, std::string& strArg, calc::FieldExprArgs& args );
	void exprList(int *_retsignal, calc::StatementBlock *inclIn, calc::FieldExprArgs& args );
	 calc::ConstructPar   parWithIndeces(int *_retsignal, calc::StatementBlock *s );
	 calc::Symbol   arrayIndex(int *_retsignal);
	 calc::Symbol   qid(int *_retsignal);
	 calc::Symbol   number(int *_retsignal);
	 calc::Symbol   unsignedNumber(int *_retsignal);
	 calc::Symbol   sign(int *_retsignal);
};

#endif /* Parser_h */
