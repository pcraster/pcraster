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
#include <memory>

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
class Parser : public ANTLRParser {
public:
	static  const ANTLRChar *tokenName(int tk);
protected:
	static  ANTLRChar *_token_tbl[];
private:

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
	static SetWordType setwd1[67];
	static SetWordType setwd2[67];
	static SetWordType setwd3[67];
	static SetWordType setwd4[67];
	static SetWordType setwd5[67];
	static SetWordType ADD_GROUP_set[12];
	static SetWordType MULT_GROUP_set[12];
	static SetWordType setwd6[67];
	static SetWordType THEN_GROUP_set[12];
	static SetWordType ELSE_GROUP_set[12];
	static SetWordType setwd7[67];
	static SetWordType setwd8[67];
	static SetWordType setwd9[67];
	static SetWordType setwd10[67];
private:
	void zzdflthandlers( int _signal, int *_retsignal );

public:
	Parser(ANTLRTokenBuffer *input);
	 std::auto_ptr<calc::ASTScript>   model(int *_retsignal);
	void externalBindings(int *_retsignal, calc::ASTNodeVector& l );
	void bindingSection(int *_retsignal, calc::ASTScript *script );
	 Parser::AP_ASTNode   bindingDefinition(int *_retsignal);
	 Parser::AP_ASTNode   bindingRHS(int *_retsignal, const calc::ASTPar& par );
	void interfaceSection(int *_retsignal, calc::ASTScript *script );
	void areaMapSection(int *_retsignal, calc::ASTScript *script );
	void timer(int *_retsignal, calc::ASTScript *script );
	 Parser::AP_ASTNode   body(int *_retsignal);
	void reportMomentDef(int *_retsignal, calc::ASTScript *script );
	void reportMoments(int *_retsignal, std::vector<calc::ParsReportMoment> &m );
	void reportMoment(int *_retsignal, calc::ParsReportMoment& p );
	 int   reportMomentStep(int *_retsignal);
	  calc::IdList    idList(int *_retsignal);
	 Parser::AP_ASTNodeList   statementList(int *_retsignal);
	 Parser::AP_ASTStat   statement(int *_retsignal);
	 Parser::AP_ASTStat   foreach(int *_retsignal);
	 Parser::AP_ASTNode   repeat(int *_retsignal);
	 Parser::AP_ASTAss   assignment(int *_retsignal);
	 Parser::AP_ASTNode   assignmentTail(int *_retsignal, const calc::ASTPar& lhs, bool& swap );
	void statementSep(int *_retsignal);
	void endOfInput(int *_retsignal);
	 Parser::AP_ASTNode   expr(int *_retsignal);
	 Parser::AP_ASTNode   xor_expr(int *_retsignal);
	 Parser::AP_ASTNode   and_expr(int *_retsignal);
	 Parser::AP_ASTNode   eq_expr(int *_retsignal);
	 Parser::AP_ASTNode   comp_expr(int *_retsignal);
	 Parser::AP_ASTNode   add_expr(int *_retsignal);
	 Parser::AP_ASTNode   mult_expr(int *_retsignal);
	 Parser::AP_ASTNode   pow_expr(int *_retsignal);
	 Parser::AP_ASTNode   sign_expr(int *_retsignal);
	 Parser::AP_ASTNode   not_expr(int *_retsignal);
	 Parser::AP_ASTNode   misc_expr(int *_retsignal);
	 Parser::AP_ASTNodeVector   modelLinkArgs(int *_retsignal, std::string& strArg );
	 Parser::AP_ASTNodeVector   exprList(int *_retsignal);
	 calc::ASTPar   parWithIndeces(int *_retsignal);
	 calc::Id   arrayIndex(int *_retsignal);
	 calc::Id   qid(int *_retsignal);
	 calc::Id   number(int *_retsignal);
	 calc::Id   unsignedNumber(int *_retsignal);
	 calc::Id   sign(int *_retsignal);
	 Parser::AP_ASTId   nrOrPar(int *_retsignal);
	 calc::Id   nrOrId(int *_retsignal);
	static SetWordType ID_GROUP_set[12];
};

#endif /* Parser_h */
