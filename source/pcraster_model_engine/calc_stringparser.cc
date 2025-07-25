#include "stddefx.h"
#include "calc_stringparser.h"
#include "calc_completeparser.h"
#include "calc_astnode.h"
#include "calc_astass.h"
#include "calc_astnodevector.h"
#include "calc_astnodelist.h"
#include "calc_aststat.h"
#include "calc_astscript.h"
#include "calc_code.h"

/*!
  \file
  This file contains the implementation of the StringParser class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC STRINGPARSER MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STRINGPARSER MEMBERS
//------------------------------------------------------------------------------

calc::StringParser::StringParser()
{
}

calc::StringParser::~StringParser()
{
}

calc::ASTNode* calc::StringParser::createExpr(const std::string& s)
{
  CompleteParser<ASTNode> sp(s);
  return sp.parse(&Parser::expr);
}

calc::ASTAss* calc::StringParser::createAssignment(const std::string& s)
{
  CompleteParser<ASTAss> sp(s);
  return sp.parse(&Parser::assignment);
}

calc::ASTNodeList* calc::StringParser::createStatementList(const std::string& s)
{
  CompleteParser<ASTNodeList> sp(s);
  return sp.parse(&Parser::statementList);
}

calc::ASTStat* calc::StringParser::createStatement(const std::string& s)
{
  CompleteParser<ASTStat> sp(s);
  return sp.parse(&Parser::statement);
}

calc::ASTScript* calc::StringParser::createScript(const std::string& str)
{
  CompleteParser<ASTScript> sp(str);
  return sp.parseScript();
}

//! as StringParser::createCodeAsNode() with an upcast to Code of the return
calc::Code * calc::StringParser::createCode(const std::string& str)
{
  ASTNode *n= createCodeAsNode(str);
  assert(dynamic_cast<Code *>(n));
  return dynamic_cast<Code *>(n);
}

//! parse a complete model, but only return the code (not the body)
/*! only the code means skipping binding,areamap and timer sections
 */
calc::ASTNode * calc::StringParser::createCodeAsNode(const std::string& str)
{
  std::unique_ptr<ASTScript> s(createScript(str));
  return s->astCode()->createClone();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


