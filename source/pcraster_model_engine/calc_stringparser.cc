#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_COMPLETEPARSER
#include "calc_completeparser.h"
#define INCLUDED_CALC_COMPLETEPARSER
#endif
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
#ifndef INCLUDED_CALC_CODE
#include "calc_code.h"
#define INCLUDED_CALC_CODE
#endif

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
  std::auto_ptr<ASTScript> s(createScript(str));
  return s->astCode()->createClone();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


