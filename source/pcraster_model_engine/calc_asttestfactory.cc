#include "stddefx.h"
#include "calc_asttestfactory.h"
#include "com_file.h"
#include "com_exception.h"
#include "pcrxml_dom.h"
#include "pcrxml_document.h"
#include "com_strlib.h"
#include "calc_astscript.h"
#include "calc_options.h"
#include "calc_astpar.h"
#include "calc_astnumber.h"
#include "calc_astnodevector.h"
#include "calc_astexpr.h"
#include "calc_nonassexpr.h"
#include "calc_astass.h"
#include "calc_positionname.h"
#include "calc_findsymbol.h" // opName2op
#include "calc_operator.h"
#include "calc_executor.h"
#include "calc_stringparser.h"
#include "calc_messagestestdb.h"
#include "calc_globallibdefs.h"

#include <QDomElement>
#include <memory>

/*!
  \file
  This file contains the implementation of the ASTTestFactory class.
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTTESTFACTORY MEMBERS
//------------------------------------------------------------------------------
calc::MessagesTestDB& calc::ASTTestFactory::db()
{
  return *(MessagesTestDB::instance());
}

void calc::execTest(const std::string& id)
{
  globalInit(); // testing only

  std::unique_ptr<ASTScript> as(ASTTestFactory::createFromIdOrStr(id));
  as->analyzeAndResolve();

  Executor e(as->cfgCode(),as->rteSettings(),as->symbols());
  e.execAll();
}

//! tool to verify generated ascii output (for example a tss) stored in messagestest.xml
/*!
 * \param fName this file is here removed, the stuff to test must recreate it.
 */
calc::TestAsciiResult::TestAsciiResult(const std::string& fName):
    d_fName(fName)
{
    com::remove(fName);
}

bool calc::TestAsciiResult::equals(const std::string& id)
{
    return fileVerify(id,d_fName);
}

//------------------------------------------------------------------------------
// DEFINITION OF ASTTESTFACTORY MEMBERS
//------------------------------------------------------------------------------

calc::ASTTestFactory::ASTTestFactory()
{
}

calc::ASTTestFactory::~ASTTestFactory()
{
}

//! create an expr node with no arguments
/*!
 * note that the unary - and unary +, have a special syntax: u-
 * and u+
 */
calc::ASTExpr* calc::ASTTestFactory::createExpr(
    const std::string& opName)
{
  size_t nrDefaultArgs=2;
  std::string opN(opName);
  if (opN == "u-" || opN == "u+") {
    // strip u, set args
    opN = opN[1]; nrDefaultArgs=1;
  }
  const Operator* op(opName2op(opN,nrDefaultArgs));
  if (!op) {
    if (opName=="ifthenelse")
      op=major2op(OP_IFTHENELSE);
    if (opName=="ifthen")
      op=major2op(OP_IFTHEN);
    if (!op)
     throw com::BadStreamFormat("createExpr illegal op: "+opName);
  }
  auto *e= new ASTExpr(opN,*op);
  return e;
}

calc::ASTPar* calc::ASTTestFactory::createPar(
    const std::string& name)
{
  return new ASTPar(name);
}

calc::ASTNumber* calc::ASTTestFactory::createNumber(
    const std::string& numericString)
{
  return new ASTNumber(numericString);
}

//! create assignment
calc::ASTAss* calc::ASTTestFactory::createAss(
    const std::string& parName,
    ASTNode  *value,
    const Position *pos)
{
  auto *a= new ASTAss();
  ASTPar p(parName);
  p.setPosition(pos);
  a->addPar(p);
  a->transferRhs(value);
  return a;
}

calc::ASTNode* calc::ASTTestFactory::createCode(
  const char *xmlCode)
{
  pcrxml::Document doc(xmlCode);
  return createCode(doc.documentElement());
}

namespace calc {
static std::string optAttr(
    const QDomElement& e,
    const char *attrName,
    const char *valueOnAbsence)
{
    return std::string(e.attribute(attrName,valueOnAbsence).toLatin1());
}

}

calc::ASTNode* calc::ASTTestFactory::createCode(
   const QDomElement& e)
{
  ASTNode *n(nullptr);
  std::string v(std::string(e.attribute("v").toLatin1()));
  std::string pos(optAttr(e,"p","0"));

  PositionName pn(pos);

  std::string name(e.tagName().toLatin1());
  PRECOND(name.size()==1);
  switch(name[0]) {
    case 'n': n=createNumber(v);
              break;
    case 'p': n=createPar(v);
              break;
    case 'a': {
              // v is par
              std::vector<QDomElement> c(pcrxml::childElements(e));
              POSTCOND(c.size()==1);
              n = createAss(v, createCode(c[0]),&pn);
              } break;
    case 'l': {
              auto *l = new ASTNodeVector();
              std::vector<QDomElement> c(pcrxml::childElements(e));
              for(auto & i : c)
                l->transferPushBack(createCode(i));
              n=l;
              } break;
    case 'e': {
       ASTExpr *expr=createExpr(v);
       // args can in a args
       std::string a(optAttr(e,"a",""));
       com::removeAllSpace(a);
       std::vector<std::string> args(com::split(a,','));
       for(auto & arg : args) {
         if (com::isDouble(arg))
           expr->transferArg(createNumber(arg));
         else
           expr->transferArg(createPar(arg));
       }
       // rest of arguments as elements
       std::vector<QDomElement> c(pcrxml::childElements(e));
       for(auto & i : c)
         expr->transferArg(createCode(i));
       n=expr;
       break;
    }
    default : PRECOND(FALSE);
  }
  n->setPosition(&pn);
  return n;
}

bool calc::ASTTestFactory::msgVerify(
    const std::string& id,
    const com::Exception& e,
    const std::string& prefix)// const
{
  return db().equals(id,e,prefix);
}

bool calc::ASTTestFactory::fileVerify(
    const std::string& id,
    const std::string& createdFile)// const
{
  return db().equalsFile(id,createdFile);
}

calc::ASTNode* calc::ASTTestFactory::createFromId(
    const std::string& id)
{
  if (db().hasXML(id)) {
    return createCode(db().xml(id));
  }
  return StringParser::createCodeAsNode(modelFromId(id));
}

calc::ASTNode* calc::ASTTestFactory::createFromId(
    const char* id)
{
  return createFromId(std::string(id));
}

std::string calc::ASTTestFactory::modelFromId(
    const std::string& id)
{
  return db().model(id);
}

calc::ASTScript* calc::ASTTestFactory::createFromIdOrStr(
    const std::string& codeOrId)
{

 Options ops; // will reset global options
 std::string options=db().options(codeOrId);
 if (!options.empty())
   ops.processOptionString(options);

 std::unique_ptr<ASTScript> as;

 if (codeOrId.find('=') != std::string::npos) {
   as.reset(StringParser::createScript(codeOrId));
 } else {
  // absence of "=" symbol means a single expr or an id
  // FTBB since ASTScript can not be a single expression
  if (codeOrId.find("pcrcalc") == 0)
     as.reset(StringParser::createScript(modelFromId(codeOrId)));
  else {
     as = std::make_unique<ASTScript>();
     ASTNode *n=StringParser::createExpr(codeOrId);
     as->transferCode(new NonAssExpr(n));
  }
 }
 POSTCOND(as.get());

 as->setRteSettings(ops);

 return as.release();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



