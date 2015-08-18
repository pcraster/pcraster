#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_PARSERTEST
#include "calc_parsertest.h"
#define INCLUDED_CALC_PARSERTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_FILE
#include "com_file.h"     // com::write
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h" // FindValue
#define INCLUDED_COM_ALGORITHM
#endif
// Module headers.
#ifndef INCLUDED_CALC_COMPLETEPARSER
#include "calc_completeparser.h"
#define INCLUDED_CALC_COMPLETEPARSER
#endif
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif
#ifndef INCLUDED_CALC_ASTEXPR
#include "calc_astexpr.h"
#define INCLUDED_CALC_ASTEXPR
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
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
#ifndef INCLUDED_CALC_DYNAMICSECTION
#include "calc_dynamicsection.h"
#define INCLUDED_CALC_DYNAMICSECTION
#endif
#ifndef INCLUDED_CALC_REPEATUNTIL
#include "calc_repeatuntil.h"
#define INCLUDED_CALC_REPEATUNTIL
#endif
#ifndef INCLUDED_CALC_RUNSETTINGS
#include "calc_runsettings.h"
#define INCLUDED_CALC_RUNSETTINGS
#endif
#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif

#ifndef INCLUDED_CALC_MESSAGESTESTDB
#include "calc_messagestestdb.h"
#define INCLUDED_CALC_MESSAGESTESTDB
#endif

#ifndef INCLUDED_CALC_MODELERRORTESTER
#include "calc_modelerrortester.h"
#define INCLUDED_CALC_MODELERRORTESTER
#endif
#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif
#ifndef INCLUDED_CALC_ASTPATH
#include "calc_astpath.h"
#define INCLUDED_CALC_ASTPATH
#endif

/*!
  \file
  This file contains the implementation of the ParserTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PARSER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ParserTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ParserTest> instance(new ParserTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testExpr, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testAssignment, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testStatement, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testCheckAndRewriteParsedAST, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testStatementList, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testCode, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testModel, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testReportSection, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testBinding, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testExternalBindings, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testParseErrors, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testNonAsciiScript, instance));
  return suite;
}

static const calc::StringParser sp;

// parsing only will fail
#define MODEL_ERROR_TEST(x) MODEL_ERROR_TESTER(ModelErrorTester,x)

//------------------------------------------------------------------------------
// DEFINITION OF PARSER MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ParserTest::ParserTest()
{
}

std::string calc::ParserTest::model(const std::string& msgId) const
{
  return MessagesTestDB::instance()->model(msgId);
}


//! setUp
void calc::ParserTest::setUp()
{
}

//! tearDown
void calc::ParserTest::tearDown()
{
}


void calc::ParserTest::testExpr()
{
  typedef std::auto_ptr<ASTNode> E;

  {
  E e(sp.createExpr("a"));
  BOOST_CHECK(dynamic_cast<ASTPar *>(e.get()));
  }

  {
  E e(sp.createExpr("a*b"));
  BOOST_CHECK(dynamic_cast<ASTExpr *>(e.get()));
  }

  {
  E e(sp.createExpr("a and b"));
  BOOST_CHECK(dynamic_cast<ASTExpr *>(e.get()));
  }
  { // test ASTExpr::transferFunctionArgs()
       // rewrite  f(a0,a1,a2,a3,a4)
       //  as f(f(f(
       //           f(a0,a1),
       //           a2),
       //         a3),
       //       a4)
    E eAutoPtr(sp.createExpr("min(0,1,2,3,4)"));
    ASTExpr *e=dynamic_cast<ASTExpr *>(eAutoPtr.get());

    for(size_t a=0; a < 4; ++a) {
      BOOST_CHECK(e);
      BOOST_CHECK(e->nrArgs()==2);
      ASTNumber *n(dynamic_cast<ASTNumber *>(e->arg(1)));
      BOOST_CHECK(n);

      BOOST_CHECK(n->value()==4-a);
      ASTExpr *nextE=dynamic_cast<ASTExpr *>(e->arg(0));
      if (a==3) { // last most inner
       BOOST_CHECK(!nextE);
       ASTNumber *n(dynamic_cast<ASTNumber *>(e->arg(0)));
       BOOST_CHECK(n->value()==0);
      }
      e=nextE;
    }
  }
}

void calc::ParserTest::testAssignment()
{
  typedef std::auto_ptr<ASTAss> A;
  {
   A a(sp.createAssignment("a=3*5;"));
   BOOST_CHECK(a->nrPars()==1);
   BOOST_CHECK(a->par(0)->name()=="a");
  }

  {
  // eof is terminator
  A a(sp.createAssignment("a=3*5"));
  BOOST_CHECK(a->nrPars()==1);
  }

  {
  A a(sp.createAssignment("p1,p2,p3=abs()"));
  BOOST_CHECK(a->nrPars()==3);
  }

  {
  A a(sp.createAssignment("k=a and b"));
  BOOST_CHECK(a->nrPars()==1);
  }

}

void calc::ParserTest::testStatementList()
{
 {
  typedef std::auto_ptr<ASTNodeList> S;
  S s(sp.createStatementList(model("pcrcalc11pre")));
  BOOST_CHECK(s->size()==2);
 }
 { // repeat
  typedef std::auto_ptr<ASTNodeList> S;
  S s(sp.createStatementList(model("pcrcalc379")));
  BOOST_REQUIRE_EQUAL(s->size(), 2U);
  ASTNodeList::const_iterator n(s->begin());
  n++;
  BOOST_CHECK(dynamic_cast<RepeatUntil *>(*n));
 }
}

void calc::ParserTest::testCode()
{
 { // only a dynamic section
  typedef std::auto_ptr<ASTNode> B;
  B l(sp.createCodeAsNode(model("pcrcalc8a")));
  DynamicSection *d(astCast<DynamicSection>(l.get(),"C/b/0"));
  BOOST_REQUIRE(d);
  ASTNodeList    *s(dynamic_cast<ASTNodeList *>(d->statements()));
  BOOST_CHECK_EQUAL(s->size(), 2U);
 }
 { // initial plus dynamic
  typedef std::auto_ptr<ASTNode> B;
  B b(sp.createCodeAsNode(model("pcrcalc8ab")));
  ASTNodeList *l(astCast<ASTNodeList>(b.get(),"C/b"));
  BOOST_REQUIRE(l);

  BOOST_CHECK_EQUAL(l->size(), 2U);
  ASTNodeList::const_iterator n(l->begin());
  if (n != l->end())
    BOOST_CHECK(dynamic_cast<ASTNodeList *>(*n)); // initial section
  n++;
  if (n != l->end())
    BOOST_CHECK(dynamic_cast<DynamicSection *>(*n));
 }
}

void calc::ParserTest::testStatement()
{
 { // fileoutput
  MODEL_ERROR_TEST(pcrcalc9);
/*
 * typedef std::auto_ptr<ASTStat> S;
 * S s(sp.createStatement(model("pcrcalc9")));
 * BOOST_CHECK(dynamic_cast<ASTExpr *>(s->stat()));
 * BOOST_CHECK(!s->reportParsed());
 * BOOST_CHECK( s->reportById().empty());
 * BOOST_CHECK(!s->reportInSitu());
 */
 }
 { // a report clause
  typedef std::auto_ptr<ASTStat> S;
  S s(sp.createStatement(model("pcrcalc301b")));
  BOOST_CHECK(dynamic_cast<ASTAss *>(s->stat()));
  BOOST_CHECK( s->reportParsed());
  BOOST_CHECK( s->reportById().empty());
  BOOST_CHECK(!s->reportInSitu());
 }
 { // a report clause with id
  typedef std::auto_ptr<ASTStat> S;
  S s(sp.createStatement(
   "report(rep2) tmp.res= if(inp1b.map, ldd(5));"));
  BOOST_CHECK(dynamic_cast<ASTAss *>(s->stat()));
  BOOST_CHECK( s->reportParsed());
  BOOST_CHECK(s->reportById().name()=="rep2");
  BOOST_CHECK(!s->reportInSitu());
 }
 { // a report clause with insitu report
  typedef std::auto_ptr<ASTStat> S;
  S s(sp.createStatement(
   "report(1,3,5,10) tmp.res= if(inp1b.map, ldd(5));"));
  BOOST_CHECK(dynamic_cast<ASTAss *>(s->stat()));
  BOOST_CHECK(s->reportParsed());
  BOOST_CHECK(s->reportById().empty());
  BOOST_CHECK(s->reportInSitu());
 }
}

void calc::ParserTest::testCheckAndRewriteParsedAST()
{
 { // implicit report on timeoutput fixed in checkAndRewriteParsedAST()
   typedef std::auto_ptr<ASTStat> S;
   S s(sp.createStatement("s = timeoutput(inp1b.map,1);"));
   BOOST_CHECK(s->reportParsed());
 }
 MODEL_ERROR_TEST(pcrcalc37);
}

void calc::ParserTest::testParseErrors()
{
  ASTNode *e;
  bool     catched;

  catched=false;
  try {
   e = sp.createExpr("ab*");
  } catch(const PosException& ) {
    catched=true;
  }
  BOOST_CHECK(catched);

  catched=false;
  try {
   e = sp.createExpr("a&b");
  } catch(const PosException& ) {
    catched=true;
  }
  BOOST_CHECK(catched);

  catched=false;
  try {
    e = sp.createExpr("and b");
  } catch(const PosException& ) {
    catched=true;
  }

  catched=false;
  try {
   // test that StringParser checks on more input
   // then neeeded for parsing pcrcalc11pre is list of statements
   // beginning with an assignment
   (void)sp.createAssignment(model("pcrcalc11pre"));
  } catch(const PosException& ) {
    catched=true;
  }
  BOOST_CHECK(catched);

 MODEL_ERROR_TEST(pcrcalc514);
 MODEL_ERROR_TEST(pcrcalc520);
 {
     com::PathName empty("empty.mod");
     com::write("",empty);
     bool catched=false;
     try {
      CompleteParser<ASTScript,com::PathName> cp(empty);
      std::auto_ptr<ASTScript> s(cp.parseScript());
     } catch(const com::Exception& e) {
BOOST_CHECK(e.messages().find("script contains no code") != std::string::npos);
       catched=true;
     }
     BOOST_CHECK(catched);
 }
}

void calc::ParserTest::testModel()
{
 struct M {
   bool cfgCode;
   M(std::string const& code):
     cfgCode(false)
    {
      std::auto_ptr<ASTScript> s(StringParser::createScript(code));
      cfgCode=s->cfgCode()!=0;
    }
 };

 {
  M m(model("pcrcalc256"));
  BOOST_CHECK(m.cfgCode);
 }
 {
  M m(model("pcrcalc0"));
  BOOST_CHECK(m.cfgCode);
 }
 { // simple statement list
   M m("p=1+0;p=p+2;");
   BOOST_CHECK(m.cfgCode);
 }
 MODEL_ERROR_TEST(pcrcalc510);
 MODEL_ERROR_TEST(pcrcalc511);
 MODEL_ERROR_TEST(pcrcalc512);
 MODEL_ERROR_TEST(pcrcalc515);
 MODEL_ERROR_TEST(pcrcalc516);
 {
  M m(model("pcrcalc509"));
  BOOST_CHECK(m.cfgCode);
 }
 MODEL_ERROR_TEST(pcrcalc555);
}

void calc::ParserTest::testBinding()
{
 MODEL_ERROR_TEST(pcrcalc41);
 MODEL_ERROR_TEST(pcrcalc42);
}

void calc::ParserTest::testReportSection()
{
 MODEL_ERROR_TEST(pcrcalc238);
 MODEL_ERROR_TEST(pcrcalc239);
}

void calc::ParserTest::testExternalBindings()
{
 // SYNTAX ERROR
 bool catched(false);
 try {
    com::PathName pn("testAddBindings.txt");
    com::write("jan=3; #comment\njan=cees + 4\n",
                "testAddBindings.txt");
    RunSettings rs(pn);
 } catch (calc::PosException e) {
  catched=true;
 }
 BOOST_CHECK(catched);

 // CORRECT
{
  com::PathName pn("testAddBindings.txt");
  com::write("jan=3.5; #comment\njan=\"xx file.txt\"\nn=4",
              "testAddBindings.txt");
  RunSettings rs(pn);

  // jan = 3.5 overwritten by jan = xx file.txt

  BOOST_CHECK(rs.size()==2); // 2 out of 3 bindings kept

  {
  ASTAss *a= dynamic_cast<ASTAss *>(rs[0]);
  BOOST_CHECK(a);
  BOOST_CHECK(a->par()->name()=="jan");

  ASTId *v= dynamic_cast<ASTId *>(a->rhs());
  BOOST_CHECK(v);
  BOOST_CHECK(v->name()=="xx file.txt");
  }

  {
  ASTAss *a= dynamic_cast<ASTAss *>(rs[1]);
  BOOST_CHECK(a);
  BOOST_CHECK(a->par()->name()=="n");

  ASTNumber *v= dynamic_cast<ASTNumber *>(a->rhs());
  BOOST_CHECK(v);
  BOOST_CHECK(v->value()==4);
  }

}
}

void calc::ParserTest::testNonAsciiScript()
{
  {
    // cyrilic chars in comments
    // comments are stripped so that is ok
    com::PathName pn("okaflow.mod");
    CompleteParser<ASTScript,com::PathName> cp(pn);

    bool catched=false;
    // check that exception is not an unsupported char
    try {
     std::auto_ptr<ASTScript> s(cp.parseScript());
     s->analyzeAndResolve();
    } catch (const com::Exception& e) {
      BOOST_CHECK(e.messages().find("cropf_cover.map") != std::string::npos);
      catched=true;
    }
    BOOST_CHECK(catched);
  }

  {
    // other char in expression (strange minus char)

    bool todoCharSet=false;
    // check that exception is not an unsupported char
    try {
     com::PathName pn("charSetProblem.mod");
     CompleteParser<ASTScript,com::PathName> cp(pn);
     std::auto_ptr<ASTScript> s(cp.parseScript());
     s->analyzeAndResolve();
    } catch (const com::Exception& ) {
      todoCharSet=false; // STILL TO SOLVE!
    }
    BOOST_WARN(todoCharSet);
  }
}
