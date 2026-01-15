#define BOOST_TEST_MODULE pcraster model_engine parser
#include <boost/test/unit_test.hpp>
#include <string>
#include "com_file.h"       // com::write
#include "com_algorithm.h"  // FindValue
#include "calc_completeparser.h"
#include "calc_astscript.h"
#include "calc_astnode.h"
#include "calc_astexpr.h"
#include "calc_astpar.h"
#include "calc_astass.h"
#include "calc_aststat.h"
#include "calc_astnumber.h"
#include "calc_astnodelist.h"
#include "calc_dynamicsection.h"
#include "calc_repeatuntil.h"
#include "calc_runsettings.h"
#include "calc_stringparser.h"
#include "calc_messagestestdb.h"
#include "calc_modelerrortester.h"
#include "calc_posexception.h"
#include "calc_astpath.h"


static const calc::StringParser sp;

// parsing only will fail
#define MODEL_ERROR_TEST(x) MODEL_ERROR_TESTER(calc::ModelErrorTester, x)

namespace parsertest
{
std::string model(const std::string &msgId)
{
  return calc::MessagesTestDB::instance()->model(msgId);
}
}  // namespace parsertest

BOOST_AUTO_TEST_CASE(testExpr)
{
  using namespace calc;

  typedef std::unique_ptr<ASTNode> E;

  {
    E const e(sp.createExpr("a"));
    BOOST_TEST(dynamic_cast<ASTPar *>(e.get()));
  }

  {
    E const e(sp.createExpr("a*b"));
    BOOST_TEST(dynamic_cast<ASTExpr *>(e.get()));
  }

  {
    E const e(sp.createExpr("a and b"));
    BOOST_TEST(dynamic_cast<ASTExpr *>(e.get()));
  }
  {  // test ASTExpr::transferFunctionArgs()
     // rewrite  f(a0,a1,a2,a3,a4)
     //  as f(f(f(
     //           f(a0,a1),
     //           a2),
     //         a3),
     //       a4)
    E const eAutoPtr(sp.createExpr("min(0,1,2,3,4)"));
    auto *e = dynamic_cast<ASTExpr *>(eAutoPtr.get());

    for (size_t a = 0; a < 4; ++a) {
      BOOST_TEST(e);
      BOOST_TEST(e->nrArgs() == 2);
      auto *n(dynamic_cast<ASTNumber *>(e->arg(1)));
      BOOST_TEST(n);

      BOOST_TEST(n->value() == 4 - a);
      auto *nextE = dynamic_cast<ASTExpr *>(e->arg(0));
      if (a == 3) {  // last most inner
        BOOST_TEST(!nextE);
        auto *n(dynamic_cast<ASTNumber *>(e->arg(0)));
        BOOST_TEST(n->value() == 0);
      }
      e = nextE;
    }
  }
}

BOOST_AUTO_TEST_CASE(testAssignment)
{
  using namespace calc;

  typedef std::unique_ptr<ASTAss> A;
  {
    A a(sp.createAssignment("a=3*5;"));
    BOOST_TEST(a->nrPars() == 1);
    BOOST_TEST(a->par(0)->name() == "a");
  }

  {
    // eof is terminator
    A a(sp.createAssignment("a=3*5"));
    BOOST_TEST(a->nrPars() == 1);
  }

  {
    A a(sp.createAssignment("p1,p2,p3=abs()"));
    BOOST_TEST(a->nrPars() == 3);
  }

  {
    A a(sp.createAssignment("k=a and b"));
    BOOST_TEST(a->nrPars() == 1);
  }
}

BOOST_AUTO_TEST_CASE(testStatementList)
{
  using namespace calc;

  {
    typedef std::unique_ptr<ASTNodeList> S;
    S s(sp.createStatementList(parsertest::model("pcrcalc11pre")));
    BOOST_TEST(s->size() == 2);
  }
  {  // repeat
    typedef std::unique_ptr<ASTNodeList> S;
    S s(sp.createStatementList(parsertest::model("pcrcalc379")));
    BOOST_REQUIRE_EQUAL(s->size(), 2U);
    ASTNodeList::const_iterator n(s->begin());
    n++;
    BOOST_TEST(dynamic_cast<RepeatUntil *>(*n));
  }
}

BOOST_AUTO_TEST_CASE(testCode)
{
  using namespace calc;

  {  // only a dynamic section
    typedef std::unique_ptr<ASTNode> B;
    B const l(sp.createCodeAsNode(parsertest::model("pcrcalc8a")));
    auto *d(astCast<DynamicSection>(l.get(), "C/b/0"));
    BOOST_TEST_REQUIRE(d);
    auto *s(dynamic_cast<ASTNodeList *>(d->statements()));
    BOOST_CHECK_EQUAL(s->size(), 2U);
  }
  {  // initial plus dynamic
    typedef std::unique_ptr<ASTNode> B;
    B const b(sp.createCodeAsNode(parsertest::model("pcrcalc8ab")));
    auto *l(astCast<ASTNodeList>(b.get(), "C/b"));
    BOOST_TEST_REQUIRE(l);

    BOOST_CHECK_EQUAL(l->size(), 2U);
    ASTNodeList::const_iterator n(l->begin());
    if (n != l->end()) {
      BOOST_TEST(dynamic_cast<ASTNodeList *>(*n));  // initial section
    }
    n++;
    if (n != l->end()) {
      BOOST_TEST(dynamic_cast<DynamicSection *>(*n));
    }
  }
}

BOOST_AUTO_TEST_CASE(testStatement)
{
  using namespace calc;

  {  // fileoutput
    MODEL_ERROR_TEST(pcrcalc9);
    /*
 * typedef std::auto_ptr<ASTStat> S;
 * S s(sp.createStatement(model("pcrcalc9")));
 * BOOST_TEST(dynamic_cast<ASTExpr *>(s->stat()));
 * BOOST_TEST(!s->reportParsed());
 * BOOST_TEST( s->reportById().empty());
 * BOOST_TEST(!s->reportInSitu());
 */
  }
  {  // a report clause
    typedef std::unique_ptr<ASTStat> S;
    S s(sp.createStatement(parsertest::model("pcrcalc301b")));
    BOOST_TEST(dynamic_cast<ASTAss *>(s->stat()));
    BOOST_TEST(s->reportParsed());
    BOOST_TEST(s->reportById().empty());
    BOOST_TEST(!s->reportInSitu());
  }
  {  // a report clause with id
    typedef std::unique_ptr<ASTStat> S;
    S s(sp.createStatement("report(rep2) tmp.res= if(inp1b.map, ldd(5));"));
    BOOST_TEST(dynamic_cast<ASTAss *>(s->stat()));
    BOOST_TEST(s->reportParsed());
    BOOST_TEST(s->reportById().name() == "rep2");
    BOOST_TEST(!s->reportInSitu());
  }
  {  // a report clause with insitu report
    typedef std::unique_ptr<ASTStat> S;
    S s(sp.createStatement("report(1,3,5,10) tmp.res= if(inp1b.map, ldd(5));"));
    BOOST_TEST(dynamic_cast<ASTAss *>(s->stat()));
    BOOST_TEST(s->reportParsed());
    BOOST_TEST(s->reportById().empty());
    BOOST_TEST(s->reportInSitu());
  }
}

BOOST_AUTO_TEST_CASE(testCheckAndRewriteParsedAST)
{
  using namespace calc;

  {  // implicit report on timeoutput fixed in checkAndRewriteParsedAST()
    typedef std::unique_ptr<ASTStat> S;
    S s(sp.createStatement("s = timeoutput(inp1b.map,1);"));
    BOOST_TEST(s->reportParsed());
  }
  MODEL_ERROR_TEST(pcrcalc37);
}

BOOST_AUTO_TEST_CASE(testParseErrors)
{
  using namespace calc;

  ASTNode *e = nullptr;
  bool catched = false;

  catched = false;
  try {
    e = sp.createExpr("ab*");
    (void)e;  // shut up compiler
  } catch (const PosException &) {
    catched = true;
  }
  BOOST_TEST(catched);

  catched = false;
  try {
    e = sp.createExpr("a&b");
  } catch (const PosException &) {
    catched = true;
  }
  BOOST_TEST(catched);

  catched = false;
  try {
    e = sp.createExpr("and b");
  } catch (const PosException &) {
    catched = true;
  }

  catched = false;
  try {
    // test that StringParser checks on more input
    // then neeeded for parsing pcrcalc11pre is list of statements
    // beginning with an assignment
    (void)sp.createAssignment(parsertest::model("pcrcalc11pre"));
  } catch (const PosException &) {
    catched = true;
  }
  BOOST_TEST(catched);

  MODEL_ERROR_TEST(pcrcalc514);
  MODEL_ERROR_TEST(pcrcalc520);
  {
    com::PathName const empty("empty.mod");
    com::write("", empty);
    bool catched = false;
    try {
      CompleteParser<ASTScript, com::PathName> cp(empty);
      std::unique_ptr<ASTScript> const s(cp.parseScript());
    } catch (const com::Exception &e) {
      BOOST_TEST(e.messages().find("script contains no code") != std::string::npos);
      catched = true;
    }
    BOOST_TEST(catched);
  }
}

BOOST_AUTO_TEST_CASE(testModel)
{
  using namespace calc;

  struct M {
    bool cfgCode{false};

    M(std::string const &code)
    {
      std::unique_ptr<ASTScript> s(StringParser::createScript(code));
      cfgCode = s->cfgCode() != nullptr;
    }
  };

  {
    M const m(parsertest::model("pcrcalc256"));
    BOOST_TEST(m.cfgCode);
  }
  {
    M const m(parsertest::model("pcrcalc0"));
    BOOST_TEST(m.cfgCode);
  }
  {  // simple statement list
    M const m("p=1+0;p=p+2;");
    BOOST_TEST(m.cfgCode);
  }
  MODEL_ERROR_TEST(pcrcalc510);
  MODEL_ERROR_TEST(pcrcalc511);
  MODEL_ERROR_TEST(pcrcalc512);
  MODEL_ERROR_TEST(pcrcalc515);
  MODEL_ERROR_TEST(pcrcalc516);
  {
    M const m(parsertest::model("pcrcalc509"));
    BOOST_TEST(m.cfgCode);
  }
  MODEL_ERROR_TEST(pcrcalc555);
}

BOOST_AUTO_TEST_CASE(testBinding)
{
  MODEL_ERROR_TEST(pcrcalc41);
  MODEL_ERROR_TEST(pcrcalc42);
}

BOOST_AUTO_TEST_CASE(testReportSection)
{
  MODEL_ERROR_TEST(pcrcalc238);
  MODEL_ERROR_TEST(pcrcalc239);
}

BOOST_AUTO_TEST_CASE(testExternalBindings)
{
  using namespace calc;

  // SYNTAX ERROR
  bool catched(false);
  try {
    com::PathName const pn("testAddBindings.txt");
    com::write("jan=3; #comment\njan=cees + 4\n", "testAddBindings.txt");
    RunSettings const rs(pn);
  } catch (calc::PosException &) {
    catched = true;
  }
  BOOST_TEST(catched);

  // CORRECT
  {
    com::PathName const pn("testAddBindings.txt");
    com::write("jan=3.5; #comment\njan=\"xx file.txt\"\nn=4", "testAddBindings.txt");
    RunSettings const rs(pn);

    // jan = 3.5 overwritten by jan = xx file.txt

    BOOST_TEST(rs.size() == 2);  // 2 out of 3 bindings kept

    {
      auto *a = dynamic_cast<ASTAss *>(rs[0]);
      BOOST_TEST(a);
      BOOST_TEST(a->par()->name() == "jan");

      auto *v = dynamic_cast<ASTId *>(a->rhs());
      BOOST_TEST(v);
      BOOST_TEST(v->name() == "xx file.txt");
    }

    {
      auto *a = dynamic_cast<ASTAss *>(rs[1]);
      BOOST_TEST(a);
      BOOST_TEST(a->par()->name() == "n");

      auto *v = dynamic_cast<ASTNumber *>(a->rhs());
      BOOST_TEST(v);
      BOOST_TEST(v->value() == 4);
    }
  }
}

BOOST_AUTO_TEST_CASE(testNonAsciiScript)
{
  using namespace calc;

  {
    // cyrilic chars in comments
    // comments are stripped so that is ok
    com::PathName const pn("okaflow.mod");
    CompleteParser<ASTScript, com::PathName> cp(pn);

    bool catched = false;
    // check that exception is not an unsupported char
    try {
      std::unique_ptr<ASTScript> s(cp.parseScript());
      s->analyzeAndResolve();
    } catch (const com::Exception &e) {
      BOOST_TEST(e.messages().find("cropf_cover.map") != std::string::npos);
      catched = true;
    }
    BOOST_TEST(catched);
  }

  {
    // other char in expression (strange minus char)

    bool todoCharSet = false;
    // check that exception is not an unsupported char
    try {
      com::PathName const pn("charSetProblem.mod");
      CompleteParser<ASTScript, com::PathName> cp(pn);
      std::unique_ptr<ASTScript> s(cp.parseScript());
      s->analyzeAndResolve();
    } catch (const com::Exception &) {
      todoCharSet = false;  // STILL TO SOLVE!
    }
    BOOST_TEST_WARN(todoCharSet);
  }
}
