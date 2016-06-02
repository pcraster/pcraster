#define BOOST_TEST_MODULE pcraster model_engine usedefanalyzer
#include <boost/test/unit_test.hpp>
#include <iostream>
#include "calc_astpath.h"
#include "calc_astcfgtester.h"
#include "calc_stringparser.h"
#include "calc_usedefanalyzer.h"
#include "calc_parset.h"
#include "calc_astpar.h"
#include "calc_jumpnode.h"
#include "calc_code.h"
#include "calc_iotype.h"

// NOTE use string failureExpected in files expected to fail, see style guide


namespace calc {
  namespace useDefAnalyzerTest {
   class Test {
     ASTCFGTester                  d_a;
     ParSet                        d_input;

     std::string ass(size_t assNr) const {
       std::ostringstream s;
       s << "C/b/l/"<<assNr<<"/a/";
       return s.str();
     }

     bool checkLastUse(const std::string& path, bool lastUse)const {
       bool ok=astCast<ASTPar>(d_a.ast(),path)->lastUse() == lastUse;
       if (!ok) {
         // make sure we do the correct path
         std::cerr << "lastUse failure path: " <<
                      path << " script pos: " <<
                      astCast<ASTPar>(d_a.ast(),path)->shortPosText();
       }
       return ok;
     }


   public:
     typedef std::set<std::string> NameSet;

     Test(const char *code):
       d_a(StringParser::createCodeAsNode(code))
     {
      setLastUse(d_a.cfg());
      d_input = inputSet(d_a.cfg());
     }
     bool input(const NameSet& input) {
       if (!(d_input == input)) {
         std::cerr << "Input mismatch got :" << d_input << std::endl;
         std::cerr << "          expected :"; printSet(std::cerr,input);
         return false;
       }
       return true;
     }
     //! lhs par in simple assignments
     bool lhs(size_t assNr,bool lastUse) const
     {
       return checkLastUse(ass(assNr)+"<",lastUse);
     }
     //! rhs par in simple assignments
     bool rhs(size_t assNr,bool lastUse, size_t argIndex=0) const
     {
       std::ostringstream arg;
       arg << argIndex;
       return checkLastUse(ass(assNr)+">/e/,/"+arg.str()+"/p",lastUse);
     }
     bool par(const char* path,bool lastUse) const {
       return checkLastUse(path,lastUse);
     }
     template <typename T>
       T* cast(const char *path) const {
         return astCast<T>(d_a.ast(),path);
     }
     JumpNode *jump(const char* path) const {
       return cast<JumpNode>(path);
     }

     static void printSet(std::ostream& str, const NameSet& s) {
       str << "(";
       std::copy(s.begin(),s.end(),std::ostream_iterator<std::string>(str,","));
       str << ")" << std::endl;
     }
  };
 }
}

#define UDA_TEST(C)  useDefAnalyzerTest::Test C


BOOST_AUTO_TEST_CASE(testLinear)
{
  using namespace calc;

  // use lookupscalar: unlimited args and no
  // rewriting in CFG as is the case with min/max/cover
  {
   UDA_TEST(test("a=lookupscalar(b);"));
   BOOST_CHECK(test.lhs(0, true));
   JumpNode *j= test.jump("C/}");
   BOOST_CHECK(j);
   BOOST_CHECK(j->deletesOnForward().empty());
   BOOST_CHECK(test.rhs(0, true));
  }
  {
   UDA_TEST(test("a=lookupscalar(b);a=lookupscalar(b)"));
   BOOST_CHECK(test.lhs(0, true));
   BOOST_CHECK(test.rhs(0, false));

   BOOST_CHECK(test.lhs(1, true));
   BOOST_CHECK(test.rhs(1, true));
  }
  {
   UDA_TEST(test("a=lookupscalar(b,c,b);"));
   BOOST_CHECK(test.lhs(0, true));
   BOOST_CHECK(test.rhs(0, true,0));
   BOOST_CHECK(test.rhs(0, true, 1));
   BOOST_CHECK(test.rhs(0, false, 2));

  }
  {
   UDA_TEST(test("a=lookupscalar(b,c,b);"));
   BOOST_CHECK(test.lhs(0, true));
   BOOST_CHECK(test.rhs(0, true,0));
   BOOST_CHECK(test.rhs(0, true, 1));
   BOOST_CHECK(test.rhs(0, false, 2));
  }
  {
   UDA_TEST(test("a=lookupscalar(b,c,b);a=lookupscalar(b)"));
   BOOST_CHECK(test.lhs(0, true));
   BOOST_CHECK(test.rhs(0, false,0));
   BOOST_CHECK(test.rhs(0, true, 1));
   BOOST_CHECK(test.rhs(0, false,2));

   BOOST_CHECK(test.lhs(0, true));
   BOOST_CHECK(test.rhs(1, true));
  }
  {
   UDA_TEST(test("a=lookupscalar(b,c,b);a=lookupscalar(a)"));
   BOOST_CHECK(test.lhs(0, false));
   BOOST_CHECK(test.rhs(0, true,0));
   BOOST_CHECK(test.rhs(0, true, 1));
   BOOST_CHECK(test.rhs(0, false,2));

   BOOST_CHECK(test.lhs(1, true));
   BOOST_CHECK(test.rhs(1, true));
  }
}

BOOST_AUTO_TEST_CASE(testLoops)
{
  using namespace calc;

  { // use of a AFTER repeat
   const char *code=
        "               # test path to assignments\n" \
        "               # a b                     \n" \
        "a=0;           # 1    l/0/a              \n" \
        "repeat {       #      l/1/r              \n" \
        "b=a+1;         # 2 1  l/1/r/b/0/a        \n" \
        "a=b*2;         # 3 2  l/1/r/b/1/a        \n" \
        "} until (3);   #      l/1/r/c            \n" \
        "b=a+4;         # 4 3  l/2                \n" \
        "               #       b:l/2/a/<         \n" \
        "               #       a:l/2/a/>/,/0/p   \n";
   UDA_TEST(test(code));

   // a 1
   BOOST_CHECK(test.par("C/b/0/a/</p",         false));
   // a 2
   BOOST_CHECK(test.par("C/b/1/b/0/a/>/,/0/p", true));
   // a 3
   BOOST_CHECK(test.par("C/b/1/b/1/a/</p",     false));
   // a 4
   BOOST_CHECK(test.par("C/b/2/a/>/,/0/p",     true));

   // b 1
   BOOST_CHECK(test.par("C/b/1/b/0/a/</p",     false));

   // b 2
   BOOST_CHECK(test.par("C/b/1/b/1/a/>/,/0/p", true));

   // b 3
   BOOST_CHECK(test.par("C/b/2/a/<//p",        true));

   JumpNode *j= test.jump("C/b/1/}");
   BOOST_CHECK(j->deletesOnForward().empty());
  }

  { // no use of a AFTER repeat
   const char *code=
        "               # test path to assignments\n" \
        "               # a b                     \n" \
        "a=0;           # 1    l/0/a              \n" \
        "repeat {       #      l/1/r              \n" \
        "b=a+1;         # 2 1  l/1/r/b/0/a        \n" \
        "a=b*2;         # 3 2  l/1/r/b/1/a        \n" \
        "} until (3);   #      l/1/r/c            \n";
   UDA_TEST(test(code));

   // a 1
   BOOST_CHECK(test.par("C/b/0/a/</p",         false));
   // a 2
   BOOST_CHECK(test.par("C/b/1/b/0/a/>/,/0/p", true));
   // a 3
   BOOST_CHECK(test.par("C/b/1/b/1/a/</p",     false));

   // b 1
   BOOST_CHECK(test.par("C/b/1/b/0/a/</p",     false));

   // b 2
   BOOST_CHECK(test.par("C/b/1/b/1/a/>/,/0/p", true));

   JumpNode *j= test.jump("C/b/1/}");
   // should have only a (not b)
   BOOST_CHECK( j->deletesOnForward().count("a"));
   BOOST_CHECK(!j->deletesOnForward().count("b"));
  }
}

// things that f***ed up later
BOOST_AUTO_TEST_CASE(testBugs)
{
  using namespace calc;

 typedef std::set<std::string> Set;
 {
  const char *code=
  "CF=inp1s.map;\n" \
  "tmp.res=CF+inp5s.map*(1-CF);\n" ;
  UDA_TEST(test(code));
  Set i;
  i.insert("inp1s.map");
  i.insert("inp5s.map");
  BOOST_CHECK(test.input(i));
 }
 {
  // Is until condition properly cleaned
  const char *code=
    "CF=inp1s.map;\n" \
    "repeat { report tmpUseDef.res=CF+4*(2-CF); } until (CF);\n";
  UDA_TEST(test(code));
  BOOST_CHECK(test.par("C/b/1/r/c/p", false));
 }
 {
  const char *code=
  "timer 1 2 1; \n " \
  "initial dynamic \n " \
  "tmpTimeOutput.res=timeoutput(nominal(inp5s.map),time()*10);\n";
  UDA_TEST(test(code));
 }
 {
  const char *code=
  " timer 1 3 1; initial tmp2.res=catchmenttotal(inpldd.map,1); "\
  "              dynamic tmp.res = accuflux(inpldd.map,1); \n";
  UDA_TEST(test(code));
  // should delete when Dynamic is ready
  JumpNode *j= test.jump("C/b/l/1/}");
  BOOST_CHECK(j);
  BOOST_CHECK( j->deletesOnForward().count("inpldd.map"));
  BOOST_CHECK(!j->deletesOnForward().count("tmp.res"));
 }
 {
  const char *code=
  " timer 1 3 1; initial tmp.res=1; dynamic tmp.res = accuflux(inpldd.map,1); \n";
  UDA_TEST(test(code));
  // should delete when DynamicSection is ready
  JumpNode *ds= test.jump("C/b/l/1/}");
  BOOST_CHECK(ds);
  BOOST_CHECK( ds->deletesOnForward().count("inpldd.map"));
  // hence not in Code "exit"
  JumpNode *cs= test.jump("C/}");
  BOOST_CHECK(cs);
  BOOST_CHECK(!cs->deletesOnForward().count("inpldd.map"));
 }
 {
   const char *code=
            "timer 1 3 1; dynamic report tmp.res=time();";
   UDA_TEST(test(code));
   JumpNode *ds=test.jump("C/b/0}");
   BOOST_CHECK(ds);
   BOOST_CHECK(!ds->deletesOnForward().count("tmp.res"));
   JumpNode *j= test.jump("C/b/0}");
   BOOST_CHECK(!j->deletesOnForward().count("tmp.res"));
   ASTPar *p=test.cast<ASTPar>("C/b/0/b/l/0/a/</p");
   BOOST_CHECK(p);
   BOOST_CHECK(p->lastUse());
 }
}

BOOST_AUTO_TEST_CASE(testXMLReflection)
{
  using namespace calc;

  useDefAnalyzerTest::Test::NameSet mz;
  mz.insert("m");
  mz.insert("z");

  {
   // z should be  deleted after dynamic loop
   const char *code= " timer 1 2 1;"
                     " dynamic"
                     " repeat {"
                     "  x= z eq 0;"
                     " }"
                     " until m le 0;";
   UDA_TEST(test(code));
   JumpNode *dynamicJump=test.jump("C/b/0/}");
   BOOST_CHECK(dynamicJump);
   BOOST_CHECK(dynamicJump->deletesOnForward() == mz);
   JumpNode *repeatJump=test.jump("C/b/0/b/l/0/}");
   BOOST_CHECK(repeatJump);
   BOOST_CHECK(repeatJump->deletesOnForward().empty());
  }

  {
     // z should be  deleted after repeat loop
     const char *code= " repeat {"
                       "  x= z eq 0;"
                       " }"
                       " until m le 0;";
     UDA_TEST(test(code));
     JumpNode *jump=test.jump("C/b/0/}");
     BOOST_CHECK(jump);
     BOOST_CHECK(jump->deletesOnForward() == mz);
  }
}

BOOST_AUTO_TEST_CASE(testIOTypes)
{
  using namespace calc;

  {
  ASTCFGTester  test(StringParser::createCodeAsNode("a=0"));
  std::map<std::string,IOType> r = ioTypes(test.cfg());
  BOOST_CHECK(r.count("a"));
  BOOST_CHECK(r["a"].input() ==pcrxml::ModelInputType::None);
  BOOST_CHECK(r["a"].output()==pcrxml::ModelOutputType::Initial);
  }
  {
  ASTCFGTester  test(StringParser::createCodeAsNode("a=b"));
  std::map<std::string,IOType> r = ioTypes(test.cfg());
  BOOST_CHECK(r.count("a"));
  BOOST_CHECK(r["a"].input() ==pcrxml::ModelInputType::None);
  BOOST_CHECK(r["a"].output()==pcrxml::ModelOutputType::Initial);
  BOOST_CHECK(r.count("b"));
  BOOST_CHECK(r["b"].input() ==pcrxml::ModelInputType::Initial);
  BOOST_CHECK(r["b"].output()==pcrxml::ModelOutputType::Fixed);
  }
  {
    ASTCFGTester  test(StringParser::createCodeAsNode(
          "a=0;         \n" \
          "repeat {     \n" \
          "a=b*2;       \n" \
          "} until (1); \n" ));
    std::map<std::string,IOType> r = ioTypes(test.cfg());
    BOOST_CHECK(r.count("a"));
    BOOST_CHECK(r["a"].input() ==pcrxml::ModelInputType::None);
    BOOST_CHECK(r["a"].output()==pcrxml::ModelOutputType::Initial);
    BOOST_CHECK(r.count("b"));
    BOOST_CHECK(r["b"].input() ==pcrxml::ModelInputType::Initial);
    BOOST_CHECK(r["b"].output()==pcrxml::ModelOutputType::Fixed);
  }
  {
  ASTCFGTester  test(StringParser::createCodeAsNode(
        "initial  dynamic a=b"));
  std::map<std::string,IOType> r = ioTypes(test.cfg());
  BOOST_CHECK(r.count("a"));
  BOOST_CHECK(r["a"].input() ==pcrxml::ModelInputType::None);
  BOOST_CHECK(r["a"].output()==pcrxml::ModelOutputType::Dynamic);
  BOOST_CHECK(r.count("b"));
  BOOST_CHECK(r["b"].input() ==pcrxml::ModelInputType::Dynamic);
  BOOST_CHECK(r["b"].output()==pcrxml::ModelOutputType::Fixed);
  }
  {
  ASTCFGTester  test(StringParser::createCodeAsNode(
        "initial  dynamic a=b; b=sqrt(a);"));
  std::map<std::string,IOType> r = ioTypes(test.cfg());
  BOOST_CHECK(r.count("a"));
  BOOST_CHECK(r["a"].input() ==pcrxml::ModelInputType::None);
  BOOST_CHECK(r["a"].output()==pcrxml::ModelOutputType::Dynamic);
  BOOST_CHECK(r.count("b"));
  BOOST_CHECK(r["b"].input() ==pcrxml::ModelInputType::Initial);
  BOOST_CHECK(r["b"].output()==pcrxml::ModelOutputType::Dynamic);
  }
  {
  ASTCFGTester  test(StringParser::createCodeAsNode(
        "initial b=3;  dynamic a=b;"));
  std::map<std::string,IOType> r = ioTypes(test.cfg());
  BOOST_CHECK(r.count("a"));
  BOOST_CHECK(r["a"].input() ==pcrxml::ModelInputType::None);
  BOOST_CHECK(r["a"].output()==pcrxml::ModelOutputType::Dynamic);
  BOOST_CHECK(r.count("b"));
  BOOST_CHECK(r["b"].input() ==pcrxml::ModelInputType::None);
  BOOST_CHECK(r["b"].output()==pcrxml::ModelOutputType::Initial);
  }
 {
  //ASTNode *e=StringParser::createExpr("a+a"); WORKS!
  //ASTNode *e=StringParser::createExpr("a"); // NOT
  ASTNode *e=StringParser::createExpr("not a"); // WORKS
  bool todo_ioTypes_expr=false;
  BOOST_WARN(todo_ioTypes_expr);
  ASTCFGTester  test(new Code(e));
  std::map<std::string,IOType> r = ioTypes(test.cfg());
  BOOST_WARN(r.count("a"));
  BOOST_WARN(r["a"].input() ==pcrxml::ModelInputType::Initial);
  BOOST_WARN(r["a"].output()==pcrxml::ModelOutputType::Fixed);
  }
}
