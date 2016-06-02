#define BOOST_TEST_MODULE pcraster model_engine pointcodeblockreplacer
#include <boost/test/unit_test.hpp>
#include <iostream>
#include "calc_astcfgtester.h"
#include "calc_stringparser.h"
#include "calc_astpath.h"
#include "calc_aststat.h"
#include "calc_astsymboltable.h"
#include "calc_insertpointcodeblocks.h"


#define private public
#include "calc_pointcodeblock.h"


// NOTE use string failureExpected in files expected to fail, see style guide


namespace calc {
  namespace pointCodeBlockReplacerTest {
   class Test {

     ASTCFGTester d_a;
   public:
     Test(const char *code):
       d_a(StringParser::createCodeAsNode(code))
     {
       insertPointCodeBlocks(ASTSymbolTable(),d_a.ast(),d_a.cfg());
     }
     ASTNode *ast() const {
       return d_a.ast();
     }
     PointCodeBlock *block(const char* path) const {
       return astCast<PointCodeBlock>(d_a.ast(),path);
     }
  };
 }
}

#define CASE(C)  pointCodeBlockReplacerTest::Test C


BOOST_AUTO_TEST_CASE(test)
{
  using namespace calc;

  PointCodeBlock *b;
  std::set<std::string> emptyS;
  std::set<std::string> xS;
  xS.insert("x");
  std::set<std::string> aS;
  aS.insert("a");
  std::set<std::string> abS;
  abS.insert("a");
  abS.insert("b");
  std::set<std::string> axS;
  axS.insert("a");
  axS.insert("x");
  std::set<std::string> eS;
  eS.insert("e");
  std::set<std::string> dS;
  dS.insert("d");

  {

   const char *code=
        "               # path          \n" \
        "               #               \n" \
        "a=0; # only ass#  l/0/a        \n" \
        "repeat {       #  l/1/r        \n" \
        "x=a+1; # P0    #  l/1/r/b/0/a  \n" \
        "a=x*2; # P0    #  l/1/r/b/1/a  \n" \
        "} until (3);   #  l/1/r/c      \n" \
        "a=b*2-a; # P1  #               \n" \
        "a=maptotal(g)+3; # l/3         \n" \
        "d=e+3;   # P2      l/4         \n" \
        "d=e+3;   # P2                  \n" \
        "d=e+3;   # P2                  \n" \
        "t=maptotal(d)+4; # l/5         \n" \
        "y=z;             # l/6         \n" \
        "t=maptotal(d)+5; #             \n";
   CASE(test(code));

   // a = 0

   // a=0 only assignments, not a block
   BOOST_CHECK(astCast<ASTStat>(test.ast(),"C/b/l/0"));
   ASTPar *p(astCast<ASTPar>(test.ast(), "C/b/l/0/a/<"));
   BOOST_CHECK(p);
   BOOST_CHECK( p->name() == "a");
   BOOST_CHECK(!p->lastUse());

   // a=maptotal(g)+3;
   BOOST_CHECK(astCast<ASTStat>(test.ast(),"C/b/l/3"));
   // y=z only assignments
   BOOST_CHECK(astCast<ASTStat>(test.ast(),"C/b/l/6"));

   // P0
   b=test.block("C/b/l/1/r/b/0/P");
   BOOST_CHECK(b->rangeText()=="BLOCK [line '5:1',line '6:1']");
   BOOST_CHECK(b->d_size==2);
   BOOST_CHECK(b->input()==aS);
   BOOST_CHECK(b->local()==xS);
   // a = x*2
   p= astCast<ASTPar>(b, "P/R/b/l/1/a/<");
   BOOST_CHECK(p);
   BOOST_CHECK( p->name() == "a");
   BOOST_CHECK(!p->lastUse());
   BOOST_CHECK(b->output()==aS);

   // P1
   b=test.block("C/b/l/2/P");
   //std::cout << "P1 " << *b << std::endl;
   BOOST_CHECK(b->rangeText()=="BLOCK [line '8:1',line '8:1']");
   BOOST_CHECK(b->d_size ==1);
   BOOST_CHECK(b->input()==abS);
   BOOST_CHECK(b->local()==emptyS);
   BOOST_CHECK(b->output()==emptyS); // since a is redefined after this block

   // P2
   b=test.block("C/b/l/4/P");
   //std::cout << "P2 " << *b << std::endl;
   BOOST_CHECK(b->rangeText()=="BLOCK [line '10:1',line '12:1']");
   BOOST_CHECK(b->d_size ==3);
   BOOST_CHECK(b->input()==eS);
   BOOST_CHECK(b->local()==emptyS);
   BOOST_CHECK(b->output()==dS);

  }
  {
   const char *code= "a=maptotal(b)+3;";
   CASE(test(code));
   BOOST_CHECK(astCast<ASTStat>(test.ast(),"C/b/l/0"));
  }
  {
   const char *code="a=b*2-a;z=maptotal(a)";
   CASE(test(code));
   b=test.block("C/b/l/0/P");
   BOOST_CHECK(b->rangeText()=="BLOCK [line '1:1',line '1:1']");
   BOOST_CHECK(b->d_size ==1);
   BOOST_CHECK(b->input()==abS);
   BOOST_CHECK(b->local()==emptyS);
   BOOST_CHECK(b->output()==aS);
  }
}
