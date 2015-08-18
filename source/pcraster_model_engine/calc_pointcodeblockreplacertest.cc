#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_POINTCODEBLOCKREPLACERTEST
#include "calc_pointcodeblockreplacertest.h"
#define INCLUDED_CALC_POINTCODEBLOCKREPLACERTEST
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
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

// Module headers.
#ifndef INCLUDED_CALC_ASTCFGTESTER
#include "calc_astcfgtester.h"
#define INCLUDED_CALC_ASTCFGTESTER
#endif
#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif
#ifndef INCLUDED_CALC_ASTPATH
#include "calc_astpath.h"
#define INCLUDED_CALC_ASTPATH
#endif
#ifndef INCLUDED_CALC_INSERTPOINTCODEBLOCKS
#include "calc_insertpointcodeblocks.h"
#define INCLUDED_CALC_INSERTPOINTCODEBLOCKS
#endif
#ifndef INCLUDED_CALC_POINTCODEBLOCK
#include "calc_pointcodeblock.h"
#define INCLUDED_CALC_POINTCODEBLOCK
#endif
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
/*!
  \file
  This file contains the implementation of the PointCodeBlockReplacerTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC POINTCODEBLOCKREPLACER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::PointCodeBlockReplacerTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PointCodeBlockReplacerTest> instance(new PointCodeBlockReplacerTest());

  suite->add(BOOST_CLASS_TEST_CASE(&PointCodeBlockReplacerTest::test, instance));

  return suite;
}

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



//------------------------------------------------------------------------------
// DEFINITION OF POINTCODEBLOCKREPLACER MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::PointCodeBlockReplacerTest::PointCodeBlockReplacerTest()
{
}



//! setUp
void calc::PointCodeBlockReplacerTest::setUp()
{
}



//! tearDown
void calc::PointCodeBlockReplacerTest::tearDown()
{
}


#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

void calc::PointCodeBlockReplacerTest::test()
{
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
