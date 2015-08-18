#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CFGCREATORTEST
#include "calc_cfgcreatortest.h"
#define INCLUDED_CALC_CFGCREATORTEST
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
#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif
#ifndef INCLUDED_CALC_CFGNODE
#include "calc_cfgnode.h"
#define INCLUDED_CALC_CFGNODE
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
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif

#ifndef INCLUDED_CALC_ASTCFGTESTER
#include "calc_astcfgtester.h"
#define INCLUDED_CALC_ASTCFGTESTER
#endif

/*!
  \file
  This file contains the implementation of the CFGCreatorTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CFGCREATOR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::CFGCreatorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CFGCreatorTest> instance(new CFGCreatorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CFGCreatorTest::testExpr, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CFGCreatorTest::testStatementList, instance));
//suite->add(BOOST_CLASS_TEST_CASE(&CFGCreatorTest::testRewrites, instance));
  return suite;
}

static const calc::StringParser sp;

namespace cfgCreatorTest {
  struct CmpNode {
    const char   *d_name;
    const calc::ASTId   *d_id;
    const calc::ASTAss  *d_ass;
    const calc::ASTStat *d_stat;
    CmpNode(const char *name,
            const calc::CFGNode    *i):
      d_name(name)
    {
      d_id =dynamic_cast<const calc::ASTId *>(i->node());
      d_ass=dynamic_cast<const calc::ASTAss *>(i->node());
      d_stat=dynamic_cast<const calc::ASTStat *>(i->node());
    }
    bool validASTNode() const {
      return d_id||d_ass||d_stat;
    }
    bool equal() const {
     PRECOND(validASTNode());
     if (d_id)
       return d_id->name()==d_name;
     std::string name;
     if (d_ass)
       name="ass-"+d_ass->par()->name();
     if (d_stat)
       name="stat-start"; // +d_stat->par()->name();
     return name==d_name;
    }
  };
}

//------------------------------------------------------------------------------
// DEFINITION OF CFGCREATOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::CFGCreatorTest::CFGCreatorTest()
{
}

//! setUp
void calc::CFGCreatorTest::setUp()
{
}

//! tearDown
void calc::CFGCreatorTest::tearDown()
{
}



void calc::CFGCreatorTest::testExpr()
{
  {
     ASTCFGTester e(sp.createExpr("a"));
     BOOST_CHECK(dynamic_cast<ASTPar *>(e.ast()));

     CFGNode* c(e.cfg());
     BOOST_CHECK(c->node() == e.ast());
  }

  {
    ASTCFGTester e(sp.createExpr("a*b"));
    BOOST_CHECK(dynamic_cast<ASTExpr *>(e.ast()));

     CFGNode* c(e.cfg());
     size_t ic(0);
     const CFGNode* i=c;
     do {
       switch(ic) {
         case 0:
          {
           ASTPar *p(dynamic_cast<ASTPar *>(i->node()));
           BOOST_CHECK(p);
           BOOST_CHECK(p->name()=="a");
          } break;
         case 1:
          {
           ASTPar *p(dynamic_cast<ASTPar *>(i->node()));
           BOOST_CHECK(p);
           BOOST_CHECK(p->name()=="b");
          } break;
         case 2:
          {
           ASTExpr *en(dynamic_cast<ASTExpr *>(i->node()));
           BOOST_CHECK(en);
           BOOST_CHECK(en->name()=="*");
           BOOST_CHECK(en==e.ast());
          } break;
         default:
           BOOST_CHECK(false);
      }
      i=i->succ(0);
      ic++;
    } while(i);
  }
  {
    ASTCFGTester e(sp.createExpr("((1+2)*(3-4))/5"));
    BOOST_CHECK(dynamic_cast<ASTExpr *>(e.ast()));

    CFGNode* c(e.cfg());
    const char *names[]={ "1","2","+","3","4","-","*","5","/"};
    size_t ic(0);
    const CFGNode* i=c;
    const CFGNode* last;
    do {
      BOOST_CHECK(ic < ARRAY_SIZE(names));
      cfgCreatorTest::CmpNode cn(names[ic], i);
      BOOST_CHECK(cn.validASTNode());
      BOOST_CHECK(cn.equal());
      last=i;
      i=i->succ(0);
      ic++;
    } while(i);
    BOOST_CHECK(ic==ARRAY_SIZE(names));
    // test pred
    while(last) {
      --ic;
      cfgCreatorTest::CmpNode cn(names[ic], last);
      BOOST_CHECK(cn.validASTNode());
      BOOST_CHECK(cn.equal());
      last=last->pred();
    }
    BOOST_CHECK(!ic); // all checked
  }
}

void calc::CFGCreatorTest::testStatementList()
{
  {
    ASTCFGTester l(sp.createStatementList("a=(1+2)*3;b=a/5"));
    BOOST_CHECK(dynamic_cast<ASTNodeList *>(l.ast()));

    CFGNode* c(l.cfg());
    const char *names[]={ "stat-start","1","2","+","3","*",
                          "ass-a","stat-start","a","5","/","ass-b"};
    size_t ic(0);
    const CFGNode* i=c;
    const CFGNode* last;
    // forward test
    do {
      BOOST_CHECK(ic < ARRAY_SIZE(names));
      cfgCreatorTest::CmpNode cn(names[ic], i);
      BOOST_CHECK(cn.validASTNode());
      BOOST_CHECK(cn.equal());
      last=i;
      i=i->succ(0);
      ic++;
    } while(i);
    BOOST_CHECK(ic==ARRAY_SIZE(names));
    // test pred
    // backward test
    while(last) {
      --ic;
      cfgCreatorTest::CmpNode cn(names[ic], last);
      BOOST_CHECK(cn.validASTNode());
      BOOST_CHECK(cn.equal());
      last=last->pred();
    }
    BOOST_CHECK(!ic); // all checked
  }
}

void calc::CFGCreatorTest::testRewrites()
{
    ASTCFGTester l(sp.createStatementList("p=timeoutput(a,b)"));
    BOOST_CHECK(dynamic_cast<ASTNodeList *>(l.ast()));

    CFGNode* c(l.cfg());
    const char *names[]={ "stat-start",
                          "a","b","p","timeoutput", "ass-p"};
    size_t ic(0);
    const CFGNode* i=c;
    const CFGNode* last;
    do {
      BOOST_CHECK(ic < ARRAY_SIZE(names));
      cfgCreatorTest::CmpNode cn(names[ic], i);
      BOOST_CHECK(cn.validASTNode());
      BOOST_CHECK(cn.equal());
      last=i;
      i=i->succ(0);
      ic++;
    } while(i);
    BOOST_CHECK(ic==ARRAY_SIZE(names));
    // test pred
    while(last) {
      --ic;
      cfgCreatorTest::CmpNode cn(names[ic], last);
      BOOST_CHECK(cn.validASTNode());
      BOOST_CHECK(cn.equal());
      last=last->pred();
    }
    BOOST_CHECK(!ic); // all checked
}
