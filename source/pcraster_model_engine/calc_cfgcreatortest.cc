#define BOOST_TEST_MODULE pcraster model_engine cfgcreator
#include <boost/test/unit_test.hpp>
#include "calc_posexception.h"
#include "calc_astnode.h"
#include "calc_cfgnode.h"
#include "calc_astexpr.h"
#include "calc_astpar.h"
#include "calc_astass.h"
#include "calc_aststat.h"
#include "calc_astnodelist.h"
#include "calc_stringparser.h"
#include "calc_astcfgtester.h"


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



BOOST_AUTO_TEST_CASE(testExpr)
{
  using namespace calc;

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

BOOST_AUTO_TEST_CASE(testStatementList)
{
  using namespace calc;

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


//
// OLS: this unit test was commented in the old setup ?!
//
// BOOST_AUTO_TEST_CASE(testRewrites)
// {
//   using namespace calc;
//
//     ASTCFGTester l(sp.createStatementList("p=timeoutput(a,b)"));
//     BOOST_CHECK(dynamic_cast<ASTNodeList *>(l.ast()));
//
//     CFGNode* c(l.cfg());
//     const char *names[]={ "stat-start",
//                           "a","b","p","timeoutput", "ass-p"};
//     size_t ic(0);
//     const CFGNode* i=c;
//     const CFGNode* last;
//     do {
//       BOOST_CHECK(ic < ARRAY_SIZE(names));
//       cfgCreatorTest::CmpNode cn(names[ic], i);
//       BOOST_CHECK(cn.validASTNode());
//       BOOST_CHECK(cn.equal());
//       last=i;
//       i=i->succ(0);
//       ic++;
//     } while(i);
//     BOOST_CHECK(ic==ARRAY_SIZE(names));
//     // test pred
//     while(last) {
//       --ic;
//       cfgCreatorTest::CmpNode cn(names[ic], last);
//       BOOST_CHECK(cn.validASTNode());
//       BOOST_CHECK(cn.equal());
//       last=last->pred();
//     }
//     BOOST_CHECK(!ic); // all checked
// }
