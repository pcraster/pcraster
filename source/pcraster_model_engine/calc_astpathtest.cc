#define BOOST_TEST_MODULE pcraster model_engine astpath
#include <boost/test/unit_test.hpp>
#include <sstream>
#include "calc_astpath.h"
#include "calc_aststat.h"
#include "calc_astass.h"
#include "calc_astnode.h"
#include "calc_astpar.h"
#include "calc_astexpr.h"
#include "calc_astnodelist.h"
#include "calc_repeatuntil.h"
#include "calc_stringparser.h"


// NOTE use string failureExpected in files expected to fail, see style guide


BOOST_AUTO_TEST_CASE(test)
{
  using namespace calc;

  const char *code=
        "               # path to assignments\n" \
        "a=0;           # l/0/a              \n" \
        "repeat {       # l/1/r              \n" \
        "b=c+1;         # l/1/r/b/0/a        \n" \
        "d=e*2;         # l/1/r/b/1/a        \n" \
        "} until (3);   # l/1/r/c            \n" \
        "f=g+4;         # l/2                \n" \
        "               #  f:l/2/a/<         \n" \
        "               #  g:l/2/a/>/e/,/0/p \n" \
        "               #  g:2/a/>/,/0/p     \n";

  std::unique_ptr<ASTNode> aPtr(StringParser::createStatementList(code));
  ASTNode *a(aPtr.get());
  {
    ASTNode *l=path(a,"l");
    BOOST_CHECK(dynamic_cast<ASTNodeList *>(l));
  }
  {
   ASTNode *l=path(a,"l/0");
   BOOST_CHECK(dynamic_cast<ASTStat *>(l));
  }
  {
   ASTNode *l=path(a,"l/0/a");
   BOOST_CHECK(dynamic_cast<ASTAss *>(l));
  }
  {
   ASTNode *l=path(a,"l/1/r");
   BOOST_CHECK(dynamic_cast<RepeatUntil *>(l));
   BOOST_CHECK(astCast<RepeatUntil>(a,"l/1/r"));
  }
  {
   ASTPar *p=astCast<ASTPar>(a,"l/2/a/<");
   BOOST_CHECK(p->name()=="f");
  }
  {
   ASTExpr *e=astCast<ASTExpr>(a,"l/2/a/>/e");
   BOOST_CHECK(e->name()=="+");
  }
  { // shortened removing what can be deduced
    //  0-9 implies a prefix l
    //  ,   implies a prefix e
   ASTPar  *p=astCast<ASTPar>(a,"2/a/>/,/0/p");
   BOOST_CHECK(p->name()=="g");
  }
  {
   bool catched=false;
   try {
    path(a,"l/1/a");
   } catch (const std::runtime_error& ) {
     catched=true;
   }
   BOOST_CHECK(catched);
  }
  {
   bool catched=false;
   try {
    path(a,"l/9/a");
   } catch (const std::runtime_error& ) {
     catched=true;
   }
   BOOST_CHECK(catched);
  }
}

