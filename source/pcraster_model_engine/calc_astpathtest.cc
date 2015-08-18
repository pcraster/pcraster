#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTPATHTEST
#include "calc_astpathtest.h"
#define INCLUDED_CALC_ASTPATHTEST
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
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif


// Module headers.
#ifndef INCLUDED_CALC_ASTPATH
#include "calc_astpath.h"
#define INCLUDED_CALC_ASTPATH
#endif
#ifndef INCLUDED_CALC_POINTCODEBLOCK
#include "calc_pointcodeblock.h"
#define INCLUDED_CALC_POINTCODEBLOCK
#endif
#ifndef INCLUDED_CALC_NONASSEXPR
#include "calc_nonassexpr.h"
#define INCLUDED_CALC_NONASSEXPR
#endif
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_ASTNODE
#include "calc_astnode.h"
#define INCLUDED_CALC_ASTNODE
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTEXPR
#include "calc_astexpr.h"
#define INCLUDED_CALC_ASTEXPR
#endif
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif
#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
#ifndef INCLUDED_CALC_REPEATUNTIL
#include "calc_repeatuntil.h"
#define INCLUDED_CALC_REPEATUNTIL
#endif
#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif
#ifndef INCLUDED_CALC_BLOCKENTRANCE
#include "calc_blockentrance.h"
#define INCLUDED_CALC_BLOCKENTRANCE
#endif
#ifndef INCLUDED_CALC_JUMPNODE
#include "calc_jumpnode.h"
#define INCLUDED_CALC_JUMPNODE
#endif
#ifndef INCLUDED_CALC_CODE
#include "calc_code.h"
#define INCLUDED_CALC_CODE
#endif


/*!
  \file
  This file contains the implementation of the ASTPathTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// VISIBLE IN OTHER UNIT TEST BY 2nd DECLATION in calc_astpath.h
//------------------------------------------------------------------------------

namespace calc {
  struct ASTPathCaster {
    ASTNode     *d_node;

    template <typename T>
      T *expect(const char *name) {
        T *t=dynamic_cast<T *>(d_node);
        if(!t) {
          const std::type_info& gotClass = typeid (*d_node);
          std::ostringstream s;
          s << "expecting " << name
            << " got another ASTNode subclass: "
            << std::string(gotClass.name ());

          throw com::Exception(s.str());
        }
        return t;
      }
#define EXPECT(X)  expect<X>(#X)

    ASTNode *node() {
      return d_node;
    }

    ASTPathCaster(ASTNode *n) {
      d_node=n;
    }

  void add(char c) {
    // implies notes means shortcuts
    switch(c) {
      case '/': break;
      case 'l':
        EXPECT(ASTNodeList);
        break;
      case 'a':
        d_node=EXPECT(ASTStat)->stat();
        EXPECT(ASTAss);
        break;
      case '<': // lhs of ass
        d_node=EXPECT(ASTAss)->par(0);
        break;
      case '>': // rhs of ass
        d_node=EXPECT(ASTAss)->rhs();
        break;
      case 'p':
        EXPECT(ASTPar);
        break;
      case 'n':
        EXPECT(ASTNumber);
        break;
      case 'e':
        EXPECT(ASTExpr);
        break;
      case ',':
        //  ,   implies a prefix e
        // args: ASTNodeVector of expr
        d_node=EXPECT(ASTExpr)->args();
        break;
      case 'C':
        EXPECT(Code);
        break;
      case 'r':
        EXPECT(RepeatUntil);
        break;
      case 'P':
        EXPECT(PointCodeBlock);
        break;
      case 'R':
        d_node=EXPECT(PointCodeBlock)->replacedCode();
        break;
      case 'c':// condition of RepeatUntil
        d_node=EXPECT(RepeatUntil)->condition();
        d_node=EXPECT(NonAssExpr)->expr();
        break;
      case 'b': // statements of the BasicBlock
        // b implies a prefix that is a subclass of BasicBlock
        d_node=EXPECT(BasicBlock)->statements();
        break;
      case '{': // BlockEntrance
        // { implies a prefix that is a subclass of BasicBlock
        d_node=EXPECT(BasicBlock)->blockEntrance();
        EXPECT(BlockEntrance);
        break;
      case '}': // JumpNode
        d_node=EXPECT(BasicBlock)->jumpNode();
        EXPECT(JumpNode);
        break;
      default: // a number
        //  0-9 implies a prefix l or expr argument
        if (!(c >= '0' && c <= '9'))
          throw com::Exception("Unknown path character");
        size_t pos= c-'0';
        ASTNodeList *sl=dynamic_cast<ASTNodeList *>(d_node);
        if (sl) {
          ASTNodeList::const_iterator i=sl->begin();
          size_t n=0;
          for( ; n!=pos && i!=sl->end(); ++i)
            n++;
          if (i==sl->end())
           throw com::Exception("invalid index in ASTSTatList");
          d_node=*i;
        } else {
         if (pos >= EXPECT(ASTNodeVector)->size())
           throw com::Exception("invalid index in ASTNodeVector");
         d_node=EXPECT(ASTNodeVector)->at(pos);
        }
    }
  }
};

} // namespace calc

/*!
 * \returns non-0 node found in root with pathStr
 * \throws  std::runtime_error if pathStr leads not to the correct node in root
 */
calc::ASTNode *calc::path(ASTNode* root, const char *pathStr) {
  PRECOND(root);
  ASTPathCaster current(root);

  const char *ptr;
  try {
    for(ptr=pathStr; *ptr; ++ptr)
       current.add(*ptr);
  } catch (com::Exception& e) {
    std::ostringstream s;
    s << "Error in path: " << pathStr << " at point " << ptr;
    e.prepend(s.str());
    throw std::runtime_error(e.getMessages());
  }
  POSTCOND(current.node());
  return current.node();
}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTPATH MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ASTPathTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ASTPathTest> instance(new ASTPathTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ASTPathTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ASTPATH MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ASTPathTest::ASTPathTest()
{
}



//! setUp
void calc::ASTPathTest::setUp()
{
}



//! tearDown
void calc::ASTPathTest::tearDown()
{
}



void calc::ASTPathTest::test()
{
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

  std::auto_ptr<ASTNode> aPtr(StringParser::createStatementList(code));
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

