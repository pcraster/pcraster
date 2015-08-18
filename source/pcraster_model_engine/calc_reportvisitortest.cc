#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_REPORTVISITORTEST
#include "calc_reportvisitortest.h"
#define INCLUDED_CALC_REPORTVISITORTEST
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

// Module headers.
#ifndef INCLUDED_CALC_REPORTVISITOR
#include "calc_reportvisitor.h"
#define INCLUDED_CALC_REPORTVISITOR
#endif
#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif
#ifndef INCLUDED_CALC_REPORTTABLE
#include "calc_reporttable.h"
#define INCLUDED_CALC_REPORTTABLE
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif

/*!
  \file
  This file contains the implementation of the ReportVisitorTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC REPORTVISITOR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ReportVisitorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ReportVisitorTest> instance(new ReportVisitorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ReportVisitorTest::testPos, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF REPORTVISITOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ReportVisitorTest::ReportVisitorTest()
{
}

//! setUp
void calc::ReportVisitorTest::setUp()
{
}

//! tearDown
void calc::ReportVisitorTest::tearDown()
{
}

void calc::ReportVisitorTest::testPos()
{

  struct M {
   private:
     ASTNode    *d_code;
   public:
     ReportPars  d_rps;
     M(const std::string& code, bool reportLastAssOfEverySymbol):
       d_code(StringParser::createCodeAsNode(code))
     {
       ReportTable emptyRT;
       ReportVisitor rv(
         reportLastAssOfEverySymbol,
         emptyRT,Timer());
       d_code->accept(rv);
       d_rps = rv.reportPars();
     }
     ~M() {
       delete d_code;
     }
     bool posEqual(std::string const& name, std::string const& pos) const {
     if (!d_rps.count(name))
         return false;
     ReportPars::const_iterator i=d_rps.find(name);
     std::string parPos=i->second.d_par->shortPosText();
     return parPos.compare(pos)==0;
    }
  };

  { // explicit report, first reported
      M m("report p=inp1s.map+0;p=p+2;",false);
      //   1245678901234567890123456
      BOOST_CHECK(m.d_rps.count("p"));
      BOOST_CHECK(m.posEqual("p","line '1:8'"));
  }
  { // explicit report BUT last ass reported
      M m("report p=inp1s.map+0;p=p+2;",true);
      //   12345678901234567890123456
      BOOST_CHECK(m.d_rps.count("p"));
      BOOST_CHECK(m.posEqual("p","line '1:22'"));
  }
  { // no explicit set, and No  last ass reported
      M m("p=inp1s.map+0;p=p+2;",false);
      BOOST_CHECK(!m.d_rps.count("p"));
  }
  { // no explicit set, but last ass reported
      M m("p=inp1s.map+0;p=p+2;",true);
      //   1234567890123456
      BOOST_CHECK(m.d_rps.count("p"));
      BOOST_CHECK(m.posEqual("p","line '1:15'"));
  }
}
