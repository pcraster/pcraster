#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_REPORTTEST
#include "calc_reporttest.h"
#define INCLUDED_CALC_REPORTTEST
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
#ifndef INCLUDED_CALC_REPORTTABLE
#include "calc_reporttable.h"
#define INCLUDED_CALC_REPORTTABLE
#endif

/*!
  \file
  This file contains the implementation of the ReportTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC REPORT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ReportTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ReportTest> instance(new ReportTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ReportTest::testReportDefault, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF REPORT MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ReportTest::ReportTest()
{
}



//! setUp
void calc::ReportTest::setUp()
{
}

//! tearDown
void calc::ReportTest::tearDown()
{
}

void calc::ReportTest::testReportDefault()
{
  Timer t1to3;
  t1to3.setStartInt(1);
  t1to3.setLastInt(3);

  {
    Report r=Report::reportDefault();
    r.update(t1to3);
    BOOST_CHECK(r.atInt(1));
    BOOST_CHECK(r.atInt(2));
    BOOST_CHECK(r.atInt(3));
  }
  {
    ReportTable rt;

    rt.update(t1to3);

    BOOST_CHECK(rt.reportDefault()->atInt(1));
    BOOST_CHECK(rt.reportDefault()->atInt(2));
    BOOST_CHECK(rt.reportDefault()->atInt(3));

    std::vector<ParsReportMoment> list;
    ParsReportMoment m = {1,+2,-1};
    list.push_back(m);
    TmpId id("reportdefault");
    rt.add(Report(id,list));

    rt.update(t1to3);

    BOOST_CHECK( rt.reportDefault()->atInt(1));
    BOOST_CHECK(!rt.reportDefault()->atInt(2));
    BOOST_CHECK( rt.reportDefault()->atInt(3));

    BOOST_CHECK( rt.find(id)->atInt(1));
    BOOST_CHECK(!rt.find(id)->atInt(2));
    BOOST_CHECK( rt.find(id)->atInt(3));
  }
}
