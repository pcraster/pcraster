#define BOOST_TEST_MODULE pcraster model_engine report
#include <boost/test/unit_test.hpp>
#include "calc_reporttable.h"


BOOST_AUTO_TEST_CASE(testReportDefault)
{
  using namespace calc;

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
