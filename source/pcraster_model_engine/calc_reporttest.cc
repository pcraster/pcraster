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
    Report r = Report::reportDefault();
    r.update(t1to3);
    BOOST_TEST(r.atInt(1));
    BOOST_TEST(r.atInt(2));
    BOOST_TEST(r.atInt(3));
  }
  {
    ReportTable rt;

    rt.update(t1to3);

    BOOST_TEST(rt.reportDefault()->atInt(1));
    BOOST_TEST(rt.reportDefault()->atInt(2));
    BOOST_TEST(rt.reportDefault()->atInt(3));

    std::vector<ParsReportMoment> list;
    ParsReportMoment const m = {1, +2, -1};
    list.push_back(m);
    TmpId const id("reportdefault");
    rt.add(Report(id, list));

    rt.update(t1to3);

    BOOST_TEST(rt.reportDefault()->atInt(1));
    BOOST_TEST(!rt.reportDefault()->atInt(2));
    BOOST_TEST(rt.reportDefault()->atInt(3));

    BOOST_TEST(rt.find(id)->atInt(1));
    BOOST_TEST(!rt.find(id)->atInt(2));
    BOOST_TEST(rt.find(id)->atInt(3));
  }
}
