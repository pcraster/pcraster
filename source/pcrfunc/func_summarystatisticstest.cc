#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FUNC_SUMMARYSTATISTICSTEST
#include "func_summarystatisticstest.h"
#define INCLUDED_FUNC_SUMMARYSTATISTICSTEST
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
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.
#ifndef INCLUDED_FUNC_SUMMARYSTATISTICS
#include "func_summarystatistics.h"
#define INCLUDED_FUNC_SUMMARYSTATISTICS
#endif



/*!
  \file
  This file contains the implementation of the SummaryStatisticsTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace func {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SUMMARYSTATISTICSTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*SummaryStatisticsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SummaryStatisticsTest> instance(new SummaryStatisticsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&SummaryStatisticsTest::testMean, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SUMMARYSTATISTICSTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
SummaryStatisticsTest::SummaryStatisticsTest(
         )
{
}



//! setUp
void SummaryStatisticsTest::setUp()
{
}



//! tearDown
void SummaryStatisticsTest::tearDown()
{
}



void SummaryStatisticsTest::testMean()
{
  {
    float sources[] = { 100.0f, 200.0f, 300.0f };

    {
      double result;

      mean<float, double>(&sources[0], 3, result);
      BOOST_CHECK(dal::comparable<double>(result, 200.0));
    }

    {
      float result;

      mean<float, float>(&sources[0], 3, result);
      BOOST_CHECK(dal::comparable<float>(result, 200.0f));

      pcr::setMV(sources[0]);
      mean<float, float>(&sources[0], 3, result);
      BOOST_CHECK(dal::comparable<float>(result, 250.0f));

      pcr::setMV(sources[1]);
      mean<float, float>(&sources[0], 3, result);
      BOOST_CHECK(dal::comparable<float>(result, 300.0f));

      pcr::setMV(sources[2]);
      mean<float, float>(&sources[0], 3, result);
      BOOST_CHECK(pcr::isMV(result));
    }
  }

  {
    std::vector<float> sources;
    sources.push_back(100.0f);
    sources.push_back(200.0f);
    sources.push_back(300.0f);

    {
      double result;

      mean<std::vector<float>::const_iterator, double>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(dal::comparable<double>(result, 200.0));
    }

    {
      float result;

      mean<std::vector<float>::const_iterator, float>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(dal::comparable<float>(result, 200.0f));

      pcr::setMV(sources[0]);
      mean<std::vector<float>::const_iterator, float>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(dal::comparable<float>(result, 250.0f));

      pcr::setMV(sources[1]);
      mean<std::vector<float>::const_iterator, float>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(dal::comparable<float>(result, 300.0f));

      pcr::setMV(sources[2]);
      mean<std::vector<float>::const_iterator, float>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(pcr::isMV(result));
    }
  }
}

} // namespace func

