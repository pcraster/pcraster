#ifndef INCLUDED_RANGECLASSIFICATIONTEST
#include "RangeClassificationTest.h"
#define INCLUDED_RANGECLASSIFICATIONTEST
#endif

// External headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_FLOATING_POINT_COMPARISON
#include <boost/test/floating_point_comparison.hpp>
#define INCLUDED_BOOST_TEST_FLOATING_POINT_COMPARISON
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_RANGECLASSIFICATION
#include "RangeClassification.h"
#define INCLUDED_RANGECLASSIFICATION
#endif



/*!
  \file
  This file contains the implementation of the RangeClassificationTest class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RANGECLASSIFICATIONTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* RangeClassificationTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RangeClassificationTest> instance(
         new RangeClassificationTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &RangeClassificationTest::testDefaultConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &RangeClassificationTest::testLinear, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &RangeClassificationTest::testLog10, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RANGECLASSIFICATIONTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
RangeClassificationTest::RangeClassificationTest()
{
}



void RangeClassificationTest::testDefaultConstructor()
{
  {
    RangeClassification<float> classification;

    BOOST_REQUIRE_EQUAL(classification.size(), 11u);
    BOOST_REQUIRE_EQUAL(classification.nrClasses(), 10u);

    BOOST_CHECK_CLOSE(classification.minCutoff(), 0.0f, 0.001f);
    BOOST_CHECK_CLOSE(classification.maxCutoff(), 1.0f, 0.001f);

    BOOST_CHECK_CLOSE(classification[3], 0.3f, 0.001f);

    BOOST_CHECK_EQUAL(classification.index(0.3f), 3u);
    BOOST_CHECK_EQUAL(classification.index(0.29f), 2u);
    BOOST_CHECK_EQUAL(classification.index(0.31f), 3u);

    BOOST_CHECK_EQUAL(classification.index(0.0f), 0u);
    BOOST_CHECK_EQUAL(classification.index(1.0f), 10u);
  }
}



void RangeClassificationTest::testLinear()
{
  {
    RangeClassification<float> classification(10.0f, 20.0f, 10,
         RangeClassification<float>::Linear);

    BOOST_REQUIRE_EQUAL(classification.size(), 11u);
    BOOST_REQUIRE_EQUAL(classification.nrClasses(), 10u);

    BOOST_CHECK_CLOSE(classification.minCutoff(), 10.0f, 0.001f);
    BOOST_CHECK_CLOSE(classification.maxCutoff(), 20.0f, 0.001f);

    BOOST_CHECK_CLOSE(classification[3], 13.0f, 0.001f);

    BOOST_CHECK_EQUAL(classification.index(13.0f), 3u);
    BOOST_CHECK_EQUAL(classification.index(12.9f), 2u);
    BOOST_CHECK_EQUAL(classification.index(13.1f), 3u);

    BOOST_CHECK_EQUAL(classification.index(10.0f), 0u);
    BOOST_CHECK_EQUAL(classification.index(20.0f), 10u);
  }
}



void RangeClassificationTest::testLog10()
{
  {
    RangeClassification<float> classification(1.0f, 1000.0f, 4,
         RangeClassification<float>::Log10);

    BOOST_REQUIRE_EQUAL(classification.size(), 4u);
    BOOST_REQUIRE_EQUAL(classification.nrClasses(), 3u);

    BOOST_CHECK_CLOSE(classification.minCutoff(), 1.0f, 0.001f);
    BOOST_CHECK_CLOSE(classification.maxCutoff(), 1000.0f, 0.001f);

    BOOST_CHECK_CLOSE(classification[0], 1.0f, 0.001f);
    BOOST_CHECK_CLOSE(classification[1], 10.0f, 0.001f);
    BOOST_CHECK_CLOSE(classification[2], 100.0f, 0.001f);
    BOOST_CHECK_CLOSE(classification[3], 1000.0f, 0.001f);

    BOOST_CHECK_EQUAL(classification.index(50.0f), 1u);
    BOOST_CHECK_EQUAL(classification.index(5.0f), 0u);
    BOOST_CHECK_EQUAL(classification.index(500.0f), 2u);

    BOOST_CHECK_EQUAL(classification.index(1.0f), 0u);
    BOOST_CHECK_EQUAL(classification.index(1000.0f), 3u);
  }
}

} // namespace ag

