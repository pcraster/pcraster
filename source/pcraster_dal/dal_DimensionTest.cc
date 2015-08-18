#ifndef INCLUDED_DAL_DIMENSIONTEST
#include "dal_DimensionTest.h"
#define INCLUDED_DAL_DIMENSIONTEST
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
#ifndef INCLUDED_DAL_DIMENSION
#include "dal_Dimension.h"
#define INCLUDED_DAL_DIMENSION
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif



/*!
  \file
  This file contains the implementation of the DimensionTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DIMENSION MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::DimensionTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DimensionTest> instance(new DimensionTest());

  suite->add(BOOST_CLASS_TEST_CASE(
         &DimensionTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DimensionTest::testContainsValueInRange, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DimensionTest::testClamp, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DimensionTest::testIndexOfValueOf, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DimensionTest::testMerge, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DimensionTest::testIntersect, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DimensionTest::testNrCoordinates, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DimensionTest::testIsWide, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DIMENSION MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::DimensionTest::DimensionTest()
{
}



//! setUp
void dal::DimensionTest::setUp()
{
}



//! tearDown
void dal::DimensionTest::tearDown()
{
}



void dal::DimensionTest::test()
{
  {
    std::set<std::string> scenarios;
    scenarios.insert("aap");
    Dimension dimension(Scenarios, scenarios);

    // Scenario names are exact.
    BOOST_CHECK_EQUAL(dimension.discretisation(), ExactDiscretisation);

    BOOST_CHECK(!dimension.isWide());
  }
}



void dal::DimensionTest::testContainsValueInRange()
{
  {
    std::vector<size_t> values;
    values.push_back(2);
    values.push_back(5);
    values.push_back(1);
    Dimension dimension(Time, values);
    BOOST_CHECK(!dimension.containsValueInRange<size_t>(0));
    BOOST_CHECK(!dimension.containsValueInRange<size_t>(1));
    BOOST_CHECK( dimension.containsValueInRange<size_t>(2));
    BOOST_CHECK( dimension.containsValueInRange<size_t>(3));
    BOOST_CHECK( dimension.containsValueInRange<size_t>(4));
    BOOST_CHECK( dimension.containsValueInRange<size_t>(5));
    BOOST_CHECK(!dimension.containsValueInRange<size_t>(6));
  }

  {
    std::vector<size_t> values;
    values.push_back(2);
    values.push_back(5);
    values.push_back(2);
    Dimension dimension(Time, values);
    BOOST_CHECK(!dimension.containsValueInRange<size_t>(0));
    BOOST_CHECK(!dimension.containsValueInRange<size_t>(1));
    BOOST_CHECK( dimension.containsValueInRange<size_t>(2));
    BOOST_CHECK(!dimension.containsValueInRange<size_t>(3));
    BOOST_CHECK( dimension.containsValueInRange<size_t>(4));
    BOOST_CHECK(!dimension.containsValueInRange<size_t>(5));
    BOOST_CHECK(!dimension.containsValueInRange<size_t>(6));
  }

  {
    std::vector<float> values;
    values.push_back(0.01f);
    values.push_back(0.99f);
    values.push_back(0.01f);
    Dimension dimension(CumulativeProbabilities, values);
    BOOST_CHECK(!dimension.containsValueInRange<float>(0.00f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.01f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.02f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.03f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.04f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.05f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.50f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.95f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.96f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.97f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.98f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.99f));
    BOOST_CHECK(!dimension.containsValueInRange<float>(1.00f));
  }

  {
    std::vector<float> values;
    values.push_back(0.01f);
    values.push_back(0.99f);
    values.push_back(0.02f);
    Dimension dimension(CumulativeProbabilities, values);
    BOOST_CHECK(!dimension.containsValueInRange<float>(0.00f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.01f));
    BOOST_CHECK(!dimension.containsValueInRange<float>(0.02f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.03f));
    BOOST_CHECK(!dimension.containsValueInRange<float>(0.04f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.05f));
    BOOST_CHECK(!dimension.containsValueInRange<float>(0.50f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.95f));
    BOOST_CHECK(!dimension.containsValueInRange<float>(0.96f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.97f));
    BOOST_CHECK(!dimension.containsValueInRange<float>(0.98f));
    BOOST_CHECK( dimension.containsValueInRange<float>(0.99f));
    BOOST_CHECK(!dimension.containsValueInRange<float>(1.00f));
  }
}



void dal::DimensionTest::testClamp()
{
  std::vector<float> values;
  values.push_back(0.01f);
  values.push_back(0.99f);
  values.push_back(0.01f);
  Dimension dimension(CumulativeProbabilities, values);
  BOOST_CHECK(dal::comparable<float>(dimension.clamp<float>(0.5), 0.5f));
}




void dal::DimensionTest::testIndexOfValueOf()
{
  float first = 0.01f;
  float last = 0.99f;
  float interval = 0.01f;

  std::vector<float> values;
  values.push_back(first);
  values.push_back(last);
  values.push_back(interval);
  Dimension dimension(CumulativeProbabilities, values);

  BOOST_REQUIRE_EQUAL(dimension.nrCoordinates(), size_t(99));

  for(size_t i = 0; i < dimension.nrCoordinates(); ++i) {
    BOOST_CHECK_EQUAL(dimension.indexOf<float>(first + i * interval), i);
    BOOST_CHECK(comparable<float>(dimension.coordinate<float>(i),
         first + i * interval));
  }
}



void dal::DimensionTest::testMerge()
{
  {
    std::vector<size_t> values1, values2;

    // 1, 3, 5, 7, 9
    values1.push_back(1);
    values1.push_back(10);
    values1.push_back(2);

    // 2, 4, 6, 8, 10
    values2.push_back(2);
    values2.push_back(10);
    values2.push_back(2);

    Dimension dimension(Time, values1);
    dimension |= Dimension(Time, values2);

    // 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
    BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
    BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(10));
    BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(1));
    BOOST_CHECK_EQUAL(dimension.nrCoordinates(), size_t(10));
  }

  {
    // Because of bug, only in release mode:
    // dimension1: /cumulative probabilities[0.01, 0.99, 0.01]
    // dimension2: /cumulative probabilities[0.01, 0.99, 0.01]
    // result:     /cumulative probabilities[0.01, 0.99, -2.14748e-07]   <------
    std::vector<float>quantiles;
    quantiles.push_back(0.01f);
    quantiles.push_back(0.99f);
    quantiles.push_back(0.01f);

    Dimension dimension1(CumulativeProbabilities, quantiles);
    Dimension dimension2(CumulativeProbabilities, quantiles);

    dimension1 |= dimension2;
    BOOST_CHECK_EQUAL(dimension1.nrValues(), size_t(3));
    BOOST_CHECK(comparable<float>(dimension1.value<float>(2), 0.01f));
  }

  {
    std::vector<double> values1, values2;

    // 1.1, 3.3
    values1.push_back(1.1);
    values1.push_back(3.3);

    // 2.2, 4.4
    values2.push_back(2.2);
    values2.push_back(4.4);

    //     3.3
    // 1.1 +----------+
    //     |          |
    //     |          |
    //     |          |
    //     +----------+ 2.2
    //              4.4
    SpaceDimensions dimensions1(1.1, 3.3, 4.4, 2.2);

    Dimension dimension(Space, BorderedDiscretisation, dimensions1);

    //     4.5
    // 1.3 +----------+
    //     |          |
    //     |          |
    //     |          |
    //     +----------+ 3.2
    //              3.4
    SpaceDimensions dimensions2(1.3, 4.5, 3.4, 3.2);

    dimension |= Dimension(Space, BorderedDiscretisation, dimensions2);

    SpaceDimensions const& result(dimension.value<SpaceDimensions>(0));
    BOOST_CHECK_CLOSE(result.west(), 1.1, 0.001);
    BOOST_CHECK_CLOSE(result.north(), 4.5, 0.001);
    BOOST_CHECK_CLOSE(result.east(), 4.4, 0.001);
    BOOST_CHECK_CLOSE(result.south(), 2.2, 0.001);
  }
}



void dal::DimensionTest::testIntersect()
{
  bool testIntersectImplemented = false;
  BOOST_WARN(testIntersectImplemented);
}



void dal::DimensionTest::testNrCoordinates()
{
  {
    RasterDimensions dimensions(3, 4, 1.5);
    Dimension dimension(Space, RegularDiscretisation, dimensions);
    BOOST_CHECK_EQUAL(dimension.nrCoordinates(), size_t(12));
  }

  {
    float first = 0.01f;
    float last = 0.99f;
    float interval = 0.01f;

    std::vector<float> values;
    values.push_back(first);
    values.push_back(last);
    values.push_back(interval);
    Dimension dimension(CumulativeProbabilities, values);
    BOOST_CHECK_EQUAL(dimension.nrCoordinates(), size_t(99));
  }
}



void dal::DimensionTest::testIsWide()
{
  {
    Dimension dimension(Time, size_t(5), size_t(5), size_t(1));
    BOOST_CHECK(!dimension.isWide());
  }

  {
    Dimension dimension(Time, size_t(5), size_t(5), size_t(2));
    BOOST_CHECK(!dimension.isWide());
  }

  {
    Dimension dimension(Time, size_t(5), size_t(6), size_t(2));
    BOOST_CHECK(!dimension.isWide());
  }

  {
    Dimension dimension(Time, size_t(5), size_t(7), size_t(2));
    BOOST_CHECK(dimension.isWide());
  }
}


