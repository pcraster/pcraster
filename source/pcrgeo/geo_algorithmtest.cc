#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_ALGORITHMTEST
#include "geo_algorithmtest.h"
#define INCLUDED_GEO_ALGORITHMTEST
#endif

// Library headers.
#ifndef INCLUDED_ITERATOR
#include <iterator>
#define INCLUDED_ITERATOR
#endif

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
#ifndef INCLUDED_GEO_ALGORITHM
#include "geo_algorithm.h"
#define INCLUDED_GEO_ALGORITHM
#endif

#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif

#ifndef INCLUDED_GEO_POINTVALUE
#include "geo_pointvalue.h"
#define INCLUDED_GEO_POINTVALUE
#endif



/*!
  \file
  This file contains the implementation of the AlgorithmTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ALGORITHM MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::AlgorithmTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<AlgorithmTest> instance(new AlgorithmTest());

  suite->add(BOOST_CLASS_TEST_CASE(&AlgorithmTest::testPointsInArea, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&AlgorithmTest::testMaximum, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ALGORITHM MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::AlgorithmTest::AlgorithmTest()
{
}



//! setUp
void geo::AlgorithmTest::setUp()
{
}



//! tearDown
void geo::AlgorithmTest::tearDown()
{
}



void geo::AlgorithmTest::testPointsInArea()
{
  {
    typedef Point<int, 2> Punt;
    typedef double Value;
    typedef std::vector<PointValue<Punt, Value> > PointValues;

    Value value(0.0);
    PointValues points;
    size_t nrRows = 3;
    size_t nrCols = 3;

    for(size_t row = 0; row < nrRows; ++row) {
      for(size_t col = 0; col < nrCols; ++col) {
        points.push_back(PointValue<Punt, Value>(Punt(row, col), value));
      }
    }

    Punt point(1, 1);
    PointValues subset;
    Punt::CoordinateType radius = 0;

    subset.clear();
    pointsInArea(point, radius, points.begin(), points.end(),
         std::back_inserter(subset));
    BOOST_CHECK(subset.size() == 9);

    radius = 1;
    subset.clear();
    pointsInArea(point, radius, points.begin(), points.end(),
         std::back_inserter(subset));
    BOOST_CHECK(subset.size() == 5);

    radius = 2;
    subset.clear();
    pointsInArea(point, radius, points.begin(), points.end(),
         std::back_inserter(subset));
    BOOST_CHECK(subset.size() == 9);
  }



}



void geo::AlgorithmTest::testMaximum()
{
  {
    typedef Point<int, 2> Punt;
    typedef int Value;
    typedef std::vector<PointValue<Punt, Value> > PointValues;

    PointValues points;
    size_t nrRows = 3;
    size_t nrCols = 3;

    for(size_t row = 0; row < nrRows; ++row) {
      for(size_t col = 0; col < nrCols; ++col) {
        points.push_back(PointValue<Punt, Value>(Punt(row, col),
              row * nrCols + col));
      }
    }

    Punt point(1, 1);
    PointValues subset;
    Punt::CoordinateType radius;
    bool result;
    Value max;

    radius = 0;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 8);

    radius = 1;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 7);

    radius = 2;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 8);
  }

  {
    typedef Point<double, 2> Point;
    typedef int Value;
    typedef std::vector<PointValue<Point, Value> > PointValues;

    PointValues points;
    size_t nrRows = 3;
    size_t nrCols = 3;

    for(size_t row = 0; row < nrRows; ++row) {
      for(size_t col = 0; col < nrCols; ++col) {
        points.push_back(PointValue<Point, Value>(Point(row, col),
              row * nrCols + col));
      }
    }

    Point point(1, 1);
    PointValues subset;
    Point::CoordinateType radius;
    bool result;
    Value max;

    radius = 0;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 8);

    radius = 1;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 7);

    radius = 2;
    subset.clear();
    result = maximum(max, point, radius, points.begin(), points.end());
    BOOST_CHECK(result == true);
    BOOST_CHECK(max == 8);
  }
}
