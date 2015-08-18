#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RASTERBOUNDARIESTEST
#include "geo_rasterboundariestest.h"
#define INCLUDED_GEO_RASTERBOUNDARIESTEST
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
#ifndef INCLUDED_GEO_RASTERBOUNDARIES
#include "geo_rasterboundaries.h"
#define INCLUDED_GEO_RASTERBOUNDARIES
#endif



/*!
  \file
  This file contains the implementation of the RasterBoundariesTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERBOUNDARIES MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::RasterBoundariesTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RasterBoundariesTest> instance(new RasterBoundariesTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RasterBoundariesTest::testBoundaries, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterBoundariesTest::testIndexLeft, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterBoundariesTest::testIndexTop, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterBoundariesTest::testIndexRight, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterBoundariesTest::testIndexBottom, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RASTERBOUNDARIES MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::RasterBoundariesTest::RasterBoundariesTest()
{
}



//! setUp
void geo::RasterBoundariesTest::setUp()
{
}

//! tearDown
void geo::RasterBoundariesTest::tearDown()
{
}



void geo::RasterBoundariesTest::testIndexLeft()
{
  RasterBoundaries<size_t> boundaries(3, 2);
  BOOST_CHECK(boundaries.indexLeft(0, 0) == 2);
  BOOST_CHECK(boundaries.indexLeft(0, 1) == 3);
  BOOST_CHECK(boundaries.indexLeft(1, 0) == 7);
  BOOST_CHECK(boundaries.indexLeft(1, 1) == 8);
  BOOST_CHECK(boundaries.indexLeft(2, 0) == 12);
  BOOST_CHECK(boundaries.indexLeft(2, 1) == 13);
}



void geo::RasterBoundariesTest::testIndexTop()
{
  RasterBoundaries<size_t> boundaries(3, 2);
  BOOST_CHECK(boundaries.indexTop(0, 0) == 0);
  BOOST_CHECK(boundaries.indexTop(0, 1) == 1);
  BOOST_CHECK(boundaries.indexTop(1, 0) == 5);
  BOOST_CHECK(boundaries.indexTop(1, 1) == 6);
  BOOST_CHECK(boundaries.indexTop(2, 0) == 10);
  BOOST_CHECK(boundaries.indexTop(2, 1) == 11);
}



void geo::RasterBoundariesTest::testIndexRight()
{
  RasterBoundaries<size_t> boundaries(3, 2);
  BOOST_CHECK(boundaries.indexRight(0, 0) == 3);
  BOOST_CHECK(boundaries.indexRight(0, 1) == 4);
  BOOST_CHECK(boundaries.indexRight(1, 0) == 8);
  BOOST_CHECK(boundaries.indexRight(1, 1) == 9);
  BOOST_CHECK(boundaries.indexRight(2, 0) == 13);
  BOOST_CHECK(boundaries.indexRight(2, 1) == 14);
}



void geo::RasterBoundariesTest::testIndexBottom()
{
  RasterBoundaries<size_t> boundaries(3, 2);
  BOOST_CHECK(boundaries.indexBottom(0, 0) == 5);
  BOOST_CHECK(boundaries.indexBottom(0, 1) == 6);
  BOOST_CHECK(boundaries.indexBottom(1, 0) == 10);
  BOOST_CHECK(boundaries.indexBottom(1, 1) == 11);
  BOOST_CHECK(boundaries.indexBottom(2, 0) == 15);
  BOOST_CHECK(boundaries.indexBottom(2, 1) == 16);
}



void geo::RasterBoundariesTest::testBoundaries()
{
  RasterBoundaries<size_t> boundaries(3, 2);

  boundaries.leftBoundary(0, 0) = 2;
  boundaries.leftBoundary(0, 1) = 3;
  boundaries.leftBoundary(1, 0) = 7;
  boundaries.leftBoundary(1, 1) = 8;
  boundaries.leftBoundary(2, 0) = 12;
  boundaries.leftBoundary(2, 1) = 13;

  boundaries.rightBoundary(0, 1) = 4;
  boundaries.rightBoundary(1, 1) = 9;
  boundaries.rightBoundary(2, 1) = 14;

  boundaries.topBoundary(0, 0) = 0;
  boundaries.topBoundary(0, 1) = 1;
  boundaries.topBoundary(1, 0) = 5;
  boundaries.topBoundary(1, 1) = 6;
  boundaries.topBoundary(2, 0) = 10;
  boundaries.topBoundary(2, 1) = 11;

  boundaries.bottomBoundary(2, 0) = 15;
  boundaries.bottomBoundary(2, 1) = 16;

  BOOST_CHECK(boundaries.leftBoundary(0, 0) == 2);
  BOOST_CHECK(boundaries.topBoundary(0, 0) == 0);
  BOOST_CHECK(boundaries.rightBoundary(0, 0) == 3);
  BOOST_CHECK(boundaries.bottomBoundary(0, 0) == 5);

  BOOST_CHECK(boundaries.leftBoundary(1, 1) == 8);
  BOOST_CHECK(boundaries.topBoundary(1, 1) == 6);
  BOOST_CHECK(boundaries.rightBoundary(1, 1) == 9);
  BOOST_CHECK(boundaries.bottomBoundary(1, 1) == 11);

  BOOST_CHECK(boundaries.rightBoundary(2, 0) == boundaries.leftBoundary(2, 1));
  BOOST_CHECK(boundaries.topBoundary(2, 0) == boundaries.bottomBoundary(1, 0));
}

