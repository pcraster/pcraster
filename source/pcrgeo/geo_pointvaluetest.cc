#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_POINTVALUETEST
#include "geo_pointvaluetest.h"
#define INCLUDED_GEO_POINTVALUETEST
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
  This file contains the implementation of the PointValueTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC POINTVALUE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::PointValueTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PointValueTest> instance(new PointValueTest());

  suite->add(BOOST_CLASS_TEST_CASE(&PointValueTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF POINTVALUE MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::PointValueTest::PointValueTest()
{
}



//! setUp
void geo::PointValueTest::setUp()
{
}



//! tearDown
void geo::PointValueTest::tearDown()
{
}



void geo::PointValueTest::test()
{
  {
    typedef Point<double, 2> Point;
    typedef double Value;

    Point point(2.2, 3.3);
    Value value(5.5);

    PointValue<Point, Value> height(point, value);
    BOOST_CHECK(height.point() == point);
    BOOST_CHECK(height.value() == value);

    point += 8.8;
    height.setPoint(point);
    BOOST_CHECK(height.point() == point);

    value += 9.9;
    height.setValue(value);
    BOOST_CHECK(height.value() == value);
  }
}
