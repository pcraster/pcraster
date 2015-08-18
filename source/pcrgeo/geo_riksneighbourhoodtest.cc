#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RIKSNEIGHBOURHOODTEST
#include "geo_riksneighbourhoodtest.h"
#define INCLUDED_GEO_RIKSNEIGHBOURHOODTEST
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
#ifndef INCLUDED_BOOST_MATH_TR1
#include <boost/math/tr1.hpp>
#define INCLUDED_BOOST_MATH_TR1
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

// Module headers.
#ifndef INCLUDED_GEO_RIKSNEIGHBOURHOOD
#include "geo_riksneighbourhood.h"
#define INCLUDED_GEO_RIKSNEIGHBOURHOOD
#endif



/*!
  \file
  This file contains the implementation of the RiksNeighbourhoodTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RIKSNEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::RiksNeighbourhoodTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RiksNeighbourhoodTest> instance(new RiksNeighbourhoodTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RiksNeighbourhoodTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RIKSNEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::RiksNeighbourhoodTest::RiksNeighbourhoodTest()
{
}



//! setUp
void geo::RiksNeighbourhoodTest::setUp()
{
}



//! tearDown
void geo::RiksNeighbourhoodTest::tearDown()
{
}



void geo::RiksNeighbourhoodTest::test()
{
  {
    // Nr 1.
    RiksNeighbourhood neighbourhood(0.0);
    BOOST_CHECK(neighbourhood.cell(0, 0) == 1.0);
  }

  {
    // Nr 2.
    RiksNeighbourhood neighbourhood(1.0, 1.0);
    BOOST_CHECK(neighbourhood.cell(0, 1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1, 0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1, 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2, 1) == 1.0);
    BOOST_CHECK(neighbourhood.sum() == 4.0);
  }

  {
    // Nr 4.
    RiksNeighbourhood neighbourhood(2.0, 2.0);
    BOOST_CHECK(neighbourhood.cell(0, 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2, 0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2, 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(4, 2) == 1.0);
    BOOST_CHECK(neighbourhood.sum() == 4.0);
  }

  {
    // Nr 13.
    double radius = boost::math::tr1::hypot(4.0, 2.0);
    RiksNeighbourhood neighbourhood(radius, radius);
    size_t offset = neighbourhood.radius();
    BOOST_CHECK(neighbourhood.cell(offset + 4, offset + 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 2, offset + 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 2, offset + 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 4, offset + 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 4, offset - 2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 2, offset - 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 2, offset - 4) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 4, offset - 2) == 1.0);
    BOOST_CHECK(neighbourhood.sum() == 8.0);
  }

  {
    // Nr 30.
    RiksNeighbourhood neighbourhood(8.0, 8.0);
    size_t offset = neighbourhood.radius();
    BOOST_CHECK(neighbourhood.cell(offset + 0, offset + 8) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 8, offset + 0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset - 8, offset + 0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(offset + 0, offset - 8) == 1.0);
    BOOST_CHECK(neighbourhood.sum() == 4.0);
  }
}

