#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CIRCULARNEIGHBOURHOODTEST
#include "geo_circularneighbourhoodtest.h"
#define INCLUDED_GEO_CIRCULARNEIGHBOURHOODTEST
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
#ifndef INCLUDED_GEO_CIRCULARNEIGHBOURHOOD
#include "geo_circularneighbourhood.h"
#define INCLUDED_GEO_CIRCULARNEIGHBOURHOOD
#endif



/*!
  \file
  This file contains the implementation of the CircularNeighbourhoodTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CIRCULARNEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::CircularNeighbourhoodTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CircularNeighbourhoodTest> instance(new CircularNeighbourhoodTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CircularNeighbourhoodTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CIRCULARNEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::CircularNeighbourhoodTest::CircularNeighbourhoodTest()
{
}



//! setUp
void geo::CircularNeighbourhoodTest::setUp()
{
}

//! tearDown
void geo::CircularNeighbourhoodTest::tearDown()
{
}



void geo::CircularNeighbourhoodTest::test()
{
  {
    CircularNeighbourhood neighbourhood(1.0);
    BOOST_CHECK(neighbourhood.cell(0,0) == 0.0);
    BOOST_CHECK(neighbourhood.cell(0,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(0,2) == 0.0);
    BOOST_CHECK(neighbourhood.cell(1,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,0) == 0.0);
    BOOST_CHECK(neighbourhood.cell(2,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,2) == 0.0);
  }
}
