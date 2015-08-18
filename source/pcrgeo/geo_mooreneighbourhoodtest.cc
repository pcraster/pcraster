#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_MOORENEIGHBOURHOODTEST
#include "geo_mooreneighbourhoodtest.h"
#define INCLUDED_GEO_MOORENEIGHBOURHOODTEST
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
#ifndef INCLUDED_GEO_MOORENEIGHBOURHOOD
#include "geo_mooreneighbourhood.h"
#define INCLUDED_GEO_MOORENEIGHBOURHOOD
#endif



/*!
  \file
  This file contains the implementation of the MooreNeighbourhoodTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MOORENEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::MooreNeighbourhoodTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MooreNeighbourhoodTest> instance(new MooreNeighbourhoodTest());

  suite->add(BOOST_CLASS_TEST_CASE(&MooreNeighbourhoodTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MOORENEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::MooreNeighbourhoodTest::MooreNeighbourhoodTest()
{
}



//! setUp
void geo::MooreNeighbourhoodTest::setUp()
{
}

//! tearDown
void geo::MooreNeighbourhoodTest::tearDown()
{
}



void geo::MooreNeighbourhoodTest::test()
{
  {
    MooreNeighbourhood neighbourhood(1.0);
    BOOST_CHECK(neighbourhood.cell(0,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(0,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(0,2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,2) == 1.0);
  }

  {
    MooreNeighbourhood neighbourhood(0.0, 1.0);
    BOOST_CHECK(neighbourhood.cell(0,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(0,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(0,2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,2) == 1.0);
  }

  {
    MooreNeighbourhood neighbourhood(1.0, 1.0);
    BOOST_CHECK(neighbourhood.cell(0,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(0,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(0,2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(1,1) == 0.0);
    BOOST_CHECK(neighbourhood.cell(1,2) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,0) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,1) == 1.0);
    BOOST_CHECK(neighbourhood.cell(2,2) == 1.0);
  }
}
