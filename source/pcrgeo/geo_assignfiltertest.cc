#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_ASSIGNFILTERTEST
#include "geo_assignfiltertest.h"
#define INCLUDED_GEO_ASSIGNFILTERTEST
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
#ifndef INCLUDED_GEO_ASSIGNFILTER
#include "geo_assignfilter.h"
#define INCLUDED_GEO_ASSIGNFILTER
#endif

#ifndef INCLUDED_GEO_FILTERENGINE
#include "geo_filterengine.h"
#define INCLUDED_GEO_FILTERENGINE
#endif



/*!
  \file
  This file contains the implementation of the AssignFilterTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASSIGNFILTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::AssignFilterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<AssignFilterTest> instance(new AssignFilterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&AssignFilterTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ASSIGNFILTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::AssignFilterTest::AssignFilterTest()
{
}



//! setUp
void geo::AssignFilterTest::setUp()
{
}



//! tearDown
void geo::AssignFilterTest::tearDown()
{
}



void geo::AssignFilterTest::test()
{
  // Create filter. Weight matrix won't be used.
  SimpleRaster<double> weights(5, 5, 1.0);
  AssignFilter filter(weights);

  // Create source raster.
  SimpleRaster<int> source(5, 5);
  source.cell(0, 0) = 1;
  source.cell(0, 1) = 2;
  source.cell(0, 2) = 3;
  source.cell(0, 3) = 4;
  source.cell(0, 4) = 5;
  source.cell(1, 0) = 6;
  source.cell(1, 1) = 7;
  source.cell(1, 2) = 8;
  source.cell(1, 3) = 9;
  source.cell(1, 4) = 10;
  source.cell(2, 0) = 11;
  source.cell(2, 1) = 12;
  source.cell(2, 2) = 13;
  source.cell(2, 3) = 14;
  source.cell(2, 4) = 15;
  source.cell(3, 0) = 16;
  source.cell(3, 1) = 17;
  source.cell(3, 2) = 18;
  source.cell(3, 3) = 19;
  source.cell(3, 4) = 20;
  source.cell(4, 0) = 21;
  source.cell(4, 1) = 22;
  source.cell(4, 2) = 23;
  source.cell(4, 3) = 24;
  source.cell(4, 4) = 25;

  // Destination raster.
  SimpleRaster<int> destination(5, 5);

  FilterEngine<int, int> engine(source, filter, destination);
  engine.calc();

  BOOST_CHECK(source == destination);
}
