#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_COUNTFILTERTEST
#include "geo_countfiltertest.h"
#define INCLUDED_GEO_COUNTFILTERTEST
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
#ifndef INCLUDED_GEO_COUNTFILTER
#include "geo_countfilter.h"
#define INCLUDED_GEO_COUNTFILTER
#endif

#ifndef INCLUDED_GEO_FILTERENGINE
#include "geo_filterengine.h"
#define INCLUDED_GEO_FILTERENGINE
#endif



/*!
  \file
  This file contains the implementation of the CountFilterTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COUNTFILTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::CountFilterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CountFilterTest> instance(new CountFilterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CountFilterTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF COUNTFILTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::CountFilterTest::CountFilterTest()
{
}



//! setUp
void geo::CountFilterTest::setUp()
{
}

//! tearDown
void geo::CountFilterTest::tearDown()
{
}



void geo::CountFilterTest::test()
{
  // Create source raster.
#ifdef __x86_64__
  typedef unsigned int count_t;
#else
  typedef size_t count_t;
#endif
  SimpleRaster<count_t> source(3, 3);
  source.cell(0, 0) = 1;
  source.cell(0, 1) = 2;
  source.cell(0, 2) = 3;
  source.cell(1, 0) = 4;
  source.cell(1, 1) = 5;
  source.cell(1, 2) = 6;
  source.cell(2, 0) = 7;
  source.cell(2, 1) = 8;
  source.cell(2, 2) = 9;

  SimpleRaster<double> weights(3, 3, 1.0);
  CountFilter<count_t, double> filter(weights, 4);
  SimpleRaster<double> destination(3, 3);
  FilterEngine<count_t, double> engine(source, filter, destination);

  engine.calc();

  BOOST_CHECK(destination.cell(0, 0) == 1.0);
  BOOST_CHECK(destination.cell(0, 1) == 1.0);
  BOOST_CHECK(destination.cell(0, 2) == 0.0);
  BOOST_CHECK(destination.cell(1, 0) == 1.0);
  BOOST_CHECK(destination.cell(1, 1) == 1.0);
  BOOST_CHECK(destination.cell(1, 2) == 0.0);
  BOOST_CHECK(destination.cell(2, 0) == 1.0);
  BOOST_CHECK(destination.cell(2, 1) == 1.0);
  BOOST_CHECK(destination.cell(2, 2) == 0.0);

  // see __x86_64__
  // TODO Suse problem: 8 byte isMV template instantion for unsigned long  
  bool size_tRequires8Byteonx86_64=false;
  BOOST_WARN(size_tRequires8Byteonx86_64);
}
