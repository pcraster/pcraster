#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_SIMPLERASTERTEST
#include "geo_simplerastertest.h"
#define INCLUDED_GEO_SIMPLERASTERTEST
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



/*!
  \file
  This file contains the implementation of the SimpleRasterTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*geo::SimpleRasterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SimpleRasterTest> instance(new SimpleRasterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&SimpleRasterTest::testProperties, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&SimpleRasterTest::testContents, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&SimpleRasterTest::testAssignment, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

geo::SimpleRasterTest::SimpleRasterTest()
{
}



void geo::SimpleRasterTest::setUp()
{
  // Empty raster.
  d_raster1 = new SimpleRaster<int>(0, 0);

  // 5 x 5 raster with cell values 8.
  d_raster2 = new SimpleRaster<int>(5, 5, 8);

  // 5 x 5 raster with undefined cell values.
  d_raster3 = new SimpleRaster<int>(5, 5);
}



void geo::SimpleRasterTest::tearDown()
{
  delete d_raster3;
  delete d_raster2;
  delete d_raster1;
}



void geo::SimpleRasterTest::testProperties()
{
  setUp();
  BOOST_CHECK(d_raster1->nrRows() == 0);
  BOOST_CHECK(d_raster1->nrCols() == 0);
  BOOST_CHECK(d_raster1->nrCells() == 0);

  BOOST_CHECK(d_raster2->nrRows() == 5);
  BOOST_CHECK(d_raster2->nrCols() == 5);
  BOOST_CHECK(d_raster2->nrCells() == 25);
  tearDown();
}



void geo::SimpleRasterTest::testContents()
{
  setUp();
  BOOST_CHECK(d_raster2->end() - d_raster2->begin() ==
                   static_cast<int>(d_raster2->nrCells()));

  for(SimpleRaster<int>::const_iterator it = d_raster2->begin();
                   it != d_raster2->end(); ++it) {
    BOOST_CHECK(*it == 8);
  }

  for(size_t r = 0; r < d_raster2->nrRows(); ++r) {
    for(size_t c = 0; c < d_raster2->nrCols(); ++c) {
      BOOST_CHECK(d_raster2->cell(r, c) == 8);
    }
  }

  for(size_t r = 0; r < d_raster3->nrRows(); ++r) {
    for(size_t c = 0; c < d_raster3->nrCols(); ++c) {
      d_raster3->cell(r, c) = (r + 1) * (c + 1);
    }
  }

  for(size_t r = 0; r < d_raster3->nrRows(); ++r) {
    for(size_t c = 0; c < d_raster3->nrCols(); ++c) {
      BOOST_CHECK(d_raster3->cell(r, c) == static_cast<int>((r + 1) * (c + 1)));
    }
  }
  tearDown();
}



void geo::SimpleRasterTest::testAssignment()
{
  setUp();
  SimpleRaster<int>::const_iterator it2, it3;

  bool result = true;

  for(it2 = d_raster2->begin(), it3 = d_raster3->begin();
                   it2 != d_raster2->end() && it3 != d_raster3->end();
                   ++it2, ++it3) {
    if(*it2 != *it3) {
      result = false;
      break;
    }
  }

  // The rasters should be different.
  BOOST_CHECK(!result);

  *d_raster2 = *d_raster3;
  result = true;

  for(it2 = d_raster2->begin(), it3 = d_raster3->begin();
                   it2 != d_raster2->end() && it3 != d_raster3->end();
                   ++it2, ++it3) {
    if(*it2 != *it3) {
      result = false;
      break;
    }
  }

  // The rasters should be equal.
  BOOST_CHECK(result);
  tearDown();
}




