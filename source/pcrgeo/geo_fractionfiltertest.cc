#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_FRACTIONFILTERTEST
#include "geo_fractionfiltertest.h"
#define INCLUDED_GEO_FRACTIONFILTERTEST
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
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.
#ifndef INCLUDED_GEO_MOORENEIGHBOURHOOD
#include "geo_mooreneighbourhood.h"
#define INCLUDED_GEO_MOORENEIGHBOURHOOD
#endif

#ifndef INCLUDED_GEO_FILTERENGINE
#include "geo_filterengine.h"
#define INCLUDED_GEO_FILTERENGINE
#endif

#ifndef INCLUDED_GEO_FRACTIONFILTER
#include "geo_fractionfilter.h"
#define INCLUDED_GEO_FRACTIONFILTER
#endif



/*!
  \file
  This file contains the implementation of the FractionFilterTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace geo {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC FRACTIONFILTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*FractionFilterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FractionFilterTest> instance(new FractionFilterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&FractionFilterTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF FRACTIONFILTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
FractionFilterTest::FractionFilterTest(
         )
{
}



//! setUp
void FractionFilterTest::setUp()
{
}



//! tearDown
void FractionFilterTest::tearDown()
{
}



void FractionFilterTest::test()
{
  // Fraction of true cells in a filter.

  // 1 0 0
  // 0 1 MV
  // 0 0 1
  SimpleRaster<UINT1> source(3, 3);
  source.cell(0) = 1;
  source.cell(1) = 0;
  source.cell(2) = 0;
  source.cell(3) = 0;
  source.cell(4) = 1;
  source.setMV(5);
  source.cell(6) = 0;
  source.cell(7) = 0;
  source.cell(8) = 1;

  SquareNeighbourhood weights(1);
  FractionFilter<UINT1> filter(weights, 1);

  // 2/4 2/5 1/3
  // 2/6 3/8 MV
  // 1/4 2/5 2/3
  SimpleRaster<REAL8> destination(3, 3);

  FilterEngine<UINT1, REAL8> engine(source, filter, destination);
  engine.calc();

  BOOST_CHECK(dal::comparable(destination.cell(0), 2.0 / 4.0));
  BOOST_CHECK(dal::comparable(destination.cell(1), 2.0 / 5.0));
  BOOST_CHECK(dal::comparable(destination.cell(2), 1.0 / 3.0));
  BOOST_CHECK(dal::comparable(destination.cell(3), 2.0 / 6.0));
  BOOST_CHECK(dal::comparable(destination.cell(4), 3.0 / 8.0));
  BOOST_CHECK(destination.isMV(5));
  BOOST_CHECK(dal::comparable(destination.cell(6), 1.0 / 4.0));
  BOOST_CHECK(dal::comparable(destination.cell(7), 2.0 / 5.0));
  BOOST_CHECK(dal::comparable(destination.cell(8), 2.0 / 3.0));
}



} // namespace geo

