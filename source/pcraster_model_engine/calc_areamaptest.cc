#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_AREAMAPTEST
#include "calc_areamaptest.h"
#define INCLUDED_CALC_AREAMAPTEST
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
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif

// Module headers.
#ifndef INCLUDED_CALC_AREAMAP
#include "calc_areamap.h"
#define INCLUDED_CALC_AREAMAP
#endif



/*!
  \file
  This file contains the implementation of the AreaMapTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace calc {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC AREAMAP MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*AreaMapTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<AreaMapTest> instance(new AreaMapTest());

  suite->add(BOOST_CLASS_TEST_CASE(&AreaMapTest::testInit, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF AREAMAP MEMBERS
//------------------------------------------------------------------------------

//! ctor
AreaMapTest::AreaMapTest(
         )
{
}



//! setUp
void AreaMapTest::setUp()
{
}



//! tearDown
void AreaMapTest::tearDown()
{
}



//! test ctors
void AreaMapTest::testInit()
{
  geo::RasterSpace in(3,10,5);

  AreaMap amIn(in);

  std::auto_ptr<pcrxml::CheckContext> cc(amIn.createXMLContext ());

  AreaMap amOut1(amIn.rasterSpace());

  // not possible anymore pcrxml::AreaMap != pcrxml::AreaMapScript
  // AreaMap amOut2(cc->areaMap().get());

  BOOST_CHECK(in == amOut1.rasterSpace());
  // BOOST_CHECK(in == amOut2.rasterSpace());
}



} // namespace calc

