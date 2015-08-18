#ifndef INCLUDED_DAL_RESAMPLETEST
#include "dal_ResampleTest.h"
#define INCLUDED_DAL_RESAMPLETEST
#endif

// External headers.
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

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_RESAMPLE
#include "dal_Resample.h"
#define INCLUDED_DAL_RESAMPLE
#endif



/*!
  \file
  This file contains the implementation of the ResampleTest class.
*/

namespace {

} // Anonymous namespace



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RESAMPLETEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* ResampleTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ResampleTest> instance(
         new ResampleTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &ResampleTest::testOverlap, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RESAMPLETEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
ResampleTest::ResampleTest()
{
}



void ResampleTest::testOverlap()
{
  // resample::Resampler<resample::Average> resampler;

  // {
  //   Raster raster1(3, 4, 5.5, 100.0, 200.0, TI_REAL4);
  //   Raster raster2(3, 4, 5.5, 100.0, 200.0, TI_REAL4);


  //   boost::tie(sourceOverlap, destinationOverlap) = resampler.overlap(raster1, raster2);
  // }


  // for(size_t i = 0; i < raster

  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}

} // namespace dal

