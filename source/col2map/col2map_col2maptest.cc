#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COL2MAP_COL2MAPTEST
#include "col2map_col2maptest.h"
#define INCLUDED_COL2MAP_COL2MAPTEST
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

#include "dal_CSFRasterDriver.h"



/*!
  \file
  This file contains the implementation of the Col2MapTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace col2map {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC COL2MAP MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* Col2MapTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<Col2MapTest> instance(new Col2MapTest());

  suite->add(BOOST_CLASS_TEST_CASE(&Col2MapTest::testNaN, instance));

  //TestSuite *suite = new TestSuite(__FILE__);

  //suite->ADD_TEST(Col2MapTest, testNaN);

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF COL2MAP MEMBERS
//------------------------------------------------------------------------------

//! ctor
Col2MapTest::Col2MapTest()
{
}



//! setUp
void Col2MapTest::setUp()
{
}



//! tearDown
void Col2MapTest::tearDown()
{
}



void Col2MapTest::testNaN()
{
  // http://pcrserver.geo.uu.nl/bugzilla/show_bug.cgi?id=247
  // nan.map is created in testrun.prolog.
  // Test whether a NaN on the input is converted to a missing value on the
  // output.
  // Status not yet correct: also test under DIFFERENT compilers if the file
  // reading already detets a string nan as a NAN, seems not standarized
#ifdef _MSC_VER
  BOOST_WARN_MESSAGE( 0, "MSC does not read Nan (std? or gcc non-std)?");
#else
  dal::CSFRasterDriver driver;
  std::string filename("nan.map");
  dal::Raster* raster = dynamic_cast<dal::RasterDriver&>(driver).read(filename);

  POSTCOND(raster);
  POSTCOND(raster->typeId() == dal::TI_REAL4);

  BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
  BOOST_CHECK_EQUAL(raster->cell<REAL4>(0), 1.0F);
  //cuTodo(pcr::isMV(raster->cell<REAL4>(1)));
  BOOST_WARN(pcr::isMV(raster->cell<REAL4>(1)));
  BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(2)));
  BOOST_CHECK_EQUAL(raster->cell<REAL4>(2), 3.0F);
#endif
}



} // namespace col2map
