#ifndef INCLUDED_DAL_RASTERDALTEST
#include "dal_RasterDalTest.h"
#define INCLUDED_DAL_RASTERDALTEST
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
#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif

#ifndef INCLUDED_DAL_RASTERDAL
#include "dal_RasterDal.h"
#define INCLUDED_DAL_RASTERDAL
#endif



/*!
  \file
  This file contains the implementation of the RasterDalTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERDAL MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*RasterDalTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RasterDalTest> instance(new RasterDalTest());

  suite->add(BOOST_CLASS_TEST_CASE(
         &RasterDalTest::testSupportedDrivers, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &RasterDalTest::testESRIASCIIGrid1, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &RasterDalTest::testHDF4Image1, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RASTERDAL MEMBERS
//------------------------------------------------------------------------------

//! ctor
RasterDalTest::RasterDalTest(
         )
{
}



//! setUp
void RasterDalTest::setUp()
{
}



//! tearDown
void RasterDalTest::tearDown()
{
}



void RasterDalTest::testSupportedDrivers()
{
  RasterDal dal(true);

  BOOST_CHECK(!dal.driverByName("NoSuchDriver"));

  // Add the drivers needed by client code.
  BOOST_CHECK(dal.driverByName("CSF"));
  BOOST_CHECK(!dal.driverByName("PCRaster"));

// #ifdef WIN32
//   BOOST_WARN(dal.driverByName("HDF4Image"));
//   BOOST_WARN(dal.driverByName("HDF4"));
// #else
//   BOOST_CHECK(dal.driverByName("HDF4Image"));
//   BOOST_CHECK(dal.driverByName("HDF4"));
// #endif
}



void RasterDalTest::testESRIASCIIGrid1()
{
  std::string filename = "esriasciigrid1.asc";
  RasterDal dal(true);
  boost::shared_ptr<Raster> raster;

  {
    boost::tie(raster, boost::tuples::ignore) = dal.open(filename);
    BOOST_REQUIRE(raster);
    BOOST_CHECK_EQUAL(raster->nrRows(), size_t(4));
    BOOST_CHECK_EQUAL(raster->nrCols(), size_t(3));
    BOOST_CHECK_EQUAL(raster->cellSize(), 10.0);
    BOOST_CHECK_EQUAL(raster->west(), 3.0);
    BOOST_CHECK_EQUAL(raster->south(), 4.0);
    BOOST_CHECK_EQUAL(raster->typeId(), TI_INT4);
  }

  {
    raster = dal.read(filename, TI_INT4);
    BOOST_REQUIRE(raster);
    INT4 const* cells = static_cast<INT4 const*>(raster->cells());
    BOOST_REQUIRE(cells);

    BOOST_CHECK_EQUAL(cells[0], 1);
    BOOST_CHECK_EQUAL(cells[1], 2);
    BOOST_CHECK_EQUAL(cells[2], 3);
    BOOST_CHECK_EQUAL(cells[3], 4);
    BOOST_CHECK(pcr::isMV(cells[4]));
    BOOST_CHECK_EQUAL(cells[5], 6);
    BOOST_CHECK_EQUAL(cells[6], 7);
    BOOST_CHECK_EQUAL(cells[7], 8);
    BOOST_CHECK_EQUAL(cells[8], 9);
    BOOST_CHECK_EQUAL(cells[9], 10);
    BOOST_CHECK_EQUAL(cells[10], 11);
    BOOST_CHECK_EQUAL(cells[11], 12);
  }
}



void RasterDalTest::testHDF4Image1()
{
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}



} // namespace dal

