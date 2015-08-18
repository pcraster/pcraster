#ifndef INCLUDED_DAL_NETCDFRASTERDRIVERTEST
#include "dal_NetCDFRasterDriverTest.h"
#define INCLUDED_DAL_NETCDFRASTERDRIVERTEST
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
#ifndef INCLUDED_DAL_NETCDFRASTERDRIVER
#include "dal_NetCDFRasterDriver.h"
#define INCLUDED_DAL_NETCDFRASTERDRIVER
#endif



/*!
  \file
  This file contains the implementation of the NetCDFRasterDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC NETCDFRASTERDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*NetCDFRasterDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<NetCDFRasterDriverTest> instance(new NetCDFRasterDriverTest());

  suite->add(BOOST_CLASS_TEST_CASE(&NetCDFRasterDriverTest::testDefaultExtension, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&NetCDFRasterDriverTest::testEmptyDataSpace, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF NETCDFRASTERDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
NetCDFRasterDriverTest::NetCDFRasterDriverTest(
         )
{
}



//! setUp
void NetCDFRasterDriverTest::setUp()
{
}



//! tearDown
void NetCDFRasterDriverTest::tearDown()
{
}



void NetCDFRasterDriverTest::testDefaultExtension()
{
  NetCDFRasterDriver driver;
  BOOST_CHECK_EQUAL(driver.format().name(), "NetCDF");
  BOOST_CHECK_EQUAL(driver.format().description(), "NetCDF raster file format");
  BOOST_CHECK_EQUAL(driver.format().datasetType(), RASTER);
  BOOST_CHECK(driver.format().isFileBased());
  BOOST_CHECK_EQUAL(driver.format().extension(), "nc");
}



void NetCDFRasterDriverTest::testEmptyDataSpace()
{
  NetCDFRasterDriver driver;
  std::string name = "test.nc";

  // Create raster.
  Raster raster(2, 3, 5.0, 0.0, 0.0, TI_INT4);
  /*
  pcr::INT4* cells = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
  pcr::setMV(cells[2]);

  // Write a single raster.
  driver.remove(name, DataSpace());
  BOOST_CHECK(!driver.exists(name));
  BOOST_CHECK(!driver.open(name));
  driver.write(raster, DataSpace(), DataSpaceAddress(), name)
  BOOST_CHECK(driver.exists(name, DataSpace(), DataSpaceAddress()));
  BOOST_CHECK(driver.open(name, DataSpace(), DataSpaceAddress()));
  */

/*
  // Get and check dataSpace.
  DataSpace space(driver.dataSpace(name, DataSpace(), DataSpaceAddress()));
  BOOST_CHECK_EQUAL(space.rank(), 2);
  ...

  // Read the raster, check whether values are correct.
  Raster* result = driver.read(name, DataSpace(), DataSpaceAddress());
  ...

  // Change some values and write raster to same dataset again. Read and check
  // values.
  ...

  driver.write(raster, DataSpace(), DataSpaceAddress(), name)
  driver.read(raster, name, DataSpace(), DataSpaceAddress());
  ...

  // Write a raster with different properties to the same dataset. Read and
  // check values.
  ...
*/
}

} // namespace dal

