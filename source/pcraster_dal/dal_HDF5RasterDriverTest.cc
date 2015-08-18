#ifndef INCLUDED_DAL_HDF5RASTERDRIVERTEST
#include "dal_HDF5RasterDriverTest.h"
#define INCLUDED_DAL_HDF5RASTERDRIVERTEST
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
#ifndef INCLUDED_DAL_HDF5RASTERDRIVER
#include "dal_HDF5RasterDriver.h"
#define INCLUDED_DAL_HDF5RASTERDRIVER
#endif



/*!
  \file
  This file contains the implementation of the HDF5RasterDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC HDF5RASTERDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*HDF5RasterDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<HDF5RasterDriverTest> instance(new HDF5RasterDriverTest());

  suite->add(BOOST_CLASS_TEST_CASE(&HDF5RasterDriverTest::testEmptyDataSpace, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF HDF5RASTERDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
HDF5RasterDriverTest::HDF5RasterDriverTest(
         )
{
}



//! setUp
void HDF5RasterDriverTest::setUp()
{
}



//! tearDown
void HDF5RasterDriverTest::tearDown()
{
}



void HDF5RasterDriverTest::testEmptyDataSpace()
{
  HDF5RasterDriver driver;
  std::string name = "test.h5";

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

