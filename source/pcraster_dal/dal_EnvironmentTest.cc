#ifndef INCLUDED_DAL_ENVIRONMENTTEST
#include "dal_EnvironmentTest.h"
#define INCLUDED_DAL_ENVIRONMENTTEST
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
#ifndef INCLUDED_DEV_UTILS
#include "dev_Utils.h"
#define INCLUDED_DEV_UTILS
#endif

// Module headers.
#ifndef INCLUDED_DAL_ENVIRONMENT
#include "dal_Environment.h"
#define INCLUDED_DAL_ENVIRONMENT
#endif



/*!
  \file
  This file contains the implementation of the EnvironmentTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ENVIRONMENTTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* EnvironmentTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<EnvironmentTest> instance(
         new EnvironmentTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &EnvironmentTest::testDalFormats, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &EnvironmentTest::testGdalData, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ENVIRONMENTTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
EnvironmentTest::EnvironmentTest()
{
}



void EnvironmentTest::testDalFormats()
{
#ifdef _WIN32
  BOOST_WARN_MESSAGE(false, "Environment handling does not work on Windows");
#else
  {
    dev::unsetEnvironmentVariable("PCRASTER_DAL_FORMATS");
    BOOST_CHECK(!dev::environmentVariableSet("PCRASTER_DAL_FORMATS"));
    Environment environment("/usr/bin/dal");
    BOOST_CHECK(environment.formatNames().empty());
  }

  {
    dev::setEnvironmentVariable("PCRASTER_DAL_FORMATS", "PCRaster");
    BOOST_CHECK(dev::environmentVariableSet("PCRASTER_DAL_FORMATS"));
    Environment environment("/usr/bin/dal");
    BOOST_REQUIRE_EQUAL(environment.formatNames().size(), size_t(1));
    BOOST_CHECK_EQUAL(environment.formatNames()[0], "PCRaster");
  }

  {
    dev::setEnvironmentVariable("PCRASTER_DAL_FORMATS",
         " PCRaster, HDF4, , ESRI Shapefile , HDF4");
    Environment environment("/usr/bin/dal");
    BOOST_REQUIRE_EQUAL(environment.formatNames().size(), size_t(3));
    BOOST_CHECK_EQUAL(environment.formatNames()[0], "PCRaster");
    BOOST_CHECK_EQUAL(environment.formatNames()[1], "HDF4");
    BOOST_CHECK_EQUAL(environment.formatNames()[2], "ESRI Shapefile");
  }
#endif
}



void EnvironmentTest::testGdalData()
{
#ifdef _WIN32
  BOOST_WARN_MESSAGE(false, "Environment handling does not work on Windows");
#else
  // Set variable if it is not already set.
  {
    dev::unsetEnvironmentVariable("GDAL_DATA");
    Environment environment("/usr");
    BOOST_CHECK_EQUAL(environment.gdalData(), "/usr/share/gdal");
  }

  // Don't touch variable if it is already set.
  {
    dev::setEnvironmentVariable("GDAL_DATA", "");
    Environment environment("/usr");
    BOOST_CHECK_EQUAL(environment.gdalData(), "");
  }

  // Don't touch variable if it is already set.
  {
    dev::setEnvironmentVariable("GDAL_DATA", " ");
    Environment environment("/usr");
    BOOST_CHECK_EQUAL(environment.gdalData(), std::string(" "));
  }
#endif
}

} // namespace dal

