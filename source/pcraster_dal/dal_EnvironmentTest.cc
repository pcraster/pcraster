#define BOOST_TEST_MODULE pcraster dal environment
#include <boost/test/unit_test.hpp>
#include "dev_Utils.h"
#include "dal_Environment.h"


BOOST_AUTO_TEST_CASE(dal_formats)
{
  using namespace dal;

#ifdef _WIN32
  BOOST_WARN_MESSAGE(false, "Environment handling does not work on Windows");
#else
  {
    dev::unsetEnvironmentVariable("PCRASTER_DAL_FORMATS");
    BOOST_TEST(!dev::environmentVariableSet("PCRASTER_DAL_FORMATS"));
    Environment const environment("/usr/bin/dal");
    BOOST_TEST(environment.formatNames().empty());
  }

  {
    dev::setEnvironmentVariable("PCRASTER_DAL_FORMATS", "PCRaster");
    BOOST_TEST(dev::environmentVariableSet("PCRASTER_DAL_FORMATS"));
    Environment const environment("/usr/bin/dal");
    BOOST_REQUIRE_EQUAL(environment.formatNames().size(), size_t(1));
    BOOST_CHECK_EQUAL(environment.formatNames()[0], "PCRaster");
  }

  {
    dev::setEnvironmentVariable("PCRASTER_DAL_FORMATS",
         " PCRaster, HDF4, , ESRI Shapefile , HDF4");
    Environment const environment("/usr/bin/dal");
    BOOST_REQUIRE_EQUAL(environment.formatNames().size(), size_t(3));
    BOOST_CHECK_EQUAL(environment.formatNames()[0], "PCRaster");
    BOOST_CHECK_EQUAL(environment.formatNames()[1], "HDF4");
    BOOST_CHECK_EQUAL(environment.formatNames()[2], "ESRI Shapefile");
  }
#endif
}


BOOST_AUTO_TEST_CASE(gdal_data)
{
  using namespace dal;

#ifdef _WIN32
  BOOST_WARN_MESSAGE(false, "Environment handling does not work on Windows");
#else
  // Set variable if it is not already set.
  {
    dev::unsetEnvironmentVariable("GDAL_DATA");
    Environment const environment("/usr");
    BOOST_CHECK_EQUAL(environment.gdalData(), "/usr/share/gdal");
  }

  // Don't touch variable if it is already set.
  {
    dev::setEnvironmentVariable("GDAL_DATA", "");
    Environment const environment("/usr");
    BOOST_CHECK_EQUAL(environment.gdalData(), "");
  }

  // Don't touch variable if it is already set.
  {
    dev::setEnvironmentVariable("GDAL_DATA", " ");
    Environment const environment("/usr");
    BOOST_CHECK_EQUAL(environment.gdalData(), std::string(" "));
  }
#endif
}
