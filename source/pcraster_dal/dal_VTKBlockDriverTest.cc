#define BOOST_TEST_MODULE pcraster dal vtk_block_driver
#include <boost/test/unit_test.hpp>
#include "dal_VTKBlockDriver.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace dal;

  VTKBlockDriver const driver;
  bool const testImplemented = false;
  BOOST_TEST_WARN(testImplemented);
}
