#define BOOST_TEST_MODULE pcraster dal text_constant_driver
#include <boost/test/unit_test.hpp>
#include "dal_Exception.h"
#include "dal_TextConstantDriver.h"


BOOST_AUTO_TEST_CASE(description)
{
  using namespace dal;

  TextConstantDriver driver;
  BOOST_CHECK_EQUAL(driver.description(), "Text constant file format");
}


BOOST_AUTO_TEST_CASE(unexisting)
{
  using namespace dal;

  std::string filename = "unexisting";
  TextConstantDriver driver;
  bool exceptionCaught;

  // Exists.
  BOOST_CHECK(!dynamic_cast<ConstantDriver const&>(driver).exists(filename));

  // Open.
  Constant* constant =
         dynamic_cast<ConstantDriver const&>(driver).open(filename);
  BOOST_CHECK(!constant);

  // Read.
  try {
    exceptionCaught = false;
    constant = dynamic_cast<ConstantDriver const&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_CHECK_EQUAL(exception.message(),
       "Data source " + filename + "(constant):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
}
