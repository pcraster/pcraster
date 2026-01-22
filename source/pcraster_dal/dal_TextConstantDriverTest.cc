#define BOOST_TEST_MODULE pcraster dal text_constant_driver
#include <boost/test/unit_test.hpp>
#include "dal_Exception.h"
#include "dal_TextConstantDriver.h"


BOOST_AUTO_TEST_CASE(description)
{
  using namespace dal;

  TextConstantDriver const driver;
  BOOST_TEST(driver.description() == "Text constant file format");
}


BOOST_AUTO_TEST_CASE(unexisting)
{
  using namespace dal;

  std::string const filename = "unexisting";
  TextConstantDriver const driver;
  bool exceptionCaught = false;

  // Exists.
  BOOST_TEST(!dynamic_cast<ConstantDriver const&>(driver).exists(filename));

  // Open.
  Constant* constant =
         dynamic_cast<ConstantDriver const&>(driver).open(filename);
  BOOST_TEST(!constant);

  // Read.
  try {
    exceptionCaught = false;
    constant = dynamic_cast<ConstantDriver const&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_TEST(exception.message() ==
       "Data source " + filename + "(constant):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_TEST(exceptionCaught);
}
