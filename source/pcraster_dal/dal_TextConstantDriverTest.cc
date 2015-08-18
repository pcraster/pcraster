#ifndef INCLUDED_DAL_TEXTCONSTANTDRIVERTEST
#include "dal_TextConstantDriverTest.h"
#define INCLUDED_DAL_TEXTCONSTANTDRIVERTEST
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
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_TEXTCONSTANTDRIVER
#include "dal_TextConstantDriver.h"
#define INCLUDED_DAL_TEXTCONSTANTDRIVER
#endif



/*!
  \file
  This file contains the implementation of the TextConstantDriverTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEXTCONSTANTDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* TextConstantDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TextConstantDriverTest> instance(
         new TextConstantDriverTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &TextConstantDriverTest::testDescription, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &TextConstantDriverTest::testUnexisting, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TEXTCONSTANTDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
TextConstantDriverTest::TextConstantDriverTest()
{
}



void TextConstantDriverTest::testDescription()
{
  TextConstantDriver driver;
  BOOST_CHECK_EQUAL(driver.description(), "Text constant file format");
}



void dal::TextConstantDriverTest::testUnexisting()
{
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

} // namespace dal

