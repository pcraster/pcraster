#ifndef INCLUDED_DAL_TEXTFILEDRIVERTEST
#include "dal_TextFileDriverTest.h"
#define INCLUDED_DAL_TEXTFILEDRIVERTEST
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
#ifndef INCLUDED_DAL_TEXTFILEDRIVER
#include "dal_TextFileDriver.h"
#define INCLUDED_DAL_TEXTFILEDRIVER
#endif



/*!
  \file
  This file contains the implementation of the TextFileDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEXTFILEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*TextFileDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TextFileDriverTest> instance(new TextFileDriverTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TextFileDriverTest::testDetermineTypeId, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TEXTFILEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! ctor
TextFileDriverTest::TextFileDriverTest(
         )
{
}



//! setUp
void TextFileDriverTest::setUp()
{
}



//! tearDown
void TextFileDriverTest::tearDown()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Idea came from aguila dal/testdata/int2mv0.hdr command. We need
             to implement StringType class and update Types class to support
             string values (see constructor).
*/
void TextFileDriverTest::testDetermineTypeId()
{
  TextFileDriver driver;

  {
    std::vector<std::string> row;
    row.push_back("NROWS");

    TypeId typeId = TI_NR_TYPES;
    driver.determineTypeId(row, typeId);
    BOOST_WARN_EQUAL(typeId, TI_STRING);
  }

  {
    std::vector<std::string> row;
    row.push_back("NROWS");
    row.push_back("4");

    TypeId typeId = TI_NR_TYPES;
    driver.determineTypeId(row, typeId);
    BOOST_WARN_EQUAL(typeId, TI_STRING);
  }
}

} // namespace dal
