// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

#ifndef INCLUDED_PCXSD_UTILSTEST
#include "pcrxsd_utilstest.h"
#define INCLUDED_PCXSD_UTILSTEST
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
#ifndef INCLUDED_PCRXSD_UTILS
#include "pcrxsd_utils.h"
#define INCLUDED_PCRXSD_UTILS
#endif


/*!
  \file
  This file contains the implementation of the UtilsTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace pcrxsd {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC UTILS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*UtilsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<UtilsTest> instance(new UtilsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&UtilsTest::testContentsIsXMLOrPCRasterFileFormat, instance));

  return suite;
}

//------------------------------------------------------------------------------
// DEFINITION OF UTILS MEMBERS
//------------------------------------------------------------------------------

//! ctor
UtilsTest::UtilsTest(
         )
{
}



//! setUp
void UtilsTest::setUp()
{
}



//! tearDown
void UtilsTest::tearDown()
{
}



void UtilsTest::testContentsIsXMLOrPCRasterFileFormat()
{
  BOOST_CHECK(contentsIsXMLOrPCRasterFileFormat("<?pi ?> \n <empty/>")=="empty");
  BOOST_CHECK(contentsIsXMLOrPCRasterFileFormat("\n<?pi ?> \n <empty/>")=="empty");
  BOOST_CHECK(contentsIsXMLOrPCRasterFileFormat("empty").empty());
  BOOST_CHECK(contentsIsXMLOrPCRasterFileFormat("< >").empty());
}



} // namespace pcrxsd

