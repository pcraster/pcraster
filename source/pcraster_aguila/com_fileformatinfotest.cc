#include "com_fileformatinfotest.h"

// Library headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// PCRaster library headers.

// Module headers.
#include "com_fileformatinfo.h"



/*!
  \file
  This file contains the implementation of the FileFormatInfoTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FILEFORMATINFO MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::FileFormatInfoTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FileFormatInfoTest> instance(new FileFormatInfoTest());

  suite->add(BOOST_CLASS_TEST_CASE(&FileFormatInfoTest::testConstructor, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF FILEFORMATINFO MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::FileFormatInfoTest::FileFormatInfoTest()
{
}



//! setUp
void com::FileFormatInfoTest::setUp()
{
}

//! tearDown
void com::FileFormatInfoTest::tearDown()
{
}



void com::FileFormatInfoTest::testConstructor()
{
  FileFormatInfo myFileInfo("My file format", "fmt|frm|mtf");

  BOOST_CHECK(myFileInfo.description() == std::string("My file format"));
  BOOST_CHECK(myFileInfo.extension() == std::string("fmt"));

  std::vector<std::string> extensions;
  extensions.push_back(std::string("fmt"));
  extensions.push_back(std::string("frm"));
  extensions.push_back(std::string("mtf"));
  BOOST_CHECK(myFileInfo.extensions() == extensions);

//  BOOST_CHECK(myFileInfo.matchesExtension("frm"));
//
//  BOOST_CHECK(!myFileInfo.matchesExtension("  frm  "));
//  BOOST_CHECK(!myFileInfo.matchesExtension("xxx"));
//  BOOST_CHECK(!myFileInfo.matchesExtension(""));
//  BOOST_CHECK(!myFileInfo.matchesExtension("   "));
//
//  // case sensitive platform depend
//#ifdef WIN32
//  BOOST_CHECK( myFileInfo.matchesExtension("FRM"));
//#else
//  BOOST_CHECK(!myFileInfo.matchesExtension("FRM"));
//#endif
}
