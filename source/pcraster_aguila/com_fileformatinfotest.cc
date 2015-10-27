#define BOOST_TEST_MODULE pcraster aguila file_format_info
#include <boost/test/unit_test.hpp>
#include "com_fileformatinfo.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace com;

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
