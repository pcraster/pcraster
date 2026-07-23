#define BOOST_TEST_MODULE pcraster aguila file_format_info
#include <boost/test/unit_test.hpp>
#include "com_fileformatinfo.h"

BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace com;

  FileFormatInfo const myFileInfo("My file format", "fmt|frm|mtf");

  BOOST_TEST(myFileInfo.description() == std::string("My file format"));
  BOOST_TEST(myFileInfo.extension() == std::string("fmt"));

  std::vector<std::string> extensions;
  extensions.emplace_back("fmt");
  extensions.emplace_back("frm");
  extensions.emplace_back("mtf");
  BOOST_TEST(myFileInfo.extensions() == extensions);

  //  BOOST_TEST(myFileInfo.matchesExtension("frm"));
  //
  //  BOOST_TEST(!myFileInfo.matchesExtension("  frm  "));
  //  BOOST_TEST(!myFileInfo.matchesExtension("xxx"));
  //  BOOST_TEST(!myFileInfo.matchesExtension(""));
  //  BOOST_TEST(!myFileInfo.matchesExtension("   "));
  //
  //  // case sensitive platform depend
  //#ifdef WIN32
  //  BOOST_TEST( myFileInfo.matchesExtension("FRM"));
  //#else
  //  BOOST_TEST(!myFileInfo.matchesExtension("FRM"));
  //#endif
}
