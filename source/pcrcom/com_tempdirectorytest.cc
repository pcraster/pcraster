#define BOOST_TEST_MODULE pcraster com temp_directory
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_tempdirectory.h"
#include "com_exception.h"

#include <filesystem>
#include <fstream>

namespace fs = std::filesystem;

BOOST_AUTO_TEST_CASE(constructor_destructor)
{
  using namespace com;

  {  // as empty dir
    TempDirectory td("pcrcalcSwap");
    BOOST_TEST(fs::exists(td.name()));
    td.remove();
    BOOST_TEST(!fs::exists(td.name()));
  }
  {  // dtor remove
    fs::path dir;
    {
      TempDirectory const td("pcrcalcSwap");
      dir = td.name();
      BOOST_TEST(fs::exists(dir));
    }
    BOOST_TEST(!fs::exists(dir));
  }
  {  // as non-empty dir
    TempDirectory td("pcrcalcSwap");
    BOOST_TEST(fs::exists(td.name()));

    fs::path const pete = td.memberPath("pete");
    fs::create_directory(pete);
    BOOST_TEST(fs::exists(pete));

    td.remove();
    BOOST_TEST(!fs::exists(td.name()));
  }
}

BOOST_AUTO_TEST_CASE(remove_failure)
{
  using namespace com;

  TempDirectory td("pcrcalcSwap");
  fs::path const toOpenForWriting = td.memberPath("toOpenForWriting");

  std::ofstream bofs{toOpenForWriting};

  BOOST_TEST(bofs.is_open());

#ifdef WIN32
  bool catched = false;
#endif
  try {
    td.remove();
  } catch (const com::Exception &e) {
    BOOST_TEST(e.messages().find("pcrcalcSwap") != std::string::npos);
#ifdef WIN32
    catched = true;
#endif
  }
#ifdef WIN32
  // linux just throws the file away, should do something
  // with chmod I think, to simulate this error
  BOOST_TEST(catched);
#endif
}
