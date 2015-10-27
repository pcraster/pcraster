#define BOOST_TEST_MODULE pcraster com temp_directory
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "boost/filesystem/path.hpp"
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/fstream.hpp"
#include "com_tempdirectory.h"
#include "com_exception.h"


namespace fs=boost::filesystem;


BOOST_AUTO_TEST_CASE(constructor_destructor)
{
  using namespace com;

 { // as empty dir
  TempDirectory td("pcrcalcSwap");
  BOOST_CHECK(fs::exists(td.name()));
  td.remove();
  BOOST_CHECK(!fs::exists(td.name()));
 }
 { // dtor remove
   fs::path dir;
   {
    TempDirectory td("pcrcalcSwap");
    dir=td.name();
    BOOST_CHECK(fs::exists(dir));
   }
  BOOST_CHECK(!fs::exists(dir));
 }
 { // as non-empty dir
  TempDirectory td("pcrcalcSwap");
  BOOST_CHECK(fs::exists(td.name()));

  fs::path pete=td.memberPath("pete");
  fs::create_directory(pete);
  BOOST_CHECK( fs::exists(pete));

  td.remove();
  BOOST_CHECK(!fs::exists(td.name()));
 }
}


BOOST_AUTO_TEST_CASE(remove_failure)
{
  using namespace com;

  TempDirectory td("pcrcalcSwap");
  fs::path  toOpenForWriting=td.memberPath("toOpenForWriting");

  fs::basic_ofstream<char> bofs(toOpenForWriting);

  BOOST_CHECK(bofs.is_open());

#ifdef WIN32
  bool catched=false;
#endif
  try {
    td.remove();
  } catch (const com::Exception& e) {
    BOOST_CHECK(e.messages().find("pcrcalcSwap") != std::string::npos);
#ifdef WIN32
    catched=true;
#endif
  }
#ifdef WIN32
  // linux just throws the file away, should do something
  // with chmod I think, to simulate this error
  BOOST_CHECK(catched);
#endif
}
