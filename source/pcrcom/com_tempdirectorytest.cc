#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_TEMPDIRECTORYTEST
#include "com_tempdirectorytest.h"
#define INCLUDED_COM_TEMPDIRECTORYTEST
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

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include "boost/filesystem/path.hpp"
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif
#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include "boost/filesystem/operations.hpp"
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif
#ifndef INCLUDED_BOOST_FILESYSTEM_FSTREAM
#include "boost/filesystem/fstream.hpp"
#define INCLUDED_BOOST_FILESYSTEM_FSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_TEMPDIRECTORY
#include "com_tempdirectory.h"
#define INCLUDED_COM_TEMPDIRECTORY
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the TempDirectoryTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

namespace com {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEMPDIRECTORY MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*TempDirectoryTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TempDirectoryTest> instance(new TempDirectoryTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TempDirectoryTest::testCtorDtor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TempDirectoryTest::testRemoveFailure, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TEMPDIRECTORY MEMBERS
//------------------------------------------------------------------------------

//! ctor
TempDirectoryTest::TempDirectoryTest(
         )
{
}



//! setUp
void TempDirectoryTest::setUp()
{
}



//! tearDown
void TempDirectoryTest::tearDown()
{
}

namespace fs=boost::filesystem;

void TempDirectoryTest::testCtorDtor()
{
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

void TempDirectoryTest::testRemoveFailure()
{
  TempDirectory td("pcrcalcSwap");
  fs::path  toOpenForWriting=td.memberPath("toOpenForWriting");

  fs::basic_ofstream<char> bofs(toOpenForWriting);

  BOOST_CHECK(bofs.is_open());

  bool catched=false;
  try {
    td.remove();
  } catch (const com::Exception& e) {
    BOOST_CHECK(e.messages().find("pcrcalcSwap") != std::string::npos);
    catched=true;
  }
#ifdef WIN32
  // linux just throws the file away, should do something
  // with chmod I think, to simulate this error
  BOOST_CHECK(catched);
#endif
}



} // namespace com

