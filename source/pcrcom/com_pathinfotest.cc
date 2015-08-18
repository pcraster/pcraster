#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PATHINFOTEST
#include "com_pathinfotest.h"
#define INCLUDED_COM_PATHINFOTEST
#endif

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

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*com::PathInfoTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PathInfoTest> instance(new PathInfoTest());
  suite->add(BOOST_CLASS_TEST_CASE(&PathInfoTest::testExists, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathInfoTest::testIsDirectory, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathInfoTest::testIsFile, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathInfoTest::testIsReadable, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathInfoTest::testTestCaseSensitiveName, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathInfoTest::testIsWritable, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathInfoTest::testTempDirectoryName, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathInfoTest::testChangeWorkingDirectory, instance));
  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

com::PathInfoTest::PathInfoTest()
{
}

void com::PathInfoTest::setUp()
{
}

void com::PathInfoTest::tearDown()
{
}



void com::PathInfoTest::testTempDirectoryName()
{
  PathName pn = tempDirectoryName();

  PathInfo pi(pn);

  BOOST_CHECK(!pn.isEmpty());
  BOOST_CHECK(pi.isDirectory());
  // there is nothing in tempDirectoryName
  // that garantuees this:
  // BOOST_CHECK(pi.isWritable());
}



void com::PathInfoTest::testExists()
{
  PathName pn;
  PathInfo pi;

  // relative/notexist does not exist.
  pn = "relative";
  pn += "relative";
  pi = PathInfo(pn);
  BOOST_CHECK(!pi.exists());

  // pi_file does exist.
  pn = "pi_file";
  pi = PathInfo(pn);
  BOOST_CHECK(pi.exists());

#ifdef WIN32
  pn ="\\\\P4\\bin\\ls";
  pi = PathInfo(pn);
  bool volumePathTest=false;
  BOOST_WARN( (pi.exists() || volumePathTest) );
#endif
}

void com::PathInfoTest::testIsDirectory()
{
  PathName pn;
  PathInfo pi;

  // Current directory is a directory.
  pn = currentWorkingDirectory();
  pi = PathInfo(pn);
  BOOST_CHECK(pi.isDirectory());

  // foo/bar is not a directory (is doesn't even exist).
  pn = "foo";
  pn += "bar";
  pi = PathInfo(pn);
  BOOST_CHECK(!pi.isDirectory());

 BOOST_CHECK(PathInfo("pi_dir").isDirectory());
 BOOST_CHECK(!PathInfo("pi_file").isDirectory());
 BOOST_CHECK(!PathInfo("pi_not_existent_file").isDirectory());
}

void com::PathInfoTest::testIsFile()
{
 BOOST_CHECK(PathInfo("pi_file").isFile());
 BOOST_CHECK(!PathInfo("pi_not_existent_file").isFile());
#ifdef WIN32
 bool reimplementIsFileOnWin32=false;
 BOOST_WARN(reimplementIsFileOnWin32);
#endif
}

void com::PathInfoTest::testIsReadable()
{
  BOOST_CHECK(!PathInfo("pi_not_existent_file").isReadable());
#ifdef WIN32
  BOOST_WARN(!PathInfo("if_notreadable").isReadable()); // WindowsPerm Bugzilla #284
#else
  BOOST_CHECK(!PathInfo("if_notreadable").isReadable());
#endif
  BOOST_CHECK( PathInfo("if_okreadable").isReadable());
}

void com::PathInfoTest::testIsWritable()
{
  BOOST_CHECK(!PathInfo("pi_not_existent_file").isWritable());
#ifdef WIN32
  BOOST_WARN(!PathInfo("if_notwritable"      ).isWritable()); // WindowsPerm Bugzilla #284
#else
  BOOST_CHECK(!PathInfo("if_notwritable"      ).isWritable());
#endif
  BOOST_CHECK( PathInfo("if_okwritable"       ).isWritable());
}

void com::PathInfoTest::testTestCaseSensitiveName()
{
 bool noExcep(true);

 try {
   {
    PathInfo pi(nativePathName("if_UpcaseDirectory/readableFile"));
    BOOST_CHECK(pi.exists());
    pi.testCaseSensitiveName();
   }
 } catch (const com::Exception& /*e*/) {
   noExcep=false;
 }
 BOOST_CHECK(noExcep);

 try {
   {
    PathInfo pi(nativePathName("../pcrcom/if_UpcaseDirectory/readableFile"));
    BOOST_CHECK(pi.exists());
    pi.testCaseSensitiveName();
   }
 } catch (const com::Exception& /*e*/) {
   noExcep=false;
 }
 BOOST_CHECK(noExcep);

 try {
   {
    PathInfo pi(nativePathName("./if_UpcaseDirectory/readableFile"));
    BOOST_CHECK(pi.exists());
    pi.testCaseSensitiveName();
   }
 } catch (const com::Exception& /*e*/) {
   noExcep=false;
 }
 BOOST_CHECK(noExcep);

#ifdef WIN32
 bool excep(false);
 try {
   {
    PathInfo pi(nativePathName("if_upcasedirectory/readableFile"));
    BOOST_CHECK(pi.exists());
    pi.testCaseSensitiveName();
   }
 } catch (const com::Exception& e) {
   excep=true;
   // mixed case message
   BOOST_CHECK(e.messages().find("ixed")!=std::string::npos);
 }
 BOOST_CHECK(excep);

 try {
   {
    PathInfo pi(nativePathName("if_UpcaseDirectory/readablefile"));
    BOOST_CHECK(pi.exists());
    pi.testCaseSensitiveName();
   }
 } catch (const com::Exception& e) {
   excep=true;
   // mixed case message
   BOOST_CHECK(e.messages().find("ixed")!=std::string::npos);
 }
 BOOST_CHECK(excep);
#endif

}


// depends on testTestCaseSensitiveName() files
void com::PathInfoTest::testChangeWorkingDirectory()
{
    const PathName nwd("if_UpcaseDirectory");
    changeWorkingDirectory(nwd);
    PathInfo pi("readableFile");
    bool found(pi.exists());
    changeWorkingDirectory("..");
    BOOST_CHECK(found);
}
