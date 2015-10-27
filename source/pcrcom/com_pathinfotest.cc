#define BOOST_TEST_MODULE pcraster com path_info
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_pathinfo.h"
#include "com_pathname.h"
#include "com_exception.h"


BOOST_AUTO_TEST_CASE(temp_directory_name)
{
  using namespace com;

  PathName pn = tempDirectoryName();

  PathInfo pi(pn);

  BOOST_CHECK(!pn.isEmpty());
  BOOST_CHECK(pi.isDirectory());
  // there is nothing in tempDirectoryName
  // that garantuees this:
  // BOOST_CHECK(pi.isWritable());
}


BOOST_AUTO_TEST_CASE(exists)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(is_directory)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(is_file)
{
  using namespace com;

 BOOST_CHECK(PathInfo("pi_file").isFile());
 BOOST_CHECK(!PathInfo("pi_not_existent_file").isFile());
#ifdef WIN32
 bool reimplementIsFileOnWin32=false;
 BOOST_WARN(reimplementIsFileOnWin32);
#endif
}


BOOST_AUTO_TEST_CASE(is_readable)
{
  using namespace com;

  BOOST_CHECK(!PathInfo("pi_not_existent_file").isReadable());
#ifdef WIN32
  BOOST_WARN(!PathInfo("if_notreadable").isReadable()); // WindowsPerm Bugzilla #284
#else
  BOOST_CHECK(!PathInfo("if_notreadable").isReadable());
#endif
  BOOST_CHECK( PathInfo("if_okreadable").isReadable());
}


BOOST_AUTO_TEST_CASE(is_writable)
{
  using namespace com;

  BOOST_CHECK(!PathInfo("pi_not_existent_file").isWritable());
#ifdef WIN32
  BOOST_WARN(!PathInfo("if_notwritable"      ).isWritable()); // WindowsPerm Bugzilla #284
#else
  BOOST_CHECK(!PathInfo("if_notwritable"      ).isWritable());
#endif
  BOOST_CHECK( PathInfo("if_okwritable"       ).isWritable());
}


BOOST_AUTO_TEST_CASE(case_sensitive_name)
{
  using namespace com;

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
BOOST_AUTO_TEST_CASE(change_working_directory)
{
    using namespace com;

    const PathName nwd("if_UpcaseDirectory");
    changeWorkingDirectory(nwd);
    PathInfo pi("readableFile");
    bool found(pi.exists());
    changeWorkingDirectory("..");
    BOOST_CHECK(found);
}
