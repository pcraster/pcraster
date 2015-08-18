#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_FILETEST
#include "com_filetest.h"
#define INCLUDED_COM_FILETEST
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef  INCLUDED_COM_EXCEPTION
# include "com_exception.h"
#define  INCLUDED_COM_EXCEPTION
#endif

#ifndef  INCLUDED_COM_PATHINFO
# include "com_pathinfo.h"
#define  INCLUDED_COM_PATHINFO
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



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*com::FileTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FileTest> instance(new FileTest());

  suite->add(BOOST_CLASS_TEST_CASE(&FileTest::testMove, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FileTest::testRemoveFile, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FileTest::testOpenIfStream, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FileTest::testOpenOfStream, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FileTest::testFilesEqual, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FileTest::testCopy, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FileTest::testReadWriteSize, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FileTest::testSkipWhiteSpace, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

com::FileTest::FileTest(){
}

void com::FileTest::setUp()
{
}

void com::FileTest::tearDown()
{
}

void com::FileTest::testOpenIfStream()
{
  bool visit;

  visit=false;
  try {
   std::ifstream fs;
   open(fs,"if_notexistant");
  } catch ( const com::OpenFileError& ) {
      visit=true;
  }
  BOOST_CHECK(visit);

  visit=false;
  try {
   std::ifstream fs;
   open(fs,"if_isdirectory");
  } catch ( const com::OpenFileError& ) {
      visit=true;
  }
  BOOST_CHECK(visit);

#ifndef WIN32
  visit=false;
  try {
   std::ifstream fs;
   open(fs,"if_notreadable");
  } catch ( const com::OpenFileError& e) {
      visit=true;
  }
  BOOST_CHECK(visit);
#endif

  visit=false;
  try {
   std::ifstream fs;
   open(fs,"if_okreadable");
  } catch (...) {
      visit=true;
  }
  BOOST_CHECK(!visit);
}

void com::FileTest::testOpenOfStream()
{

  bool visit;

  visit=false;
  try {
   std::ofstream fs;
   open(fs,"if_openof_ok_doenotexist");
  } catch ( const com::OpenFileError& ) {
      visit=true;
  }
  BOOST_CHECK(!visit);

  visit=false;
  try {
   std::ofstream fs;
   open(fs,"if_isdirectory");
  } catch ( const com::OpenFileError& e) {
      BOOST_CHECK(e.errorNr()==E_ISDIR);
      visit=true;
  }
  BOOST_CHECK(visit);

#ifdef WIN32
  visit=false;
  try {
   std::ofstream fs;
   open(fs,"if_notwritable");
   BOOST_WARN(false); // should not come here
   BOOST_WARN(!fs); // should not come here
  } catch ( const com::OpenFileError& e) {
      BOOST_WARN(e.errorNr()==E_ACCESWRITE);
      visit=true;
  }
  BOOST_WARN(visit);
#else
  visit=false;
  try {
   std::ofstream fs;
   open(fs,"if_notwritable");
   BOOST_CHECK(false); // should not come here
   BOOST_CHECK(!fs); // should not come here
  } catch ( const com::OpenFileError& e) {
      BOOST_WARN(e.errorNr()==E_ACCESWRITE);
      visit=true;
  }
  BOOST_CHECK(visit);
#endif

  // create in subdir
  visit=false;
  PathName pn("if_isdirectory");
  pn += "can_create";
  try {
   std::ofstream fs;
   open(fs,pn.toString());
  } catch (...) {
      visit=true;
  }
  BOOST_CHECK(!visit);
  PathInfo(pn).exists();

  // can not create in subdir
  visit=false;
  PathName pn2("if_notwritablesubdir");
  pn2 += "can_not_create";
  try {
   std::ofstream fs;
   open(fs,pn2.toString());
  } catch ( const com::OpenFileError& e) {
      // bugzilla #82  WIndows
      // if fixed also edit enum comment of E_ACCESCREATE
      BOOST_WARN(e.errorNr() == E_ACCESCREATE);
      visit=true;
  }
  // BOOST_CHECK(visit);
  PathInfo(pn2).exists();
}



void com::FileTest::testMove()
{
  bool moveFailed;

  // + Move existing regular file.
  moveFailed = false;
  try {
    PathName pn;
    PathInfo pi;

    pn = "mv_existingfile";
    pi = PathInfo(pn);
    BOOST_CHECK(pi.exists());
    BOOST_CHECK(pi.isFile());
    move("mv_existingfile", "mv_movedexistingfile");
    BOOST_CHECK(!pi.exists());
    pn = PathName("mv_movedexistingfile");
    pi = PathInfo(pn);
    BOOST_CHECK(pi.exists());
    BOOST_CHECK(pi.isFile());
  }
  catch(const FileError& ) {
    moveFailed = true;
  }
#ifdef WIN32
  BOOST_WARN(!moveFailed); // WindowsPerm Bugzilla #284
#else
  BOOST_CHECK(!moveFailed);
#endif



  // + Move existing directory.
  moveFailed = false;
  try {
    PathName pn;
    PathInfo pi;

    pn = "mv_existingdir";
    pi = PathInfo(pn);
    BOOST_CHECK(pi.exists());
    BOOST_CHECK(pi.isDirectory());
    move("mv_existingdir", "mv_movedexistingdir");
    BOOST_CHECK(!pi.exists());
    pn = PathName("mv_movedexistingdir");
    pi = PathInfo(pn);
    BOOST_CHECK(pi.exists());
    BOOST_CHECK(pi.isDirectory());
  }
  catch(const FileError& ) {
    moveFailed = true;
  }

#ifdef WIN32
  BOOST_WARN(!moveFailed); // WindowsPerm Bugzilla #284
#else
  BOOST_CHECK(!moveFailed);
#endif

  // - Move non-existing regular file.
  moveFailed = false;
  try {
    PathName pn;
    PathInfo pi;

    pn = "mv_failureExpected";
    pi = PathInfo(pn);
    BOOST_CHECK(!pi.exists());
    BOOST_CHECK(!pi.isDirectory());
    move("mv_failureExpected", "mv_movedfailureExpected");
  }
  catch(const FileError& e) {
    BOOST_CHECK(e.messages().find("No such file or directory") !=
                   std::string::npos);
    moveFailed = true;
  }

  BOOST_CHECK(moveFailed);



  // - Move non-existing directory.
  moveFailed = false;
  try {
    PathName pn;
    PathInfo pi;

    pn = "mv_failureExpectedDir";
    pi = PathInfo(pn);
    BOOST_CHECK(!pi.exists());
    BOOST_CHECK(!pi.isDirectory());
    move("mv_failureExpectedDir", "mv_movedfailureExpectedDir");
  }
  catch(const FileError& e) {
    BOOST_CHECK(e.messages().find("No such file or directory") !=
                   std::string::npos);
    moveFailed = true;
  }

  BOOST_CHECK(moveFailed);



}



void com::FileTest::testRemoveFile()
{
 try {
  PathName f1("rm_file");
  com::remove(f1);
  PathName f2("rm_emptyDir");
  com::remove(f2);
 } catch(const FileError& ) {
    BOOST_CHECK(false);
 }

 PathName f("rm_nestedDir");
 f += "rm_thisEmptyDir";
 BOOST_CHECK(com::PathInfo(f).exists());
 com::remove(f);
 BOOST_CHECK(!com::PathInfo(f).exists());

 // can not remove a directory containing something
 bool visit=false;
 PathName notDeleted("rm_failureExpectedFilledDir");
 try {
  BOOST_CHECK(com::PathInfo(notDeleted).exists());
  com::remove(notDeleted);
 } catch(const FileError& ) {
  visit=true;
 }
 BOOST_CHECK(com::PathInfo(notDeleted).exists());
 BOOST_CHECK(visit);
}

void com::FileTest::testFilesEqual()
{
  BOOST_CHECK(filesEqual("fe_one","fe_one"));
  BOOST_CHECK(!filesEqual("fe_one","fe_two"));
  BOOST_CHECK(!filesEqual("fe_empty","fe_one"));
  BOOST_CHECK(filesEqual("fe_empty","fe_empty"));
}

void com::FileTest::testCopy()
{
 {
  remove("result.tmp");
  copy("fe_one","result.tmp");
  BOOST_CHECK(exists("result.tmp"));
  BOOST_CHECK(filesExistsAndEqual("fe_one","result.tmp"));
 }
 {
  {
   ScopedRename sr("result.tmp","result.swap");
   BOOST_CHECK(filesExistsAndEqual("fe_one","result.swap"));
   BOOST_CHECK(!exists("result.tmp"));
  }
  BOOST_CHECK(filesExistsAndEqual("fe_one","result.tmp"));
  BOOST_CHECK(!exists("result.swap"));
 }
 {
  remove("result.tmp");
  copy("fe_empty","result.tmp");
  BOOST_CHECK(exists("result.tmp"));
  BOOST_CHECK(filesExistsAndEqual("fe_empty","result.tmp"));
 }
 {
  PathName r("if_isdirectory");
  BOOST_CHECK(exists(r));

  r.join("fe_one");
  remove(r);

  copy("fe_one","if_isdirectory");
  BOOST_CHECK(exists(r));

  BOOST_CHECK(filesExistsAndEqual("fe_one",r.toString()));
 }
}

//! test both read and write
void com::FileTest::testReadWriteSize()
{
 {
  std::string c;
  PathName pn("fe_empty");
  read(c,pn);
  BOOST_CHECK(size(pn)==0);
  write(c,"result.tmp");
  BOOST_CHECK(filesEqual("fe_empty","result.tmp"));
  BOOST_CHECK(size("result.tmp")==0);
 }
 {
  std::string c;
  PathName pn("fe_one");
  read(c,pn);
  write(c,"result.tmp");
  BOOST_CHECK(filesEqual("fe_one","result.tmp"));
 }
 {
  std::string c;
  std::string contents("line only two\n");
  PathName pn("fe_two");
  BOOST_CHECK(size(pn) == contents.size());
  read(c,pn);
  BOOST_CHECK(c==contents);
  write(c,"result.tmp");
  BOOST_CHECK(filesEqual("fe_two","result.tmp"));
  size_t expectedSize=contents.size();
#ifdef WIN32
  // newline is 2 chars
  expectedSize+=1;
#endif
  BOOST_CHECK(size("result.tmp") == expectedSize);
 }
}

void com::FileTest::testSkipWhiteSpace()
{
  // Mmm, doesn't work.
  std::stringstream stream("   a\n\t ");
  stream.seekg(0);
  // front space
  skipWhiteSpace(stream);
  char a;
  stream >> a;
  BOOST_CHECK(a=='a');
  // back space
  skipWhiteSpace(stream);
  BOOST_CHECK(stream.eof());
  // THIS is not true, str() always has the whole
  //  buffer, not the buffer starting from the seek-point
  // BOOST_CHECK(stream.str() == "a\n\t ");
  BOOST_CHECK(stream.str() == "   a\n\t ");
}
