#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_RUNDIRECTORYTEST
#include "calc_rundirectorytest.h"
#define INCLUDED_CALC_RUNDIRECTORYTEST
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
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_DIRECTORY
#include "com_directory.h"
#define INCLUDED_COM_DIRECTORY
#endif

// Module headers.

#ifndef INCLUDED_CALC_RUNDIRECTORY
#include "calc_rundirectory.h"
#define INCLUDED_CALC_RUNDIRECTORY
#endif


/*!
  \file
  This file contains the implementation of the RunDirectoryTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RUNDIRECTORY MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::RunDirectoryTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RunDirectoryTest> instance(new RunDirectoryTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RunDirectoryTest::testDefault, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RunDirectoryTest::testRunDir, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RunDirectoryTest::testSearchPath, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RunDirectoryTest::testOutputFilePath, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RUNDIRECTORY MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::RunDirectoryTest::RunDirectoryTest()
{
}



//! setUp
void calc::RunDirectoryTest::setUp()
{
}

//! tearDown
void calc::RunDirectoryTest::tearDown()
{
}



void calc::RunDirectoryTest::testDefault()
{
  bool found;

  RunDirectory r;
  const std::string s1("fileName");
  BOOST_CHECK(s1 == r.inPath(found,s1));
  BOOST_CHECK(!found);
  r.setRunDirectory("","");
  BOOST_CHECK(s1 == r.inPath(found,s1));
  BOOST_CHECK(!found);

  const std::string s2("mayContainDirPartInDefaultScheme/fileName");
  BOOST_CHECK(s2 == r.inPath(found,s2));
  BOOST_CHECK(!found);

 // test abs path in as outPath is allowed
 com::PathName af("absFile");
 af.makeAbsolute();

}

void calc::RunDirectoryTest::testRunDir()
{
  RunDirectory r;
  std::string dir("rundir_baseOfRunDirNotExistantIsOk");
  r.setRunDirectory(dir,"");
  const std::string s1("fileName");

/*
  bool found;
  BOOST_CHECK(s1 == r.inPath(found,s1));
  BOOST_CHECK(!found);
  com::PathName o1(dir);
  o1+=s1;
  BOOST_CHECK(o1.toString() == r.outPath(s1));
*/
}

void calc::RunDirectoryTest::testSearchPath()
{
  bool found;
  RunDirectory r;
  std::string runDirName("rundir_dir1/dir2/dir3");

  com::PathName pnRd(runDirName);
  pnRd.makeNative();
  com::Directory runDir(pnRd);

  runDir.create(true);

  r.setRunDirectory(runDirName,"");

  std::string runFileNotFound("rundir_dir1/inFile");
  com::PathName pnFnf(runFileNotFound);
  pnFnf.makeNative();
  com::create(pnFnf);

  std::string runFileFound("rundir_dir1/dir2/inFile");
  com::PathName pnFf(runFileFound);
  pnFf.makeNative();
  pnFf.makeAbsolute();
  com::create(pnFf);

  BOOST_CHECK(pnFf.toString() == r.inPath(found,"inFile"));
  BOOST_CHECK(found);

  std::string fne("inFileNotExistant");
  BOOST_CHECK(fne == r.inPath(found,fne));
  BOOST_CHECK(!found);
}

void calc::RunDirectoryTest::testOutputFilePath()
{
  const std::string s1("fileName");
  {
   RunDirectory rd;
   BOOST_CHECK(s1 == rd.outputFilePath(s1));
  }

  {
    // will be created
   std::string dir("rundir_baseOfRunDirNotExistantIsOk");
   RunDirectory rd(dir);
   com::PathName o1(dir);
   BOOST_CHECK(!com::exists(dir));
   o1+=s1;
   BOOST_CHECK(o1.toString() == rd.outputFilePath(s1));
   BOOST_CHECK(com::exists(dir));
   com::PathInfo pi(dir);
   BOOST_CHECK(pi.isDirectory());

   com::create(rd.outputFilePath(s1));
   BOOST_CHECK(com::PathInfo(o1).isFile());

   // test abs path as outputFilePath is allowed
   // NOT in -r ?
   com::PathName af("absFile");
   af.makeAbsolute();

   BOOST_CHECK(af.toString()== rd.outputFilePath(af.toString()));

  }
/*
 *try {
 *   r.outputFilePath(af.toString());
 *   BOOST_CHECK(exceptionCatched);
 *} catch (const com::Exception& e) {
 *   exceptionCatched=true;
 *  // BOOST_CHECK(e.messages().find("-r") != std::string::npos);
 *  // BOOST_CHECK(e.messages().find("part") != std::string::npos);
 *}
*/
 bool exceptionCatched=false;
 try {
    RunDirectory rd("NonBasePartOfRunDirMustExist/base");
    std::string s=rd.outputFilePath("failureExpected");
    BOOST_CHECK(exceptionCatched);
 } catch (const com::Exception& /*e*/) {
    exceptionCatched=true;
    //BOOST_CHECK(e.messages().find("-r") != std::string::npos);
 }
 BOOST_CHECK(exceptionCatched);
}

/*
void calc::RunDirectoryTest::testExternalBindings()
{
 //  implemented in calc_executortest.cc
}
*/
