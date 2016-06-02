#define BOOST_TEST_MODULE pcraster model_engine rundirectory
#include <boost/test/unit_test.hpp>
#include "com_exception.h"
#include "com_pathinfo.h"
#include "com_file.h"
#include "com_directory.h"
#include "calc_rundirectory.h"




BOOST_AUTO_TEST_CASE(testDefault)
{
  using namespace calc;

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


BOOST_AUTO_TEST_CASE(testRunDir)
{
  using namespace calc;

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


BOOST_AUTO_TEST_CASE(testSearchPath)
{
  using namespace calc;

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

BOOST_AUTO_TEST_CASE(testOutputFilePath)
{
  using namespace calc;

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
