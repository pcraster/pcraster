#define BOOST_TEST_MODULE pcraster old_calc run_directory
#include <boost/test/unit_test.hpp>
#include "com_exception.h"
#include "com_pathinfo.h"
#include "com_file.h"
#include "com_directory.h"
#include "calc_rundirectory.h"


BOOST_AUTO_TEST_CASE(default_)
{
  using namespace calc;

  bool found;

  RunDirectory r;
  const std::string s1("fileName");
  BOOST_CHECK(s1 == r.inputFilePath(found,s1));
  BOOST_CHECK(!found);
  BOOST_CHECK(s1 == r.outputFilePath(s1));
  r.setRunDirectory("","");
  BOOST_CHECK(s1 == r.inputFilePath(found,s1));
  BOOST_CHECK(!found);
  BOOST_CHECK(s1 == r.outputFilePath(s1));

  const std::string s2("mayContainDirPartInDefaultScheme/fileName");
  BOOST_CHECK(s2 == r.inputFilePath(found,s2));
  BOOST_CHECK(!found);
  BOOST_CHECK(s2 == r.outputFilePath(s2));

 // test abs path in as outputFilePath is allowed
 com::PathName af("absFile");
 af.makeAbsolute();
 BOOST_CHECK(r.outputFilePath(af.toString()) == af.toString());
}


BOOST_AUTO_TEST_CASE(run_dir)
{
  using namespace calc;

  bool found;
  RunDirectory r;
  std::string dir("rundir_baseOfRunDirNotExistantIsOk");
  r.setRunDirectory(dir,"");
  const std::string s1("fileName");
  BOOST_CHECK(s1 == r.inputFilePath(found,s1));
  BOOST_CHECK(!found);
  com::PathName o1(dir);
  o1+=s1;
  BOOST_CHECK(o1.toString() == r.outputFilePath(s1));

  r.setupForExecution();
  com::PathInfo pi(dir);
  BOOST_CHECK(pi.isDirectory());

  com::create(r.outputFilePath(s1));
  BOOST_CHECK(com::PathInfo(o1).isFile());

 // test abs path as outputFilePath is NOT allowed
 bool exceptionCatched=false;
 com::PathName af("absFile");
 af.makeAbsolute();
 try {
    r.outputFilePath(af.toString());
    BOOST_CHECK(exceptionCatched);
 } catch (const com::Exception& e) {
    exceptionCatched=true;
    BOOST_CHECK(e.messages().find("-r") != std::string::npos);
    BOOST_CHECK(e.messages().find("part") != std::string::npos);
 }

 r.setRunDirectory("",""); // correct reset
 BOOST_CHECK(s1 == r.inputFilePath(found,s1));
 BOOST_CHECK(!found);
 BOOST_CHECK(s1 == r.outputFilePath(s1));



 exceptionCatched=false;
 try {
    std::string dir("NonBasePartOfRunDirMustExist/base");
    RunDirectory re;
    re.setRunDirectory(dir,"");
    BOOST_CHECK(exceptionCatched);
 } catch (const com::Exception& e) {
    exceptionCatched=true;
    BOOST_CHECK(e.messages().find("-r") != std::string::npos);
 }
 BOOST_CHECK(exceptionCatched);
}


BOOST_AUTO_TEST_CASE(search_path)
{
  using namespace calc;

  bool found;
  RunDirectory r;
  std::string runDirName("rundir_dir1/dir2/dir3");

  com::PathName pnRd(runDirName);
  pnRd.makeNative();
  com::Directory runDir(pnRd);
  // runDir.create(true);

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

  BOOST_TEST_MESSAGE(pnFf.toString()); //
  BOOST_TEST_MESSAGE(r.inputFilePath(found,"inFile")); // //
  BOOST_CHECK(pnFf.toString() == r.inputFilePath(found,"inFile"));
  BOOST_CHECK(found);

  std::string fne("inFileNotExistant");
  BOOST_CHECK(fne == r.inputFilePath(found,fne));
  BOOST_CHECK(!found);
}

// void calc::RunDirectoryTest::testExternalBindingsFile()
