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

  bool found = false;

  RunDirectory r;
  const std::string s1("fileName");
  BOOST_TEST(s1 == r.inputFilePath(found, s1));
  BOOST_TEST(!found);
  BOOST_TEST(s1 == r.outputFilePath(s1));
  r.setRunDirectory("", "");
  BOOST_TEST(s1 == r.inputFilePath(found, s1));
  BOOST_TEST(!found);
  BOOST_TEST(s1 == r.outputFilePath(s1));

  const std::string s2("mayContainDirPartInDefaultScheme/fileName");
  BOOST_TEST(s2 == r.inputFilePath(found, s2));
  BOOST_TEST(!found);
  BOOST_TEST(s2 == r.outputFilePath(s2));

  // test abs path in as outputFilePath is allowed
  com::PathName af("absFile");
  af.makeAbsolute();
  BOOST_TEST(r.outputFilePath(af.toString()) == af.toString());
}

BOOST_AUTO_TEST_CASE(run_dir)
{
  using namespace calc;

  bool found = false;
  RunDirectory r;
  std::string const dir("rundir_baseOfRunDirNotExistantIsOk");
  r.setRunDirectory(dir, "");
  const std::string s1("fileName");
  BOOST_TEST(s1 == r.inputFilePath(found, s1));
  BOOST_TEST(!found);
  com::PathName o1(dir);
  o1 += s1;
  BOOST_TEST(o1.toString() == r.outputFilePath(s1));

  r.setupForExecution();
  com::PathInfo const pi(dir);
  BOOST_TEST(pi.isDirectory());

  com::create(r.outputFilePath(s1));
  BOOST_TEST(com::PathInfo(o1).isFile());

  // test abs path as outputFilePath is NOT allowed
  bool exceptionCatched = false;
  com::PathName af("absFile");
  af.makeAbsolute();
  try {
    r.outputFilePath(af.toString());
    BOOST_TEST(exceptionCatched);
  } catch (const com::Exception &e) {
    exceptionCatched = true;
    BOOST_TEST(e.messages().find("-r") != std::string::npos);
    BOOST_TEST(e.messages().find("part") != std::string::npos);
  }

  r.setRunDirectory("", "");  // correct reset
  BOOST_TEST(s1 == r.inputFilePath(found, s1));
  BOOST_TEST(!found);
  BOOST_TEST(s1 == r.outputFilePath(s1));


  exceptionCatched = false;
  try {
    std::string const dir("NonBasePartOfRunDirMustExist/base");
    RunDirectory re;
    re.setRunDirectory(dir, "");
    BOOST_TEST(exceptionCatched);
  } catch (const com::Exception &e) {
    exceptionCatched = true;
    BOOST_TEST(e.messages().find("-r") != std::string::npos);
  }
  BOOST_TEST(exceptionCatched);
}

BOOST_AUTO_TEST_CASE(search_path)
{
  using namespace calc;

  bool found = false;
  RunDirectory r;
  std::string const runDirName("rundir_dir1/dir2/dir3");

  com::PathName pnRd(runDirName);
  pnRd.makeNative();
  com::Directory const runDir(pnRd);
  // runDir.create(true);

  r.setRunDirectory(runDirName, "");

  std::string const runFileNotFound("rundir_dir1/inFile");
  com::PathName pnFnf(runFileNotFound);
  pnFnf.makeNative();
  com::create(pnFnf);

  std::string const runFileFound("rundir_dir1/dir2/inFile");
  com::PathName pnFf(runFileFound);
  pnFf.makeNative();
  pnFf.makeAbsolute();
  com::create(pnFf);

  BOOST_TEST_MESSAGE(pnFf.toString());                   //
  BOOST_TEST_MESSAGE(r.inputFilePath(found, "inFile"));  // //
  BOOST_TEST(pnFf.toString() == r.inputFilePath(found, "inFile"));
  BOOST_TEST(found);

  std::string const fne("inFileNotExistant");
  BOOST_TEST(fne == r.inputFilePath(found, fne));
  BOOST_TEST(!found);
}

// void calc::RunDirectoryTest::testExternalBindingsFile()
