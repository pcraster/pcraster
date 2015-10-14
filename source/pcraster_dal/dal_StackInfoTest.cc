#define BOOST_TEST_MODULE pcraster dal stack_info
#include <boost/test/unit_test.hpp>
#include "dal_Exception.h"
#include "dal_StackInfo.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace dal;

  boost::filesystem::path filename;

  {
    // Static stack in current dir.
    StackInfo info("soil.csf");
    BOOST_CHECK_EQUAL(info.name(), "soil.csf");
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename().string(), "soil.csf");
  }

  {
    // Static stack in current dir. No extension.
    StackInfo info("soil");
    BOOST_CHECK_EQUAL(info.name(), "soil");
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename().string(), "soil");
  }

  {
    // Dynamic stack in current dir.
    StackInfo info("soil0000.010+100");
    BOOST_CHECK_EQUAL(info.name(), "soil0000");
    BOOST_CHECK(info.isDynamic());
    BOOST_CHECK(info.isScanned());
    BOOST_CHECK_EQUAL(info.size(), size_t(91));
    BOOST_CHECK_EQUAL(info.filename(1).string(), "soil0000.001");
    BOOST_CHECK_EQUAL(info.filename(10).string(), "soil0000.010");
    BOOST_CHECK_EQUAL(info.filename(100).string(), "soil0000.100");
    BOOST_CHECK(info.contains(10));
    BOOST_CHECK(!info.contains(0));
    BOOST_CHECK(!info.contains(1));
    BOOST_CHECK(!info.contains(9));
    BOOST_CHECK(info.contains(11));
    BOOST_CHECK(info.contains(99));
    BOOST_CHECK(info.contains(100));
    BOOST_CHECK(!info.contains(101));
    BOOST_CHECK(info.begin() != info.end());
    BOOST_CHECK_EQUAL(*info.begin(), size_t(10));
    BOOST_CHECK_EQUAL(*(info.end() - 1), size_t(100));
  }

  //  {
  //    StackInfo info("soil", 10, 100);
  //    BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("soil0000",
  //         boost::filesystem::no_check).string());
  //    BOOST_CHECK(info.isDynamic());
  //    BOOST_CHECK(info.isScanned());
  //    BOOST_CHECK_EQUAL(info.size(), size_t(91));
  //    BOOST_CHECK_EQUAL(info.filename(1).string(), "soil0000.001");
  //    BOOST_CHECK_EQUAL(info.filename(10).string(), "soil0000.010");
  //    BOOST_CHECK_EQUAL(info.filename(100).string(), "soil0000.100");
  //    BOOST_CHECK(info.contains(10));
  //    BOOST_CHECK(!info.contains(0));
  //    BOOST_CHECK(!info.contains(1));
  //    BOOST_CHECK(!info.contains(9));
  //    BOOST_CHECK(info.contains(11));
  //    BOOST_CHECK(info.contains(99));
  //    BOOST_CHECK(info.contains(100));
  //    BOOST_CHECK(!info.contains(101));
  //    BOOST_CHECK(info.begin() != info.end());
  //    BOOST_CHECK_EQUAL(*info.begin(), size_t(10));
  //    BOOST_CHECK_EQUAL(*(info.end() - 1), size_t(100));
  //  }

  {
    StackInfo info("soilsoil.010+100");
    BOOST_CHECK_EQUAL(info.name(), "soilsoil");
    BOOST_CHECK(info.isDynamic());
    BOOST_CHECK(info.isScanned());
    BOOST_CHECK_EQUAL(info.size(), size_t(0));
    BOOST_CHECK_EQUAL(info.filename(1).string(), "soilsoil.001");
    BOOST_CHECK_EQUAL(info.filename(10).string(), "soilsoil.010");
    BOOST_CHECK_EQUAL(info.filename(100).string(), "soilsoil.100");
  }

  //  {
  //    StackInfo info("soilsoil", 10, 100);
  //    BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("soilsoil",
  //         boost::filesystem::no_check).string());
  //    BOOST_CHECK(info.isDynamic());
  //    BOOST_CHECK(info.isScanned());
  //    BOOST_CHECK_EQUAL(info.size(), size_t(0));
  //    BOOST_CHECK_EQUAL(info.filename(1).string(), "soilsoil.001");
  //    BOOST_CHECK_EQUAL(info.filename(10).string(), "soilsoil.010");
  //    BOOST_CHECK_EQUAL(info.filename(100).string(), "soilsoil.100");
  //  }

  {
    StackInfo info("soilsoil.s10+100");
    BOOST_CHECK_EQUAL(info.name(), "soilsoil.s");
    BOOST_CHECK(info.isDynamic());
    BOOST_CHECK(info.isScanned());
    BOOST_CHECK_EQUAL(info.size(), size_t(0));
    BOOST_CHECK_EQUAL(info.filename(1).string(), "soilsoil.s01");
    BOOST_CHECK_EQUAL(info.filename(10).string(), "soilsoil.s10");
    BOOST_CHECK_EQUAL(info.filename(99).string(), "soilsoil.s99");
  }

  //  {
  //    StackInfo info("soilsoil.s", 10, 100);
  //    BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("soilsoil.s",
  //         boost::filesystem::no_check).string());
  //    BOOST_CHECK(info.isDynamic());
  //    BOOST_CHECK(info.isScanned());
  //    BOOST_CHECK_EQUAL(info.size(), size_t(0));
  //    BOOST_CHECK_EQUAL(info.filename(1).string(), "soilsoil.s01");
  //    BOOST_CHECK_EQUAL(info.filename(10).string(), "soilsoil.s10");
  //    BOOST_CHECK_EQUAL(info.filename(99).string(), "soilsoil.s99");
  //  }

  {
    // Dynamic stack in current dir.
    StackInfo info("soil0000.010");
    BOOST_CHECK_EQUAL(info.name(), "soil0000.0");
    BOOST_CHECK(info.isDynamic());
    BOOST_CHECK(info.isScanned());
    BOOST_CHECK_EQUAL(info.size(), size_t(1));
    BOOST_CHECK_EQUAL(info.filename(10).string(), "soil0000.010");
    BOOST_CHECK(info.contains(10));
    BOOST_CHECK(!info.contains(0));
    BOOST_CHECK(!info.contains(1));
    BOOST_CHECK(!info.contains(9));
    BOOST_CHECK(!info.contains(11));
    BOOST_CHECK(info.begin() != info.end());
    BOOST_CHECK_EQUAL(*info.begin(), size_t(10));
    BOOST_CHECK(++info.begin() == info.end());
  }

  {
    StackInfo info("soil0000.010+10");
    BOOST_CHECK_EQUAL(info.name(), "soil0000.0");
    BOOST_CHECK(info.isDynamic());
    BOOST_CHECK(info.isScanned());
    BOOST_CHECK_EQUAL(info.size(), size_t(1));
    BOOST_CHECK_EQUAL(info.filename(10).string(), "soil0000.010");
    BOOST_CHECK(info.contains(10));
    BOOST_CHECK(!info.contains(0));
    BOOST_CHECK(!info.contains(1));
    BOOST_CHECK(!info.contains(9));
    BOOST_CHECK(!info.contains(11));
    BOOST_CHECK(info.begin() != info.end());
    BOOST_CHECK_EQUAL(*info.begin(), size_t(10));
    BOOST_CHECK(++info.begin() == info.end());
  }

  //  {
  //    StackInfo info("soil", 10, 10);
  //    BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("soil",
  //         boost::filesystem::no_check).string());
  //    BOOST_CHECK(info.isDynamic());
  //    BOOST_CHECK(info.isScanned());
  //    BOOST_CHECK_EQUAL(info.size(), size_t(1));
  //    BOOST_CHECK_EQUAL(info.filename(10).string(), "soil0000.010");
  //    BOOST_CHECK(info.contains(10));
  //    BOOST_CHECK(!info.contains(0));
  //    BOOST_CHECK(!info.contains(1));
  //    BOOST_CHECK(!info.contains(9));
  //    BOOST_CHECK(!info.contains(11));
  //    BOOST_CHECK(info.begin() != info.end());
  //    BOOST_CHECK_EQUAL(*info.begin(), size_t(10));
  //    BOOST_CHECK(++info.begin() == info.end());
  //  }

  {
    // Dynamic stack in current dir. Last step number is bigger than possible
    // according to space in the name (999 is max possible).
    StackInfo info("XXXeight.001+20000");
    BOOST_CHECK_EQUAL(info.name(), "XXXeight");
    BOOST_CHECK(info.isDynamic());
    BOOST_CHECK(info.isScanned());
    BOOST_CHECK_EQUAL(info.size(), size_t(0));
    BOOST_CHECK_EQUAL(info.filename(10).string(), "XXXeight.010");
  }

  //  {
  //    StackInfo info("XXXeight", 1, 20000);
  //    BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("XXXeight",
  //         boost::filesystem::no_check).string());
  //    BOOST_CHECK(info.isDynamic());
  //    BOOST_CHECK(info.isScanned());
  //    BOOST_CHECK_EQUAL(info.size(), size_t(0));
  //    BOOST_CHECK_EQUAL(info.filename(10).string(), "XXXeight.010");
  //  }

  {
    // Name ends with numeric digits.
    StackInfo info("XXXX9700.001+30");
    BOOST_CHECK_EQUAL(info.name(), "XXXX9700.0");
    BOOST_CHECK(info.isDynamic());
    BOOST_CHECK(info.isScanned());
    BOOST_CHECK_EQUAL(info.size(), size_t(0));
    BOOST_CHECK_EQUAL(info.filename(10).string(), "XXXX9700.010");
  }

  //  {
  //    StackInfo info("XXXX9700", 1, 30);
  //    BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("XXXX9700.0",
  //         boost::filesystem::no_check).string());
  //    BOOST_CHECK(info.isDynamic());
  //    BOOST_CHECK(info.isScanned());
  //    BOOST_CHECK_EQUAL(info.size(), size_t(0));
  //    BOOST_CHECK_EQUAL(info.filename(10).string(), "XXXX9700.010");
  //  }

  {
    // Test functionality that based on the last timestep the digits for the
    // first time step are determined. In this case, 9700 cannot be part of the
    // timestep and thus is part of the basename.
    StackInfo info("lisw9700.001+999");
    BOOST_CHECK_EQUAL(info.name(), "lisw9700");
    BOOST_CHECK(info.isDynamic());
    BOOST_CHECK(info.isScanned());
    BOOST_CHECK_EQUAL(info.size(), size_t(0));
    BOOST_CHECK_EQUAL(info.filename(10).string(), "lisw9700.010");
  }

  //  {
  //    StackInfo info("lisw9700", 1, 999);
  //    BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("lisw9700",
  //         boost::filesystem::no_check).string());
  //    BOOST_CHECK(info.isDynamic());
  //    BOOST_CHECK(info.isScanned());
  //    BOOST_CHECK_EQUAL(info.size(), size_t(0));
  //    BOOST_CHECK_EQUAL(info.filename(10).string(), "lisw9700.010");
  //  }

  // pcrcalc has a trick that if the extension
  // is given each timestep is written to the
  // same file
  {
    // CW wanted to use this one in pcrcalc
    StackInfo info("tmp.res");
    BOOST_CHECK_EQUAL(info.name(), "tmp.res");
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename(1).string(), "tmp.res");
    BOOST_CHECK_EQUAL(info.filename(10).string(), "tmp.res");
    BOOST_CHECK_EQUAL(info.filename(300).string(), "tmp.res");
  }

  // Throw!
  //  {
  //    // and not this one in pcrcalc
  //    dal::StackInfo info("tmp.res", 1, 1000);
  //    BOOST_CHECK(info.isDynamic());
  //    BOOST_CHECK_EQUAL(info.filename(1), "tmp.res");
  //    BOOST_CHECK_EQUAL(info.filename(300), "tmp.res");
  //  }

  //  {
  //    // Dynamic stack in current dir.
  //    StackInfo info("soil(10,100)");

  //    BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("soil0000",
  //         boost::filesystem::no_check).string());
  //    BOOST_CHECK(info.isDynamic());
  //    BOOST_CHECK(info.isScanned());
  //    BOOST_CHECK_EQUAL(info.size(), size_t(91));
  //    BOOST_CHECK_EQUAL(info.filename(1).string(), "soil0000.001");
  //    BOOST_CHECK_EQUAL(info.filename(10).string(), "soil0000.010");
  //    BOOST_CHECK_EQUAL(info.filename(100).string(), "soil0000.100");
  //    BOOST_CHECK(info.contains(10));
  //    BOOST_CHECK(!info.contains(0));
  //    BOOST_CHECK(!info.contains(1));
  //    BOOST_CHECK(!info.contains(9));
  //    BOOST_CHECK(info.contains(11));
  //    BOOST_CHECK(info.contains(99));
  //    BOOST_CHECK(info.contains(100));
  //    BOOST_CHECK(!info.contains(101));
  //    BOOST_CHECK(info.begin() != info.end());
  //    BOOST_CHECK_EQUAL(*info.begin(), size_t(10));
  //    BOOST_CHECK_EQUAL(*(info.end() - 1), size_t(100));
  //  }

  //  {
  //    // Dynamic stack in current dir.
  //    StackInfo info("soil(10,20,2)");

  //    BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("soil0000",
  //         boost::filesystem::no_check).string());
  //    BOOST_CHECK_EQUAL(info.size(), size_t(5));
  //    BOOST_CHECK_EQUAL(info.filename(10).string(), "soil0000.010");
  //    BOOST_CHECK(!info.contains(9));
  //    BOOST_CHECK( info.contains(10));
  //    BOOST_CHECK(!info.contains(11));
  //    BOOST_CHECK( info.contains(12));
  //    BOOST_CHECK(!info.contains(13));
  //    BOOST_CHECK( info.contains(14));
  //    BOOST_CHECK(!info.contains(15));
  //    BOOST_CHECK( info.contains(16));
  //    BOOST_CHECK(!info.contains(17));
  //    BOOST_CHECK( info.contains(18));
  //    BOOST_CHECK(!info.contains(19));
  //    BOOST_CHECK( info.contains(20));
  //    BOOST_CHECK(!info.contains(21));
  //    BOOST_CHECK_EQUAL(*info.begin(), size_t(10));
  //    BOOST_CHECK_EQUAL(*(info.end() - 1), size_t(20));
  //  }

  // Directories.
  {
    // Static stack in current dir.
    boost::filesystem::path path(".");
    path /= "soil.csf";
    StackInfo info(path.string());
    BOOST_CHECK_EQUAL(info.name(), path.string());
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename(), path);
  }

  {
    // Static stack in upper dir.
    boost::filesystem::path path("..");
    path /= "soil.csf";
    StackInfo info(path.string());
    BOOST_CHECK_EQUAL(info.name(), path.string());
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename(), path);
  }

  {
    // Static stack in lower dir.
    boost::filesystem::path path("bla");
    path /= "soil.csf";
    StackInfo info(path.string());
    BOOST_CHECK_EQUAL(info.name(), path.string());
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename(), path);
  }

  {
    // Static stack in root dir.
    boost::filesystem::path path("/");
    path /= "soil.csf";
    StackInfo info(path.string());
    BOOST_CHECK_EQUAL(info.name(), path.string());
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename(), path);
  }

  {
    // Static stack in sub dir of roo dir.
    boost::filesystem::path path("/");
    path /= "bla";
    path /= "soil.csf";
    StackInfo info(path.string());
    BOOST_CHECK_EQUAL(info.name(), path.string());
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename(), path);
  }

  // {
  //   StackInfo info("soilsoi..001+100");
  //   BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("soilsoi.",
  //        boost::filesystem::no_check).string());
  //   BOOST_CHECK(info.isDynamic());
  //   BOOST_CHECK_EQUAL(info.filename(10).string(), "soilsoi..010");
  // }

  {
    StackInfo info("soilsoi0.000");
    BOOST_CHECK_EQUAL(info.name(), boost::filesystem::path("soilsoi0.000").string());
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename().string(), "soilsoi0.000");
  }

  {
    StackInfo info("soilsoil.000");
    BOOST_CHECK_EQUAL(info.name(), boost::filesystem::path("soilsoil.000").string());
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename().string(), "soilsoil.000");
  }

  {
    StackInfo info("5");
    BOOST_CHECK_EQUAL(info.name(), boost::filesystem::path("5").string());
    BOOST_CHECK(!info.isDynamic());
    BOOST_CHECK_EQUAL(info.filename().string(), "5");
  }

#ifdef WIN32
  {
    StackInfo info("../bla.map", false);
    BOOST_CHECK_EQUAL(info.name(),
         boost::filesystem::path("../bla.map").string());
  }
#else
  {
    StackInfo info("volcano/volcano0.090+volcano/lava0000.090", false);
    BOOST_CHECK_EQUAL(info.name(), boost::filesystem::path(
         "volcano/volcano0.090+volcano/lava0000.0").string());
  }
#endif
}


BOOST_AUTO_TEST_CASE(bad_formats)
{
  using namespace dal;

  bool caughtException;

  // stupid error
  caughtException = false;
  try {
    StackInfo("+");
  }
  catch(Exception const&) {
    caughtException = true;
  }
  BOOST_CHECK(caughtException);

  // no last timestep
  caughtException = false;
  try {
    StackInfo("rnpnts00.001+");
  }
  catch(Exception const&) {
    caughtException = true;
  }
  BOOST_CHECK(caughtException);

  // last timestep is not numeric
  caughtException = false;
  try {
    StackInfo("rnpnts00.001+XXX");
  }
  catch(Exception const&) {
    caughtException = true;
  }
  BOOST_CHECK(caughtException);

  // no first timestep
  caughtException = false;
  try {
    StackInfo("rnpntsxx.xxx+431");
  }
  catch(Exception const&) {
    caughtException = true;
  }
  BOOST_CHECK(caughtException);

  // first timestep larger than second
  caughtException = false;
  try {
    StackInfo("XXXX970.009+3");
  }
  catch(Exception const&) {
    caughtException = true;
  }
  BOOST_CHECK(caughtException);


  // {
  //   StackInfo info("+");
  //   BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("+",
  //        boost::filesystem::no_check).string());
  //   BOOST_CHECK(!info.isDynamic());
  //   BOOST_CHECK_EQUAL(info.filename().string(), "+");
  // }

  // {
  //   StackInfo info("+100");
  //   BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("+100",
  //        boost::filesystem::no_check).string());
  //   BOOST_CHECK(!info.isDynamic());
  //   BOOST_CHECK_EQUAL(info.filename().string(), "+100");
  // }

  // {
  //   StackInfo info("100+");
  //   BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("100+",
  //        boost::filesystem::no_check).string());
  //   BOOST_CHECK(!info.isDynamic());
  //   BOOST_CHECK_EQUAL(info.filename().string(), "100+");
  // }

  // {
  //   StackInfo info("100+100");
  //   BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("100+100",
  //        boost::filesystem::no_check).string());
  //   BOOST_CHECK(!info.isDynamic());
  //   BOOST_CHECK_EQUAL(info.filename().string(), "100+100");
  // }

  // {
  //   StackInfo info("a100+100");
  //   BOOST_CHECK_EQUAL(info.name().string(), boost::filesystem::path("a100+100",
  //        boost::filesystem::no_check).string());
  //   BOOST_CHECK(!info.isDynamic());
  //   BOOST_CHECK_EQUAL(info.filename().string(), "a100+100");
  // }
}


BOOST_AUTO_TEST_CASE(to_string)
{
  using namespace dal;

  {
    StackInfo info("blabla", false);
    BOOST_CHECK_EQUAL(info.toString(), "blabla");
  }

  {
    StackInfo info("blabla.bla", false);
    BOOST_CHECK_EQUAL(info.toString(), "blabla.bla");
  }

  {
    StackInfo info("blabla00.001+100");
    BOOST_CHECK_EQUAL(info.toString(), "blabla00.001+100");
  }

  /*
  {
    StackInfo info("blabla", 1, 100, false);
    BOOST_CHECK_EQUAL(info.toString(), "blabla00.001+100");
  }

  {
    StackInfo info("blablabl.a", 1, 100, false);
    BOOST_CHECK_EQUAL(info.toString(), "blablabl.a01+99");
  }
  */
}


BOOST_AUTO_TEST_CASE(copy)
{
  using namespace dal;

  {
    StackInfo info("soil.csf");
    StackInfo copy(info);

    BOOST_CHECK_EQUAL(copy.name(), boost::filesystem::path("soil.csf").string());
    BOOST_CHECK(!copy.isDynamic());
    BOOST_CHECK_EQUAL(copy.filename().string(), "soil.csf");

    copy = info;
    BOOST_CHECK_EQUAL(copy.name(), boost::filesystem::path("soil.csf").string());
    BOOST_CHECK(!copy.isDynamic());
    BOOST_CHECK_EQUAL(copy.filename().string(), "soil.csf");
  }
}
