#define BOOST_TEST_MODULE pcraster com path_name
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_pathname.h"
#include "com_pathinfo.h"


// These are the mothers of all tests in path name.
// Test if we get back what we put in.
//   testCtor
//   testToString
//
// Test if we get the right parts of what we put in.
//   testSplitFor
//
// First the tests which query the path name.
//   testIsEmpty
//   testIsRelative
//   testExtension
//   testRemoveExtension
//
// Then the functions which need those query functions of PathName.
//   testAdd
//   testUp
//   testMakeNative
//   testMakeAbsolute
//
// The mother test of all path name comparisons.
//   testCompare
//
// then the brilliant tests of Cees
//   testEquals
//   testClear
//
//   testUnc


static std::string d_slash(com::PathName::dirPathDelimNative());

BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace com;

  com::PathName const pn("../messagestest.xml");
  BOOST_TEST(!pn.isAbsolute());

  try {
    PathName const pn1(std::string("todoHaatQuoteInNaam.map\""));
  } catch (...) {
  }
  bool const todoHaatQuoteInNaam = false;
  BOOST_TEST_WARN(todoHaatQuoteInNaam);
}

BOOST_AUTO_TEST_CASE(to_string)
{
  using namespace com;

  PathName pn;
  std::string result;

  // ""
  result = "";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "/"
  result = d_slash;
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "/bla"
  result = d_slash + "bla";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "//bla"
  result = d_slash + d_slash + "bla";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "/bla/"
  result = d_slash + "bla";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "//bla/"
  result = d_slash + d_slash + "bla";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);


  // "/bla/bli"
  result = d_slash + "bla" + d_slash + "bli";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);


  // "/bla/bli/"
  result = d_slash + "bla" + d_slash + "bli";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "//bla/bli/"
  result = d_slash + d_slash + "bla" + d_slash + "bli";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "//bla/bli"
  result = d_slash + d_slash + "bla" + d_slash + "bli";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

#ifdef WIN32
  bool todoCheckDoubleSlash = false;
  BOOST_TEST_WARN(todoCheckDoubleSlash);
#else
  // "//bla//bli"
  result = d_slash + d_slash + "bla" + d_slash + d_slash + "bli";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "//bla//bli/"
  result = d_slash + d_slash + "bla" + d_slash + d_slash + "bli";
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "//bla//bli//"
  result = d_slash + d_slash + "bla" + d_slash + d_slash + "bli" + d_slash + d_slash;
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);

  // "//bla//"
  result = d_slash + d_slash + "bla" + d_slash + d_slash;
  pn = PathName(result);
  BOOST_TEST(pn.toString() == result);
#endif
}

BOOST_AUTO_TEST_CASE(split_for)
{
  using namespace com;

  PathName pn;
#ifdef WIN32
  std::string drive;
#endif
  std::string directory;
  std::string base;

  // ""
  pn = PathName("");
  directory = "";
  base = "";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "/"
  pn = PathName(d_slash);
  directory = d_slash;
  base = "";  // d_slash;
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "//"
  pn = PathName(d_slash + d_slash);
  directory = d_slash;
  base = d_slash;
#ifdef WIN32
  BOOST_TEST(pn.directoryName() == "\\\\");
#else
  BOOST_TEST(pn.directoryName() == "//");  // ""
#endif
  // #ifdef WIN32
  //  BOOST_TEST(pn.baseName() == "\\\\");
  // #else
  BOOST_TEST(pn.baseName() == "");  // "//"
                                         // #endif

  // "bla"
  pn = PathName("bla");
  directory = "";
  base = "bla";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "bla/"
  pn = PathName("bla" + d_slash);
  directory = "bla";
  base = "";  // "."
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "/bla"
  pn = PathName(d_slash + "bla");
  directory = d_slash;
  base = "bla";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "//bla"
  pn = PathName(d_slash + d_slash + "bla");
#ifdef __APPLE__
  directory = "//";
#elif defined(WIN32)
  directory = "\\\\bla";
#else
  directory = "/";
#endif
#ifdef WIN32
  base = "";  // \\\\bla
#else
  base = "bla";  // "//bla";
#endif
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "/bla/"
  pn = PathName(d_slash + "bla" + d_slash);
  directory = d_slash + "bla";
  base = "";  // ".";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "bla/bli"
  pn = PathName("bla" + d_slash + "bli");
  directory = "bla";
  base = "bli";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "bla/bli/"
  pn = PathName("bla" + d_slash + "bli" + d_slash);
  directory = "bla" + d_slash + "bli";
  base = "";  // ".";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "/bla/bli"
  pn = PathName(d_slash + "bla" + d_slash + "bli");
  directory = d_slash + "bla";
  base = "bli";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "/bla/bli/"
  pn = PathName(d_slash + "bla" + d_slash + "bli" + d_slash);
  directory = d_slash + "bla" + d_slash + "bli";
  base = "";  // ".";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

#ifdef WIN32
  // "c:" -> "c:"
  pn = PathName("c:");
  directory = "c:";  // ""
  base = "";         // c:
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "c:\"
  pn = PathName("c:" + d_slash);
  directory = "c:\\";  // c:
  base = "";           // "\\"
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "c:bla" -> "c:bla"
  pn = PathName("c:bla");
  directory = "c:";
  base = "bla";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "c:bla
  pn = PathName("c:bla" + d_slash);
  directory = "c:";
  base = "bla";
  BOOST_TEST(pn.directoryName() == "c:bla");
  BOOST_TEST(pn.baseName() == "");  // ".");

  // "c:\bla"
  pn = PathName("c:" + d_slash + "bla");
  directory = "c:\\";
  base = "bla";
  BOOST_TEST(pn.directoryName() == directory);
  BOOST_TEST(pn.baseName() == base);

  // "c:\bla\"
  pn = PathName("c:" + d_slash + "bla" + d_slash);
  BOOST_TEST(pn.directoryName() == "c:\\bla");
  BOOST_TEST(pn.baseName() == "");  //  ".");
#endif


  // "bla//bli"
  pn = PathName("bla" + d_slash + d_slash + "bli");
  directory = "bla" + d_slash;
  base = "bli";
  BOOST_TEST(pn.baseName() == base);
}

BOOST_AUTO_TEST_CASE(is_empty)
{
  using namespace com;

  PathName pn;
  BOOST_TEST(pn.isEmpty());

  pn = PathName(d_slash);
  BOOST_TEST(!pn.isEmpty());

  pn = PathName("bla");
  BOOST_TEST(!pn.isEmpty());

#ifdef WIN32
  pn = PathName("c:");
  BOOST_TEST(!pn.isEmpty());
#endif
}

BOOST_AUTO_TEST_CASE(is_relative)
{
  using namespace com;

  PathName pn;

  // ""
  pn = PathName("");
  BOOST_TEST(pn.isRelative());

  // "foo"
  pn = PathName("foo");
  BOOST_TEST(pn.isRelative());

  // "foo/bar"
  pn = PathName("foo" + d_slash + "bar");
  BOOST_TEST(pn.isRelative());

  // "/"
  pn = PathName(d_slash);
#ifdef WIN32
  // On windows this is relative to the current drive.
  BOOST_TEST(pn.isRelative());
#else
  // On unix this is the root dir.
  BOOST_TEST(!pn.isRelative());
#endif

  // "c:\"
#ifdef WIN32
  pn = PathName("c:" + d_slash);
  BOOST_TEST(!pn.isRelative());
#endif

  // "/foo"
  pn = PathName(d_slash + "foo");
#ifdef WIN32
  BOOST_TEST(pn.isRelative());
#else
  BOOST_TEST(!pn.isRelative());
#endif

  // "/foo/bar"
  pn = PathName(d_slash + "foo" + d_slash + "bar");
#ifdef WIN32
  BOOST_TEST(pn.isRelative());
#else
  BOOST_TEST(!pn.isRelative());
#endif

  // "/foo/bar/"
  pn = PathName(d_slash + "foo" + d_slash + "bar" + d_slash);
#ifdef WIN32
  BOOST_TEST(pn.isRelative());
#else
  BOOST_TEST(!pn.isRelative());
#endif
}

BOOST_AUTO_TEST_CASE(make_absolute)
{
  using namespace com;

  PathName pathName;

  pathName = PathName("bla");
  BOOST_TEST(pathName.isRelative());
  pathName.makeAbsolute();
  BOOST_TEST(pathName.isAbsolute());
}

BOOST_AUTO_TEST_CASE(extension)
{
  using namespace com;

  PathName pn("bla.txt");
  BOOST_TEST(pn.hasExtension());
  BOOST_TEST(pn.extension() == "txt");
  {
    PathName const pn("bla.mpeg");
    BOOST_TEST(pn.hasExtension());
    BOOST_TEST(pn.extension() == "mpeg");
  }

  pn = PathName("bla");
  BOOST_TEST(!pn.hasExtension());
  BOOST_TEST(pn.extension() == "");

  pn = PathName("");
  BOOST_TEST(!pn.hasExtension());

  pn = PathName("bla");
  pn.addExtension("txt");
  BOOST_TEST(pn.hasExtension());
  BOOST_TEST(pn.extension() == "txt");

  pn = PathName("bla");
  pn.addExtension("");
  BOOST_TEST(!pn.hasExtension());
  BOOST_TEST(pn == PathName("bla"));

  {
    PathName pn("blx.mpeg");
    BOOST_TEST(pn.hasExtension());
    pn.addExtension("gif");
    BOOST_TEST(pn.extension() == "gif");
    BOOST_TEST(pn == PathName("blx.mpeg.gif"));
  }
  {
    PathName pn("bla.mpeg");
    BOOST_TEST(pn.hasExtension());
    pn.setExtension("gif");
    BOOST_TEST(pn.extension() == "gif");
    BOOST_TEST(pn == PathName("bla.gif"));
  }
  {
    PathName pn("bla.prefix1.mpeg");
    BOOST_TEST(pn.hasExtension());
    pn.setExtension("gif");
    BOOST_TEST(pn.extension() == "gif");
    BOOST_TEST(pn == PathName("bla.prefix1.gif"));
  }
  {
    PathName pn("validated/windowdiversity.Result3.omap");
    BOOST_TEST(pn.hasExtension());
    BOOST_TEST(pn.extension() == "omap");
    pn.setExtension("map");
    BOOST_TEST(pn.extension() == "map");
    BOOST_TEST(pn == PathName("validated/windowdiversity.Result3.map"));
  }

#ifdef WIN32
  try {
    pn = PathName("bla.");
  } catch (...) {
  };
  bool todoNameCheckFailure = false;
  // see boost::filesystem::name_check
  BOOST_TEST_WARN(todoNameCheckFailure);
#else
  pn = PathName("bla.");
#endif
  BOOST_TEST(!pn.hasExtension());
  pn.addExtension("txt");
  BOOST_TEST(pn.extension() == "txt");
  BOOST_TEST(pn == PathName("bla.txt"));
}

BOOST_AUTO_TEST_CASE(remove_extension)
{
  using namespace com;

  {
    PathName pn("bla");
    pn.addExtension("txt");
    pn.removeExtension();
    BOOST_TEST(!pn.hasExtension());
    BOOST_TEST(pn.extension() == "");
  }
  {
    PathName pn("dataset1.dt2d");
    BOOST_TEST(pn.extension() == "dt2d");
    pn.removeExtension();
    BOOST_TEST(!pn.hasExtension());
    BOOST_TEST(pn.toString() == "dataset1");
  }
  {  // did crash wrong ass to d_path in removeExtension
    PathName pn("d:/habitat/maps/sample1.bil");
    BOOST_TEST(pn.extension() == "bil");
    pn.removeExtension();
    BOOST_TEST(!pn.hasExtension());

    BOOST_TEST(pn.toString() == "d:/habitat/maps/sample1");
  }
}

BOOST_AUTO_TEST_CASE(add)
{
  using namespace com;

  PathName pn;
  std::string result;

  // "" + ""
  pn = PathName("") + PathName("");
  result = "";
  BOOST_TEST(pn == result);

  // "/" + ""
  pn = PathName(d_slash) + PathName("");
  result = d_slash;
  BOOST_TEST(pn == result);

  // "" + "/"
  pn = PathName("") + PathName(d_slash);
  result = d_slash;
  BOOST_TEST(pn == result);

  // "/" + "/"
  pn = PathName(d_slash) + PathName(d_slash);
  // "/" absolute
  result = d_slash;  // +d_slash;
  BOOST_TEST(pn.toString() == result);

  // "bla" + ""
  pn = PathName("bla") + PathName("");
  result = "bla" + d_slash;  // "bla";
  BOOST_TEST(pn == result);

  // "" + "bla"
  pn = PathName("") + PathName("bla");
  result = "bla";
  BOOST_TEST(pn == result);

  // "bla" + "/"
  pn = PathName("bla") + PathName(d_slash);
  //#ifdef WIN32
  //result = "bla" + d_slash;
  result = d_slash;
  BOOST_TEST(pn == result);
  //#else
  //  result = "bla";
  //#endif

  // "/" + "bla"
  pn = PathName(d_slash) + PathName("bla");
  result = d_slash + "bla";
  BOOST_TEST(pn == result);

  // "bla" + "bli"
  pn = PathName("bla") + PathName("bli");
  result = "bla" + d_slash + "bli";
  BOOST_TEST(pn == result);

  // "bla" + "/bli"
  pn = PathName("bla") + PathName(d_slash + "bli");
  result = d_slash + "bli";  // "bla" + d_slash + "bli";
  BOOST_TEST(pn == result);

  // "bla/" + "bli"
  pn = PathName("bla" + d_slash) + PathName("bli");
  result = "bla" + d_slash + "bli";
  BOOST_TEST(pn == result);

  // "bla/" + "/bli"
  pn = PathName("bla" + d_slash) + PathName(d_slash + "bli");
  result = d_slash + "bli";  // "bla" + d_slash + d_slash + "bli";
  BOOST_TEST(pn == result);

  // "/bla" + "bli"
  pn = PathName(d_slash + "bla") + PathName("bli");
  result = d_slash + "bla" + d_slash + "bli";
  BOOST_TEST(pn == result);

  // "bla" + "/bli"
  pn = PathName("bla") + PathName(d_slash + "bli");
  result = d_slash + "bli";  //"bla" + d_slash + "bli";
  BOOST_TEST(pn == result);

  // "/bla" + "/bli"
  pn = PathName(d_slash + "bla") + PathName(d_slash + "bli");
  result = d_slash + "bli";  //d_slash + "bla" + d_slash + "bli";
  BOOST_TEST(pn == result);

#ifdef WIN32

  // "" + "c:"
  pn = PathName("") + PathName("c:");
  result = "c:";
  BOOST_TEST(pn == result);

  // "bla" + "c:"
  pn = PathName("bla") + PathName("c:");
  BOOST_TEST(pn == "c:");  //"bla\\c:");

  // "/bla" + "c:"
  pn = PathName(d_slash + "bla") + PathName("c:");
  result = "c:";  //\\bla\\c:";
  BOOST_TEST(pn == result);

  // "c:" + "d:"
  pn = PathName("c:") + PathName("d:");
  result = "d:";  //"c:d:";
  BOOST_TEST(pn.toString() == result);

  // "c:" + "bla"
  pn = PathName("c:") + PathName("bla");
  result = "c:bla";
  BOOST_TEST(pn == result);

  // "c:" + "/bla"
  pn = PathName("c:") + PathName(d_slash + "bla");
  result = "c:" + d_slash + "bla";
  BOOST_TEST(pn == result);

  // "c:/" + "bla"
  pn = PathName("c:" + d_slash) + PathName("bla");
  result = "c:" + d_slash + "bla";
  BOOST_TEST(pn == result);

#endif
}

BOOST_AUTO_TEST_CASE(up)
{
  using namespace com;

  std::string result;
  PathName pn;

  // "" -> ""
  pn = PathName("");
  pn.up();
  result = "";
  BOOST_TEST(pn == result);

  // "foo" -> ""
  pn = "foo";
  pn.up();
  result = "";
  BOOST_TEST(pn == result);

  // "/" -> ""
  pn = PathName(d_slash);
  BOOST_TEST(pn.hasBaseName() == false);
  pn.up();
  result = d_slash;  // "";
  BOOST_TEST(pn == result);

  // "/foo" -> "/"
  pn = PathName(d_slash + "foo");
  pn.up();
  result = d_slash;
  BOOST_TEST(pn == result);

  // "foo/bar" -> "foo"
  pn = "foo";
  pn += "bar";
  pn.up();
  result = "foo";
  BOOST_TEST(pn == result);

  // "/foo/bar" -> "/foo"
  pn = PathName(d_slash + "foo" + d_slash + "bar");
  pn.up();
  result = d_slash + "foo";
  BOOST_TEST(pn == result);

#ifdef WIN32

  // "c:"
  pn = PathName("c:");
  pn.up();
  result = "c:";  // "";
  BOOST_TEST(pn == result);

  // "c:\" -> ""
  pn = PathName("c:" + d_slash);
  pn.up();
  result = "c:\\";  //"c:";
  BOOST_TEST(pn == result);


  // "c:bla" -> "c:"
  pn = PathName("c:bla");
  pn.up();
  result = "c:";
  BOOST_TEST(pn == result);

  pn = PathName("c:bla" + d_slash);
  pn.up();
  BOOST_TEST(pn == "c:bla");

  // "c:\bla" -> "c:\"
  pn = PathName("c:" + d_slash + "bla");
  pn.up();
  result = "c:" + d_slash;
  BOOST_TEST(pn == result);

  // "c:\bla" -> "c:\"
  pn = PathName("c:" + d_slash + "bla" + d_slash);
  pn.up();
  result = "c:" + d_slash + "bla";
  BOOST_TEST(pn == result);

  // "c:\\" -> ""
  try {
    pn = PathName("c:" + d_slash + d_slash);
  } catch (...) {
    ;
  }
  bool todoCheckDoubleSlash = false;
  BOOST_TEST_WARN(todoCheckDoubleSlash);
  pn.up();
  result = "c:\\\\";  // "c:\\";
  BOOST_TEST(pn == result);

#endif
}

BOOST_AUTO_TEST_CASE(make_native)
{
  using namespace com;

  std::string result;
  PathName pn;

  // "/usr/local/bin"
  pn = PathName("usr/local/bin");
  pn.makeNative();
  result = "usr" + d_slash + "local" + d_slash + "bin";

  BOOST_TEST(pn == result);
}

BOOST_AUTO_TEST_CASE(compare)
{
  using namespace com;

  PathName const path1("abc");
  PathName const path2("def");
  PathName const path3("DEF");
  PathName const path4("abc1");
  PathName const path5("abc2");


  BOOST_TEST(path1.compare(path2) < 0);
  BOOST_TEST(path4.compare(path5) < 0);

#ifdef WIN32
  BOOST_TEST(path2.compare(path3) == 0);
#else
  BOOST_TEST(path2.compare(path3) > 0);
#endif
}

BOOST_AUTO_TEST_CASE(clear)
{
  using namespace com;

#ifdef WIN32
  PathName pn("C:\\pcrcalc\\testdir");
#else
  PathName pn("/home/cees/pcrtree/apps/pcrcalc/testdir");
#endif
  BOOST_TEST(!pn.isEmpty());
  pn.clear();
  BOOST_TEST(pn.isEmpty());
}

BOOST_AUTO_TEST_CASE(equals)
{
  using namespace com;

#ifdef WIN32
  PathName dirNoEndSlash("C:\\pcrcalc\\testdir");
  PathName dirEndSlash("C:\\pcrcalc\\testdir\\");
#else
  PathName const dirNoEndSlash("/home/cees/pcrtree/apps/pcrcalc/testdir");
  PathName const dirEndSlash("/home/cees/pcrtree/apps/pcrcalc/testdir/");
#endif

  // Since the constructor of PathName tries not to edit the input string
  // both paths above are not equal.
  BOOST_TEST(!dirEndSlash.equals(dirNoEndSlash));

  PathName const cd(currentWorkingDirectory());
  PathName travel(cd);
  travel += "sub1";
  travel.up();
  BOOST_TEST(cd.equals(travel));
}

BOOST_AUTO_TEST_CASE(unc)
{
  using namespace com;

#ifndef WIN32
  bool const notWin32 = true;
  if (notWin32) {
    return;
  }
#endif

  PathName const pn(R"(\\P4\bin)");
  std::filesystem::path const bp("//P4/bin");
  BOOST_TEST(bp.root_name() == "//P4");
  BOOST_TEST(pn.directoryName() == "\\\\P4\\");
}
