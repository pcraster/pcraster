#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PATHNAMETEST
#include "com_pathnametest.h"
#define INCLUDED_COM_PATHNAMETEST
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

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*com::PathNameTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PathNameTest> instance(new PathNameTest());

  // These are the mothers of all tests in path name.
  // Test if we get back what we put in.
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testCtor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testToString, instance));
  // Test if we get the right parts of what we put in.
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testSplitFor, instance));

  // Order matters! First the tests which query the path name.
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testIsEmpty, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testIsRelative, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testExtension, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testRemoveExtension, instance));

  // Then the functions which need those query functions of PathName.
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testAdd, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testUp, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testMakeNative, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testMakeAbsolute, instance));

  // The mother test of all path name comparisons.
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testCompare, instance));

  // then the brilliant tests of Cees
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testEquals, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testClear, instance));

  suite->add(BOOST_CLASS_TEST_CASE(&PathNameTest::testUnc, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

com::PathNameTest::PathNameTest()

  :    d_slash(PathName::dirPathDelimNative())
{
}

void com::PathNameTest::setUp()
{
}

void com::PathNameTest::tearDown()
{
}

void com::PathNameTest::testCtor()
{
  com::PathName pn("../messagestest.xml");
  BOOST_CHECK(!pn.isAbsolute());

  try {
    PathName pn1(std::string("todoHaatQuoteInNaam.map\""));
  } catch(...) {
  }
  bool todoHaatQuoteInNaam = false;
  BOOST_WARN(todoHaatQuoteInNaam);
}

void com::PathNameTest::testToString()
{
  PathName pn;
  std::string result;

  // ""
  result = "";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "/"
  result = d_slash;
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "/bla"
  result = d_slash + "bla";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "//bla"
  result = d_slash + d_slash + "bla";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "/bla/"
  result = d_slash + "bla";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "//bla/"
  result = d_slash + d_slash + "bla";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);


  // "/bla/bli"
  result = d_slash + "bla" + d_slash + "bli";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);


  // "/bla/bli/"
  result = d_slash + "bla" + d_slash + "bli";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "//bla/bli/"
  result = d_slash + d_slash + "bla" + d_slash + "bli";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "//bla/bli"
  result = d_slash + d_slash + "bla" + d_slash + "bli";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

#ifdef WIN32
  bool todoCheckDoubleSlash=false;
  BOOST_WARN(todoCheckDoubleSlash);
#else
  // "//bla//bli"
  result = d_slash + d_slash + "bla" + d_slash +
                   d_slash + "bli";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "//bla//bli/"
  result = d_slash + d_slash + "bla" + d_slash +
                   d_slash + "bli";
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "//bla//bli//"
  result = d_slash + d_slash + "bla" + d_slash +
                   d_slash + "bli" + d_slash + d_slash;
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "//bla//"
  result = d_slash + d_slash + "bla" + d_slash +
             d_slash;
  pn = PathName(result);
  BOOST_CHECK_EQUAL(pn.toString(), result);
#endif
}


void com::PathNameTest::testSplitFor()
{
  PathName pn;
#ifdef WIN32
  std::string drive;
#endif
  std::string directory, base;

  // ""
  pn        = PathName("");
  directory = "";
  base      = "";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "/"
  pn        = PathName(d_slash);
  directory = "";
  base      = d_slash;
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
#ifdef WIN32
  BOOST_CHECK_EQUAL(pn.baseName(), "\\");
#else
  BOOST_CHECK_EQUAL(pn.baseName(), base);
#endif

  // "//"
  pn        = PathName(d_slash + d_slash);
  directory = d_slash;
  base      = d_slash;
  BOOST_CHECK_EQUAL(pn.directoryName(), "");
#ifdef WIN32
  BOOST_CHECK_EQUAL(pn.baseName(), "\\\\");
#else
  BOOST_CHECK_EQUAL(pn.baseName(), "//");
#endif

  // "bla"
  pn        = PathName("bla");
  directory = "";
  base      = "bla";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "bla/"
  pn        = PathName("bla" + d_slash);
  directory = "bla";
  base      = ".";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "/bla"
  pn        = PathName(d_slash + "bla");
  directory = d_slash;
  base      = "bla";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "//bla"
  pn        = PathName(d_slash + d_slash + "bla");
  directory = "";
#ifdef WIN32
  base      = "\\\\bla";
#else
  base      = "//bla";
#endif
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "/bla/"
  pn        = PathName(d_slash + "bla" + d_slash);
  directory = d_slash + "bla";
  base      = ".";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "bla/bli"
  pn        = PathName("bla" + d_slash + "bli");
  directory = "bla";
  base      = "bli";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "bla/bli/"
  pn        = PathName("bla" + d_slash + "bli" + d_slash);
  directory = "bla" + d_slash + "bli";
  base      = ".";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "/bla/bli"
  pn        = PathName(d_slash + "bla" + d_slash +
                   "bli");
  directory = d_slash + "bla";
  base      = "bli";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "/bla/bli/"
  pn        = PathName(d_slash + "bla" + d_slash +
                   "bli" + d_slash);
  directory = d_slash + "bla" + d_slash + "bli";
  base      = ".";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

#ifdef WIN32
  // "c:" -> "c:"
  pn        = PathName("c:");
  directory = "";
  base       = "c:";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "c:\"
  pn        = PathName("c:" + d_slash);
  directory = "c:";
  base      = "\\";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "c:bla" -> "c:bla"
  pn        = PathName("c:bla");
  directory = "c:";
  base      = "bla";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "c:bla
  pn        = PathName("c:bla" + d_slash);
  directory = "c:";
  base      = "bla";
  BOOST_CHECK_EQUAL(pn.directoryName(), "c:bla");
  BOOST_CHECK_EQUAL(pn.baseName(), ".");

  // "c:\bla"
  pn        = PathName("c:" + d_slash + "bla");
  directory = "c:\\";
  base      = "bla";
  BOOST_CHECK_EQUAL(pn.directoryName(), directory);
  BOOST_CHECK_EQUAL(pn.baseName(), base);

  // "c:\bla\"
  pn        = PathName("c:" + d_slash + "bla" +d_slash);
  BOOST_CHECK_EQUAL(pn.directoryName(), "c:\\bla");
  BOOST_CHECK_EQUAL(pn.baseName(), ".");
#endif


  // "bla//bli"
  pn        = PathName("bla" + d_slash + d_slash + "bli");
  directory = "bla" + d_slash;
  base      = "bli";
  BOOST_CHECK_EQUAL(pn.baseName(), base);
}





void com::PathNameTest::testIsEmpty()
{
  PathName pn;
  BOOST_CHECK(pn.isEmpty());

  pn = PathName(d_slash);
  BOOST_CHECK(!pn.isEmpty());

  pn = PathName("bla");
  BOOST_CHECK(!pn.isEmpty());

#ifdef WIN32
  pn = PathName("c:");
  BOOST_CHECK(!pn.isEmpty());
#endif
}



void com::PathNameTest::testIsRelative()
{
  PathName pn;

  // ""
  pn = PathName("");
  BOOST_CHECK(pn.isRelative());

  // "foo"
  pn = PathName("foo");
  BOOST_CHECK(pn.isRelative());

  // "foo/bar"
  pn = PathName("foo" + d_slash + "bar");
  BOOST_CHECK(pn.isRelative());

  // "/"
  pn = PathName(d_slash);
#ifdef WIN32
  // On windows this is relative to the current drive.
  BOOST_CHECK(pn.isRelative());
#else
  // On unix this is the root dir.
  BOOST_CHECK(!pn.isRelative());
#endif

  // "c:\"
#ifdef WIN32
  pn = PathName("c:" + d_slash);
  BOOST_CHECK(!pn.isRelative());
#endif

  // "/foo"
  pn = PathName(d_slash + "foo");
#ifdef WIN32
  BOOST_CHECK(pn.isRelative());
#else
  BOOST_CHECK(!pn.isRelative());
#endif

  // "/foo/bar"
  pn = PathName(d_slash + "foo" + d_slash + "bar");
#ifdef WIN32
  BOOST_CHECK(pn.isRelative());
#else
  BOOST_CHECK(!pn.isRelative());
#endif

  // "/foo/bar/"
  pn = PathName(d_slash + "foo" + d_slash + "bar" +
                   d_slash);
#ifdef WIN32
  BOOST_CHECK(pn.isRelative());
#else
  BOOST_CHECK(!pn.isRelative());
#endif
}



void com::PathNameTest::testMakeAbsolute()
{
  PathName pathName;

  pathName = PathName("bla");
  BOOST_CHECK(pathName.isRelative());
  pathName.makeAbsolute();
  BOOST_CHECK(pathName.isAbsolute());
}

void com::PathNameTest::testExtension()
{
  PathName pn("bla.txt");
  BOOST_CHECK(pn.hasExtension());
  BOOST_CHECK_EQUAL(pn.extension(), "txt");
  {
    PathName pn("bla.mpeg");
    BOOST_CHECK(pn.hasExtension());
    BOOST_CHECK_EQUAL(pn.extension(), "mpeg");
  }

  pn = PathName("bla");
  BOOST_CHECK(!pn.hasExtension());
  BOOST_CHECK_EQUAL(pn.extension(), "");

  pn = PathName("");
  BOOST_CHECK(!pn.hasExtension());

  pn = PathName("bla");
  pn.addExtension("txt");
  BOOST_CHECK(pn.hasExtension());
  BOOST_CHECK_EQUAL(pn.extension(), "txt");

  pn = PathName("bla");
  pn.addExtension("");
  BOOST_CHECK(!pn.hasExtension());
  BOOST_CHECK(pn == PathName("bla"));

  {
    PathName pn("blx.mpeg");
    BOOST_CHECK(pn.hasExtension());
    pn.addExtension("gif");
    BOOST_CHECK_EQUAL(pn.extension(), "gif");
    BOOST_CHECK_EQUAL(pn, PathName("blx.mpeg.gif"));
  }
  {
    PathName pn("bla.mpeg");
    BOOST_CHECK(pn.hasExtension());
    pn.setExtension("gif");
    BOOST_CHECK_EQUAL(pn.extension(), "gif");
    BOOST_CHECK(pn == PathName("bla.gif"));
  }
  {
    PathName pn("bla.prefix1.mpeg");
    BOOST_CHECK(pn.hasExtension());
    pn.setExtension("gif");
    BOOST_CHECK_EQUAL(pn.extension(), "gif");
    BOOST_CHECK(pn == PathName("bla.prefix1.gif"));
  }
  {
    PathName pn("validated/windowdiversity.Result3.omap");
    BOOST_CHECK(pn.hasExtension());
    BOOST_CHECK_EQUAL(pn.extension(), "omap");
    pn.setExtension("map");
    BOOST_CHECK_EQUAL(pn.extension(), "map");
    BOOST_CHECK(pn ==
        PathName("validated/windowdiversity.Result3.map"));
  }

#ifdef WIN32
  try {
   pn = PathName("bla.");
  } catch (...) {
  };
  bool todoNameCheckFailure=false;
  // see boost::filesystem::name_check
  BOOST_WARN(todoNameCheckFailure);
#else
  pn = PathName("bla.");
#endif
  BOOST_CHECK(!pn.hasExtension());
  pn.addExtension("txt");
  BOOST_CHECK_EQUAL(pn.extension(), "txt");
  BOOST_CHECK(pn == PathName("bla.txt"));
}

void com::PathNameTest::testRemoveExtension()
{
  {
    PathName pn("bla");
    pn.addExtension("txt");
    pn.removeExtension();
    BOOST_CHECK(!pn.hasExtension());
    BOOST_CHECK_EQUAL(pn.extension(), "");
  }
  {
    PathName pn("dataset1.dt2d");
    BOOST_CHECK_EQUAL(pn.extension(), "dt2d");
    pn.removeExtension();
    BOOST_CHECK(!pn.hasExtension());
    BOOST_CHECK_EQUAL(pn.toString(), "dataset1");
  }
  { // did crash wrong ass to d_path in removeExtension
    PathName pn("d:/habitat/maps/sample1.bil");
    BOOST_CHECK_EQUAL(pn.extension(), "bil");
    pn.removeExtension();
    BOOST_CHECK(!pn.hasExtension());

    BOOST_CHECK_EQUAL(pn.toString(), "d:/habitat/maps/sample1");
  }
}



void com::PathNameTest::testAdd()
{
  PathName pn;
  std::string result;

  // "" + ""
  pn = PathName("") + PathName("");
  result = "";
  BOOST_CHECK(pn == result);

  // "/" + ""
  pn = PathName(d_slash) + PathName("");
  result = d_slash;
  BOOST_CHECK(pn == result);

  // "" + "/"
  pn = PathName("") + PathName(d_slash);
  result = d_slash;
  BOOST_CHECK(pn == result);

  // "/" + "/"
  pn = PathName(d_slash) + PathName(d_slash);
  // "/" absolute
  result = d_slash+d_slash;
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "bla" + ""
  pn = PathName("bla") + PathName("");
  result = "bla";
  BOOST_CHECK(pn == result);

  // "" + "bla"
  pn = PathName("") + PathName("bla");
  result = "bla";
  BOOST_CHECK(pn == result);

  // "bla" + "/"
  pn = PathName("bla") + PathName(d_slash);
#ifdef WIN32
  result = "bla" + d_slash;
  BOOST_CHECK(pn == result);
#else
  result = "bla";
#endif

  // "/" + "bla"
  pn = PathName(d_slash) + PathName("bla");
  result = d_slash + "bla";
  BOOST_CHECK(pn == result);

  // "bla" + "bli"
  pn = PathName("bla") + PathName("bli");
  result = "bla" + d_slash + "bli";
  BOOST_CHECK(pn == result);

  // "bla" + "/bli"
  pn = PathName("bla") + PathName(d_slash + "bli");
  result = "bla" + d_slash + "bli";
  BOOST_CHECK(pn == result);

  // "bla/" + "bli"
  pn = PathName("bla" + d_slash) + PathName("bli");
  result = "bla" + d_slash + "bli";
  BOOST_CHECK(pn == result);

  // "bla/" + "/bli"
  pn = PathName("bla" + d_slash) + PathName(d_slash + "bli");
  result = "bla" + d_slash + d_slash + "bli";
  BOOST_CHECK_EQUAL(pn, result);

  // "/bla" + "bli"
  pn = PathName(d_slash + "bla") + PathName("bli");
  result = d_slash + "bla" + d_slash + "bli";
  BOOST_CHECK(pn == result);

  // "bla" + "/bli"
  pn = PathName("bla") + PathName(d_slash + "bli");
  result = "bla" + d_slash + "bli";
  BOOST_CHECK(pn == result);

  // "/bla" + "/bli"
  pn = PathName(d_slash + "bla") + PathName(d_slash + "bli");
  result = d_slash + "bla" + d_slash + "bli";
  BOOST_CHECK(pn == result);

#ifdef WIN32

  // "" + "c:"
  pn = PathName("") + PathName("c:");
  result = "c:";
  BOOST_CHECK(pn == result);

  // "bla" + "c:"
  pn = PathName("bla") + PathName("c:");
  BOOST_CHECK_EQUAL(pn,"bla\\c:");

  // "/bla" + "c:"
  pn = PathName(d_slash + "bla") + PathName("c:");
  result = "\\bla\\c:";
  BOOST_CHECK_EQUAL(pn, result);

  // "c:" + "d:"
  pn = PathName("c:") + PathName("d:");
  result = "c:d:";
  BOOST_CHECK_EQUAL(pn.toString(), result);

  // "c:" + "bla"
  pn = PathName("c:") + PathName("bla");
  result = "c:bla";
  BOOST_CHECK(pn == result);

  // "c:" + "/bla"
  pn = PathName("c:") + PathName(d_slash + "bla");
  result = "c:" + d_slash + "bla";
  BOOST_CHECK(pn == result);

  // "c:/" + "bla"
  pn = PathName("c:" + d_slash) + PathName("bla");
  result = "c:" + d_slash + "bla";
  BOOST_CHECK(pn == result);

#endif
}



void com::PathNameTest::testUp()
{
  std::string result;
  PathName pn;

  // "" -> ""
  pn = PathName("");
  pn.up();
  result = "";
  BOOST_CHECK(pn == result);

  // "foo" -> ""
  pn = "foo";
  pn.up();
  result = "";
  BOOST_CHECK(pn == result);

  // "/" -> ""
  pn = PathName(d_slash);
  BOOST_CHECK(pn.hasBaseName());
  pn.up();
  result = "";
  BOOST_CHECK(pn == result);

  // "/foo" -> "/"
  pn = PathName(d_slash + "foo");
  pn.up();
  result = d_slash;
  BOOST_CHECK(pn == result);

  // "foo/bar" -> "foo"
  pn = "foo";
  pn += "bar";
  pn.up();
  result = "foo";
  BOOST_CHECK(pn == result);

  // "/foo/bar" -> "/foo"
  pn = PathName(d_slash + "foo" + d_slash + "bar");
  pn.up();
  result = d_slash + "foo";
  BOOST_CHECK(pn == result);

#ifdef WIN32

  // "c:"
  pn = PathName("c:");
  pn.up();
  result = "";
  BOOST_CHECK(pn == result);

  // "c:\" -> ""
  pn = PathName("c:" + d_slash);
  pn.up();
  result = "c:";
  BOOST_CHECK(pn == result);


  // "c:bla" -> "c:"
  pn = PathName("c:bla");
  pn.up();
  result = "c:";
  BOOST_CHECK(pn == result);

  pn = PathName("c:bla" + d_slash);
  pn.up();
  BOOST_CHECK_EQUAL(pn,"c:bla");

  // "c:\bla" -> "c:\"
  pn = PathName("c:" + d_slash + "bla");
  pn.up();
  result = "c:" + d_slash;
  BOOST_CHECK_EQUAL(pn,result);

  // "c:\bla" -> "c:\"
  pn = PathName("c:" + d_slash + "bla" + d_slash);
  pn.up();
  result = "c:" + d_slash + "bla";
  BOOST_CHECK_EQUAL(pn,result);

  // "c:\\" -> ""
  try {
  pn = PathName("c:" + d_slash + d_slash);
  } catch (...) {
    ;
  }
  bool todoCheckDoubleSlash=false;
  BOOST_WARN(todoCheckDoubleSlash);
  pn.up();
  result = "c:\\";
  BOOST_CHECK(pn == result);

#endif
}

void com::PathNameTest::testMakeNative()
{
  std::string result;
  PathName pn;

  // "/usr/local/bin"
  pn = PathName("usr/local/bin");
  pn.makeNative();
  result = "usr" + d_slash + "local" + d_slash + "bin";

  BOOST_CHECK(pn == result);
}

void com::PathNameTest::testCompare()
{
  PathName path1("abc");
  PathName path2("def");
  PathName path3("DEF");
  PathName path4("abc1");
  PathName path5("abc2");


  BOOST_CHECK(path1.compare(path2) < 0);
  BOOST_CHECK(path4.compare(path5) < 0);

#ifdef WIN32
  BOOST_CHECK(path2.compare(path3) == 0);
#else
  BOOST_CHECK(path2.compare(path3) > 0);
#endif
}

void com::PathNameTest::testClear()
{
#ifdef WIN32
  PathName pn("C:\\pcrcalc\\testdir");
#else
  PathName pn("/home/cees/pcrtree/apps/pcrcalc/testdir");
#endif
 BOOST_CHECK(!pn.isEmpty());
 pn.clear();
 BOOST_CHECK(pn.isEmpty());
}


void com::PathNameTest::testEquals()
{
#ifdef WIN32
  PathName dirNoEndSlash("C:\\pcrcalc\\testdir");
  PathName dirEndSlash("C:\\pcrcalc\\testdir\\");
#else
  PathName dirNoEndSlash("/home/cees/pcrtree/apps/pcrcalc/testdir");
  PathName dirEndSlash("/home/cees/pcrtree/apps/pcrcalc/testdir/");
#endif

  // Since the constructor of PathName tries not to edit the input string
  // both paths above are not equal.
  BOOST_CHECK(!dirEndSlash.equals(dirNoEndSlash));

  PathName cd(currentWorkingDirectory());
  PathName travel(cd);
  travel += "sub1";
  travel.up();
  BOOST_CHECK(cd.equals(travel));
}

void com::PathNameTest::testUnc()
{
  #ifndef WIN32
  bool notWin32=true;
  if (notWin32)
     return;
  #endif

  PathName pn("\\\\P4\\bin");
  boost::filesystem::path bp("//P4/bin");
  BOOST_CHECK_EQUAL(bp.root_name(), "//P4");
  BOOST_CHECK_EQUAL(pn.directoryName(), "\\\\P4\\");


}
