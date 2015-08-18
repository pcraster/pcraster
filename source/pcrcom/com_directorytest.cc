#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_DIRECTORYTEST
#include "com_directorytest.h"
#define INCLUDED_COM_DIRECTORYTEST
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

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*com::DirectoryTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DirectoryTest> instance(new DirectoryTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DirectoryTest::testCreateDirectory, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DirectoryTest::testEraseDirectory, instance));
  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

com::DirectoryTest::DirectoryTest()
{
}

void com::DirectoryTest::setUp()
{
}

void com::DirectoryTest::tearDown()
{
}

void com::DirectoryTest::testCreateDirectory()
{
  try
  {
    PathName pn;
    PathInfo pi;
    Directory dir;

    pn = PathName("dir_dir1") + PathName("dir2") + PathName("dir3");
    pi.setPathName(pn);
    dir.setPathName(pn);
    BOOST_CHECK(!pi.exists());
    dir.create(true);
    BOOST_CHECK(pi.isDirectory());

    pn = PathName("dir_dir1") + PathName("dir2") + PathName("dir3") +
                   PathName("dir4");
    pi.setPathName(pn);
    dir.setPathName(pn);
    BOOST_CHECK(!pi.exists());
    dir.create(false);
    BOOST_CHECK(pi.isDirectory());

    // cannot create directory with name of
    //  existing file
    bool catched = false;
    try
    {
      pn = PathName("pi_failureExpectedMkDir");
      pi.setPathName(pn);
      dir.setPathName(pn);
      BOOST_CHECK(pi.exists());
      dir.create(false);
      BOOST_CHECK(0); // never go here
    }
    catch(const OpenFileError& )
    {
      catched = true;
    }
    BOOST_CHECK(catched);

    // throw for not existing parent dir
    catched = false;
    try
    {
      pn = "failureExpectedDir/yeah";
      pi.setPathName(pn);
      dir.setPathName(pn);
      BOOST_CHECK(!pi.exists());
      dir.create(false);
      BOOST_CHECK(0); // never go here
    }
    catch(const OpenFileError& )
    {
      catched = true;
    }
    BOOST_CHECK(catched);
  }
  catch(const OpenFileError& )
  {
    BOOST_CHECK(0);
  }
}



void com::DirectoryTest::testEraseDirectory()
{
  try {
    PathName pn;
    PathInfo pi;
    Directory dir;

    // testCreateDirectory created dir_dir1/dir2/dir3/dir4
    // Erase the dir4 subdir.
    pn = PathName("dir_dir1") + PathName("dir2") + PathName("dir3");
    pi.setPathName(pn);
    dir.setPathName(pn);
    BOOST_CHECK(pi.exists());
    dir.erase(PathName("dir4"), false);
    BOOST_CHECK(pi.exists());

    // Now we have dir_dir1/dir2/dir3. Let's erase the whole tree.
    pn = PathName("dir_dir1");
    pi.setPathName(pn);
    dir.setPathName(pn);
    BOOST_CHECK(pi.exists());
    dir.erase(true);
    BOOST_CHECK(!pi.exists());

    pn=PathName("dir_removeSub");
    pn+="rmThis";
    pi.setPathName(pn);
    dir.setPathName(pn);
    BOOST_CHECK(pi.exists());
    dir.erase(true);
    BOOST_CHECK(!pi.exists());
    pi.setPathName("dir_removeSub");
    BOOST_CHECK(pi.exists());

  }
  catch(FileError& ) {
    const bool expectNoException(false);
    BOOST_CHECK(expectNoException);
  }
}
