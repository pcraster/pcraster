#define BOOST_TEST_MODULE pcraster com directory
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_directory.h"
#include "com_exception.h"
#include "com_pathinfo.h"
#include "com_pathname.h"
#include <iostream>


BOOST_AUTO_TEST_CASE(create_directory)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(erase_directory)
{
  using namespace com;

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
