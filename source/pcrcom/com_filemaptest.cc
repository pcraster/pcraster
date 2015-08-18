#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_FILEMAPTEST
#include "com_filemaptest.h"
#define INCLUDED_COM_FILEMAPTEST
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

// Module headers.
#ifndef INCLUDED_COM_FILEMAP
#include "com_filemap.h"
#define INCLUDED_COM_FILEMAP
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
/*!
  \file
  This file contains the implementation of the FileMapTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FILEMAP MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::FileMapTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FileMapTest> instance(new FileMapTest());

  suite->add(BOOST_CLASS_TEST_CASE(&FileMapTest::testEmptyFile, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FileMapTest::testIterators, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&FileMapTest::fileMapToLarge, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF FILEMAP MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::FileMapTest::FileMapTest(){
}



//! setUp
void com::FileMapTest::setUp()
{
}

//! tearDown
void com::FileMapTest::tearDown()
{
}

void com::FileMapTest::testEmptyFile()
{
    com::PathName pn("empty.filemap");
    com::create(pn);
#ifdef WIN32
    com::FileMap fm(pn);
    BOOST_CHECK(fm.begin() == fm.pointer());
    BOOST_CHECK(fm.begin() == fm.end());
#else
    // linux no mmap call with length 0 allowed since 2.6.something
    bool catched=false;
    try {
     com::FileMap fm(pn);
    } catch(const com::OpenFileError& e) {
      catched=true;
      BOOST_CHECK(e.messages().find(
         "mmap does not support 0 sized files") != std::string::npos);
    }
    BOOST_CHECK(catched);
#endif
}

void com::FileMapTest::testIterators()
{

  // read some stuff
  const char *files[2] = {"zinc.unix.eas","zinc.dos.eas"};
  std::vector<std::string> header;
  header.push_back("Zinc measurements on River Meuse flood plains");
  header.push_back("3");
  header.push_back("xcoord, m");
  header.push_back("ycoord, m");
  header.push_back("zinc, ppm");
  header.push_back("181072 333611 1022");

  for(size_t i=0; i<2; i++) {
    com::PathName pn(files[i]);
    com::FileMap  fm(pn);
    std::string   contents(fm.begin(),fm.end());
    std::vector<std::string> lines(com::split(contents,'\n'));
    BOOST_CHECK(lines.size() == 5+155);
    for(size_t l=0; l<header.size(); l++) {
      if (lines[l][0] == '\r')
        lines[l].erase(0,1);
      if (!lines[l].empty()) {
        size_t last=lines[l].size()-1;
        if (lines[l][last] == '\r')
          lines[l].erase(last,1);
       }
      BOOST_CHECK(header[l] == lines[l]);
    }
  }
}

void com::FileMapTest::fileMapToLarge()
{
#ifdef WIN32
  com::PathName big("E:\\gam_allXL.xyz");
#else
  com::PathName big("/home/cees/tmp/gam_allXL.xyz");
#endif
  if (com::exists(big)) {
    BOOST_CHECK(size(big) > gigaByte<size_t>(2));
    testOpenForReading(big);
    bool catched(false);
    try {
     FileMap n(big);
    } catch (const com::OpenFileError& e) {
      catched=true;

      BOOST_CHECK(e.messages().find("Too large to map in memory")!=std::string::npos);
    }
    BOOST_CHECK(catched);
  }
}
