#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_TABLEINFOTEST
#include "com_tableinfotest.h"
#define INCLUDED_COM_TABLEINFOTEST
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

// Module headers.
#ifndef INCLUDED_COM_TABLEINFO
#include "com_tableinfo.h"
#define INCLUDED_COM_TABLEINFO
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif


/*!
  \file
  This file contains the implementation of the TableInfoTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLEINFO MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::TableInfoTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TableInfoTest> instance(new TableInfoTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TableInfoTest::testAvailable, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TableInfoTest::testDetect, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TableInfoTest::testBigDetect, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TABLEINFO MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::TableInfoTest::TableInfoTest()
{
}



//! setUp
void com::TableInfoTest::setUp()
{
}

//! tearDown
void com::TableInfoTest::tearDown()
{
}

void com::TableInfoTest::testAvailable()
{
 PathName pn("failureExpected.notExitst.txt");
 bool catched=false;
 try {
   TableInfo tab(pn);
 } catch (const com::Exception& e) {
   BOOST_CHECK(e.messages().find("No such file") != std::string::npos);
   catched=true;
 }
 BOOST_CHECK(catched);
}

/*! detect file format, nr columsn etc.
 */
void com::TableInfoTest::testDetect()
{
 {
  const char *files[2] = {"zinc.unix.eas","zinc.dos.eas"};
  for(size_t i=0; i < 2; i++) {
    TableInfo tab(files[i]);
    BOOST_CHECK(tab.layout() == TableInfo::GEO_EAS);
  }
 }

 PathName pn("tableInfoTest.txt");

 { write("1\n2\n3\n",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::PLAIN_TXT);
   BOOST_CHECK(tab.nrColumns() == 1);
 }
 { write("0.5\n1\n8 \n 3",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::PLAIN_TXT);
   BOOST_CHECK(tab.nrColumns() == 1);
 }
 { write("\n2\n\n0",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::PLAIN_TXT);
   BOOST_CHECK(tab.nrColumns() == 1);
 }
 { // geo eas with no data
   write("\n2\n\ntitle2 ",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::GEO_EAS);
   BOOST_CHECK(tab.nrColumns() == 2);
   BOOST_CHECK(tab.columnNames()[0].empty());
   BOOST_CHECK(tab.columnNames()[1]=="title2");
 }
 { // geo eas with numeric descriptions and a single line of data
   write("1\n2\n0\n0\n2 3",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::GEO_EAS);
   BOOST_CHECK(tab.nrColumns() == 2);
   BOOST_CHECK(tab.columnNames()[0]=="0");
   BOOST_CHECK(tab.columnNames()[1]=="0");
 }
 { // geo eas but short on column names
   write("description\n3\ntitle1\ntitle2\n",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::UNKNOWN);
 }
 { // text with single line of data
   write("0",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::PLAIN_TXT);
   BOOST_CHECK(tab.nrColumns() == 1);
   BOOST_CHECK(tab.columnNames()[0].empty());
 }
 { // text with 2 lines of data but irregular
   write("0\n1 2",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::PLAIN_TXT);
 }
 { // file with only space
   write(" \t \n  ",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::EMPTY);
   BOOST_CHECK(tab.nrColumns() == 0);
 }
 { // empty file, 0 bytes
   write("",pn);
   BOOST_CHECK(size(pn)==0);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::EMPTY);
   BOOST_CHECK(tab.nrColumns() == 0);
 }
 { // not a table file
   TableInfo tab(com::PathName("spawnScript"));
   BOOST_CHECK(tab.layout() == TableInfo::UNKNOWN);
 }
 { // geoEas with incorrect data (1 record too long)
   com::write("geoEas d\n2\nt1\nt2\n1 2 3\n4 5 \n",pn);
   TableInfo tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::GEO_EAS);
 }
}

void com::TableInfoTest::testBigDetect()
{
#ifdef WIN32
  com::PathName big("E:\\gam_allXL.xyz");
#else
  com::PathName big("/home/cees/tmp/gam_allXL.xyz");
#endif
  if (com::exists(big)) {
   BOOST_CHECK(size(big) > (1<<31)-2);
   TableInfo tab(big);
   BOOST_CHECK(tab.layout() == TableInfo::PLAIN_TXT);
   BOOST_CHECK(tab.nrColumns() == 3);
  }
}
