#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_TABLETEST
#include "com_tabletest.h"
#define INCLUDED_COM_TABLETEST
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
#ifndef INCLUDED_COM_TABLE
#include "com_table.h"
#define INCLUDED_COM_TABLE
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif

/*!
  \file
  This file contains the implementation of the TableTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::TableTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TableTest> instance(new TableTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TableTest::testTextFormat, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TABLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::TableTest::TableTest()
{
}



//! setUp
void com::TableTest::setUp()
{
}

//! tearDown
void com::TableTest::tearDown()
{
}



void com::TableTest::testTextFormat()
{

  com::Table tab;
  std::ifstream ifs;
  // UNIX line feeds
  com::open(ifs,"zinc.unix.eas");

  ifs >> tab;


  BOOST_CHECK(tab.nrRecs()==155);
  BOOST_CHECK(tab.nrCols()==3);

  // first 2 lines
  BOOST_CHECK(tab.value(0,0)==181072);
  BOOST_CHECK(tab.value(1,0)==333611);
  BOOST_CHECK(tab.value(2,0)==1022);
  BOOST_CHECK(tab.value(0,1)==181025);
  BOOST_CHECK(tab.value(1,1)==333558);
  BOOST_CHECK(tab.value(2,1)==1141);

  // last
  BOOST_CHECK(tab.value(0,154)==180627);
  BOOST_CHECK(tab.value(1,154)==330190);
  BOOST_CHECK(tab.value(2,154)==375);
}
