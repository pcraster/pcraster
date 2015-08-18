#ifndef INCLUDED_DAL_MEMORYTABLEDRIVERTEST
#include "dal_MemoryTableDriverTest.h"
#define INCLUDED_DAL_MEMORYTABLEDRIVERTEST
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
#ifndef INCLUDED_DAL_MEMORYDATAPOOL
#include "dal_MemoryDataPool.h"
#define INCLUDED_DAL_MEMORYDATAPOOL
#endif

#ifndef INCLUDED_DAL_MEMORYTABLEDRIVER
#include "dal_MemoryTableDriver.h"
#define INCLUDED_DAL_MEMORYTABLEDRIVER
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif



/*!
  \file
  This file contains the implementation of the MemoryTableDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*MemoryTableDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MemoryTableDriverTest> instance(new MemoryTableDriverTest());

  suite->add(BOOST_CLASS_TEST_CASE(&MemoryTableDriverTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! ctor
MemoryTableDriverTest::MemoryTableDriverTest()
{
}



//! setUp
void MemoryTableDriverTest::setUp()
{
  std::string title;
  std::vector<std::string> titles;
  std::vector<TypeId> typeIds;

  title = "Table1";
  titles.push_back("UINT1");
  titles.push_back("INT4");
  titles.push_back("NR_TYPE_IDS");
  titles.push_back("REAL4");
  typeIds.push_back(TI_UINT1);
  typeIds.push_back(TI_INT4);
  typeIds.push_back(TI_NR_TYPES);
  typeIds.push_back(TI_REAL4);
  d_table1 = new Table(title, titles, typeIds);

  d_table1->createCols();
  Array<UINT1> col1(d_table1->col<UINT1>(0));
  Array<INT4> col2(d_table1->col<INT4>(1));
  Array<REAL4> col4(d_table1->col<REAL4>(3));
  col1.push_back(1);
  col1.push_back(0);
  col1.push_back(1);
  col2.push_back(1);
  col2.push_back(2);
  col2.push_back(3);
  col4.push_back(REAL4(1.1));
  col4.push_back(REAL4(2.2));
  col4.push_back(REAL4(3.3));

  title = "Table2";
  titles.clear();
  typeIds.clear();
  d_table2 = new Table(title, titles, typeIds);

  title = "Table3";
  titles.clear();
  typeIds.clear();
  d_table3 = new Table(title, titles, typeIds);
}



//! tearDown
void MemoryTableDriverTest::tearDown()
{
  delete d_table3;
  delete d_table2;
  delete d_table1;
}



void MemoryTableDriverTest::test()
{
  setUp();
  MemoryDataPool pool;
  MemoryTableDriver driver(&pool);

  BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists("table1"));

  dynamic_cast<TableDriver&>(driver).write(*d_table1, "table1");
  BOOST_CHECK(dynamic_cast<Driver&>(driver).exists("table1"));

  Table* table = dynamic_cast<TableDriver&>(driver).read("table1");
  BOOST_CHECK(*d_table1 == *table);
  BOOST_CHECK(d_table1 != table);
  delete table;
  tearDown();
}

} // namespace dal

