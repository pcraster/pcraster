#ifndef INCLUDED_DAL_TABLETEST
#include "dal_TableTest.h"
#define INCLUDED_DAL_TABLETEST
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
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif



/*!
  \file
  This file contains the implementation of the TableTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*TableTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TableTest> instance(new TableTest());

  suite->add(BOOST_CLASS_TEST_CASE(
         &TableTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &TableTest::testCopy, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &TableTest::testAssign, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TABLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
TableTest::TableTest()
{
}



//! setUp
void TableTest::setUp()
{
  d_empty = new Table();

  std::vector<std::string> titles;
  std::vector<TypeId> typeIds;

  typeIds.push_back(TI_UINT1);
  typeIds.push_back(TI_INT4);
  typeIds.push_back(TI_NR_TYPES);
  typeIds.push_back(TI_REAL4);

  titles.push_back("UINT1");
  titles.push_back("INT4");
  titles.push_back("NR_TYPES");
  titles.push_back("REAL4");

  d_table1 = new Table("Table1", titles, typeIds);
  d_table1->createCols();

  Array<UINT1>& col1(d_table1->col<UINT1>(0));
  col1.push_back(1);
  col1.push_back(0);
  col1.push_back(1);

  Array<INT4>& col2(d_table1->col<INT4>(1));
  col2.push_back(3);
  col2.push_back(33);
  col2.push_back(333);

  Array<REAL4>& col4(d_table1->col<REAL4>(3));
  col4.push_back(REAL4(3.3));
  col4.push_back(REAL4(4.4));
  col4.push_back(REAL4(5.5));
}



//! tearDown
void TableTest::tearDown()
{
  delete d_table1;
  delete d_empty;
}



void TableTest::testEmptyTable(
         Table const& table)
{
  BOOST_CHECK_EQUAL(table.nrCols(), size_t(0));
  BOOST_CHECK_EQUAL(table.nrRecs(), size_t(0));
  BOOST_CHECK(table.title().empty());
}



void TableTest::testTable1(
         Table const& table)
{
  BOOST_CHECK_EQUAL(table.nrCols(), size_t(4));
  BOOST_CHECK_EQUAL(table.nrRecs(), size_t(3));
  BOOST_CHECK_EQUAL(table.title(), "Table1");
  BOOST_CHECK_EQUAL(table.typeId(0), TI_UINT1);
  BOOST_CHECK_EQUAL(table.typeId(1), TI_INT4);
  BOOST_CHECK_EQUAL(table.typeId(2), TI_NR_TYPES);
  BOOST_CHECK_EQUAL(table.typeId(3), TI_REAL4);
  BOOST_CHECK_EQUAL(table.title(0), "UINT1");
  BOOST_CHECK_EQUAL(table.title(1), "INT4");
  BOOST_CHECK_EQUAL(table.title(2), "NR_TYPES");
  BOOST_CHECK_EQUAL(table.title(3), "REAL4");

  Array<UINT1> const& col1(table.col<UINT1>(0));
  BOOST_CHECK(dal::comparable(col1[0], UINT1(1)));
  BOOST_CHECK(dal::comparable(col1[1], UINT1(0)));
  BOOST_CHECK(dal::comparable(col1[2], UINT1(1)));

  Array<INT4> const& col2(table.col<INT4>(1));
  BOOST_CHECK(dal::comparable(col2[0], INT4(3)));
  BOOST_CHECK(dal::comparable(col2[1], INT4(33)));
  BOOST_CHECK(dal::comparable(col2[2], INT4(333)));

  Array<REAL4> const& col4(table.col<REAL4>(3));
  BOOST_CHECK(dal::comparable(col4[0], REAL4(3.3)));
  BOOST_CHECK(dal::comparable(col4[1], REAL4(4.4)));
  BOOST_CHECK(dal::comparable(col4[2], REAL4(5.5)));
}



void TableTest::testConstructor()
{
  setUp();
  // empty
  testEmptyTable(*d_empty);

  // table1
  testTable1(*d_table1);
  tearDown();
}



void TableTest::testCopy()
{
  setUp();
  // Copy construct empty.
  {
    Table copy(*d_empty);
    testEmptyTable(copy);
  }

  // Copy construct table1.
  {
    Table copy(*d_table1);
    testTable1(copy);
  }

  // Assign empty to table1.
  {
    Table copy(*d_table1);
    copy = *d_empty;
    testEmptyTable(copy);
  }

  // Assign table1 to empty.
  {
    Table copy(*d_empty);
    copy = *d_table1;
    testTable1(copy);
  }
  tearDown();
}



void TableTest::testAssign()
{
  {
    Table table1, table2;

    table1.appendCol<INT4>();
    table1.appendCol<INT4>();

    size_t joinFrom = 0;
    size_t colToWrite = 1;

    Array<INT4>& ids1(table1.col<INT4>(joinFrom));
    ids1.resize(5);
    ids1[0] = 1;
    ids1[1] = 3;
    ids1[2] = 5;
    ids1[3] = 7;
    ids1[4] = 9;

    Array<INT4>& values1(table1.col<INT4>(colToWrite));
    values1.resize(5);
    values1[0] = 5;
    values1[1] = 5;
    values1[2] = 5;
    pcr::setMV(values1[3]);
    values1[4] = 5;

    table2.appendCol<INT4>();
    table2.appendCol<INT4>();

    size_t joinTo = 1;
    size_t colToRead = 0;

    Array<INT4>& ids2(table2.col<INT4>(joinTo));
    ids2.resize(5);
    ids2[0] = 9;
    ids2[1] = 3;
    ids2[2] = 1;
    ids2[3] = 7;
    ids2[4] = 5;

    Array<INT4>& values2(table2.col<INT4>(colToRead));
    values2.resize(5);
    values2[0] = 4;
    pcr::setMV(values2[1]);
    values2[2] = 3;
    values2[3] = 2;
    values2[4] = 1;

    table1.assign(colToWrite, joinFrom, table2, colToRead, joinTo);

    BOOST_CHECK_EQUAL(values1[0], 3);
    BOOST_CHECK(pcr::isMV(values1[1]));
    BOOST_CHECK_EQUAL(values1[2], 1);
    BOOST_CHECK_EQUAL(values1[3], 2);
    BOOST_CHECK_EQUAL(values1[4], 4);
  }
}

} // namespace dal

