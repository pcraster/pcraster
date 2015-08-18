#ifndef INCLUDED_DAL_TEXTTABLEDRIVERTEST
#include "dal_TextTableDriverTest.h"
#define INCLUDED_DAL_TEXTTABLEDRIVERTEST
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
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_TEXTTABLEDRIVER
#include "dal_TextTableDriver.h"
#define INCLUDED_DAL_TEXTTABLEDRIVER
#endif



/*!
  \file
  This file contains the implementation of the TextTableDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEXTTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::TextTableDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TextTableDriverTest> instance(new TextTableDriverTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testDescription, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testUnexisting, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testEmpty, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testInvalidGrammar, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testTable1, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testTable2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testTable5, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testTable6, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testTable2eas, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testDOSFormattedTable, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testColumnWithEmptyValues, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TextTableDriverTest::testColumnWithQuiteSomeZeros, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TEXTTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::TextTableDriverTest::TextTableDriverTest()
{
}



//! setUp
void dal::TextTableDriverTest::setUp()
{
}



//! tearDown
void dal::TextTableDriverTest::tearDown()
{
}



void dal::TextTableDriverTest::testDescription()
{
  TextTableDriver driver;
  BOOST_CHECK_EQUAL(driver.description(), "Text table file format");
}



void dal::TextTableDriverTest::testUnexisting()
{
  std::string filename = "unexisting";
  TextTableDriver driver;
  bool exceptionCaught;

  Table* table = driver.open(boost::filesystem::path(filename));
  BOOST_CHECK(!table);

  try {
    exceptionCaught = false;
    table = dynamic_cast<TableDriver const&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_CHECK_EQUAL(exception.message(),
       "Data source " + filename + "(table):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
}



void dal::TextTableDriverTest::testEmpty()
{
  std::string filename = "emptyfile";
  TextTableDriver driver;
  bool exceptionCaught;

  Table* table = driver.open(boost::filesystem::path(filename));
  BOOST_CHECK(!table);

  try {
    exceptionCaught = false;
    table = dynamic_cast<TableDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_CHECK_EQUAL(exception.message(),
       "Data source " + filename + "(table):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
}



void dal::TextTableDriverTest::testInvalidGrammar()
{
  std::string filename = ":/:/:/:/:";
  TextTableDriver driver;
  bool exceptionCaught=false;

  Table* table=0;

  try {
    exceptionCaught = false;
    table = driver.open(boost::filesystem::path(filename));
#ifndef WIN32
    BOOST_CHECK(!table);
#endif
    table = dynamic_cast<TableDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
// #ifdef WIN32
//     BOOST_CHECK_EQUAL(exception.message(),
//        "Pathname '" + filename + "': Not valid on the current platform");
// #else
    BOOST_CHECK_EQUAL(exception.message(),
       "Data source " + filename + "(table):\ncannot be opened");
// #endif
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
}



void dal::TextTableDriverTest::testTable1()
{
  std::string filename = "table1.col";
  TextTableDriver driver;
  Table* table;

  {
    table = driver.open(boost::filesystem::path(filename));
    BOOST_CHECK(table);
    BOOST_CHECK_EQUAL(table->nrCols(), size_t(3));

    BOOST_CHECK_EQUAL(table->typeId(0), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(1), TI_INT1);
    BOOST_CHECK_EQUAL(table->typeId(2), TI_REAL4);

    table->createCols();
    dynamic_cast<TableDriver&>(driver).read(*table, filename);
    BOOST_CHECK_EQUAL(table->nrRecs(), size_t(6));

    Array<UINT1>& col1 = table->col<UINT1>(0);
    Array<INT1>& col2 = table->col<INT1>(1);
    Array<REAL4>& col3 = table->col<REAL4>(2);

    BOOST_CHECK_EQUAL(col1[0],  1);
    BOOST_CHECK_EQUAL(col1[1],  4);
    BOOST_CHECK_EQUAL(col1[2],  7);
    BOOST_CHECK_EQUAL(col1[3], 10);
    BOOST_CHECK_EQUAL(col1[4], 13);
    BOOST_CHECK_EQUAL(col1[5], 16);

    BOOST_CHECK_EQUAL(col2[0],  2);
    BOOST_CHECK_EQUAL(col2[1], -5);
    BOOST_CHECK_EQUAL(col2[2],  8);
    BOOST_CHECK_EQUAL(col2[3], 11);
    BOOST_CHECK_EQUAL(col2[4], 14);
    BOOST_CHECK_EQUAL(col2[5], 17);

    BOOST_CHECK_EQUAL(col3[0],  3.0);
    BOOST_CHECK_EQUAL(col3[1],  6.0);
    BOOST_CHECK_EQUAL(col3[2],  9.0);
    BOOST_CHECK_EQUAL(col3[3], 12.0);
    BOOST_CHECK_EQUAL(col3[4], 15.5);
    BOOST_CHECK_EQUAL(col3[5], 18.0);

    delete table;
  }

  DataSpace dataSpace(dynamic_cast<Driver&>(driver).dataSpace(filename));
  BOOST_CHECK_EQUAL(dataSpace.rank(), size_t(1));
  BOOST_CHECK(dataSpace.hasTime());

  Dimension const& dimension(dataSpace.dimension(0));
  BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
  BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(16));
  BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(3));
}



void dal::TextTableDriverTest::testTable2()
{
/*
cola colb colc
 1  2  3
 4 -5  6
 7  8  9
10 11 12
13 14 15.5
16 17 18
*/
  std::string filename = "table2.col";
  TextTableDriver driver;
  Table* table = driver.open(boost::filesystem::path(filename));
  BOOST_REQUIRE(table);
  BOOST_REQUIRE_EQUAL(table->nrCols(), size_t(3));

  BOOST_REQUIRE_EQUAL(table->typeId(0), TI_UINT1);
  BOOST_REQUIRE_EQUAL(table->typeId(1), TI_INT1);
  BOOST_REQUIRE_EQUAL(table->typeId(2), TI_REAL4);

  table->createCols();
  dynamic_cast<TableDriver&>(driver).read(*table, filename);
  BOOST_REQUIRE_EQUAL(table->nrRecs(), size_t(6));

  Array<UINT1>& col1 = table->col<UINT1>(0);
  Array<INT1>&  col2 = table->col<INT1>(1);
  Array<REAL4>& col3 = table->col<REAL4>(2);

  BOOST_CHECK_EQUAL(table->title(0), "cola");
  BOOST_CHECK_EQUAL(col1[0],  1);
  BOOST_CHECK_EQUAL(col1[1],  4);
  BOOST_CHECK_EQUAL(col1[2],  7);
  BOOST_CHECK_EQUAL(col1[3], 10);
  BOOST_CHECK_EQUAL(col1[4], 13);
  BOOST_CHECK_EQUAL(col1[5], 16);

  BOOST_CHECK_EQUAL(table->title(1), "colb");
  BOOST_CHECK_EQUAL(col2[0],  2);
  BOOST_CHECK_EQUAL(col2[1], -5);
  BOOST_CHECK_EQUAL(col2[2],  8);
  BOOST_CHECK_EQUAL(col2[3], 11);
  BOOST_CHECK_EQUAL(col2[4], 14);
  BOOST_CHECK_EQUAL(col2[5], 17);

  BOOST_CHECK_EQUAL(table->title(2), "colc");
  BOOST_CHECK_EQUAL(col3[0],  3.0);
  BOOST_CHECK_EQUAL(col3[1],  6.0);
  BOOST_CHECK_EQUAL(col3[2],  9.0);
  BOOST_CHECK_EQUAL(col3[3], 12.0);
  BOOST_CHECK_EQUAL(col3[4], 15.5);
  BOOST_CHECK_EQUAL(col3[5], 18.0);

  delete table;

  DataSpace dataSpace(dynamic_cast<Driver&>(driver).dataSpace(filename));
  BOOST_CHECK_EQUAL(dataSpace.rank(), size_t(1));
  BOOST_CHECK(dataSpace.hasTime());

  Dimension const& dimension(dataSpace.dimension(0));
  BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
  BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(16));
  BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(3));
}



// 1 2 3 4 5
void dal::TextTableDriverTest::testTable5()
{
  std::string filename = "table5.col";
  Table* table;

  {
    TextTableDriver driver(AUTO_HEADER);
    table = driver.open(boost::filesystem::path(filename));

    BOOST_CHECK(table);
    BOOST_CHECK_EQUAL(table->nrCols(), size_t(5));
    BOOST_CHECK_EQUAL(table->typeId(0), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(1), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(2), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(3), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(4), TI_UINT1);

    table->createCols();
    dynamic_cast<TableDriver&>(driver).read(*table, filename);
    BOOST_CHECK_EQUAL(table->nrRecs(), size_t(1));

    Array<UINT1>& col1 = table->col<UINT1>(0);
    Array<UINT1>& col2 = table->col<UINT1>(1);
    Array<UINT1>& col3 = table->col<UINT1>(2);
    Array<UINT1>& col4 = table->col<UINT1>(3);
    Array<UINT1>& col5 = table->col<UINT1>(4);

    BOOST_CHECK_EQUAL(col1[0],  1);
    BOOST_CHECK_EQUAL(col2[0],  2);
    BOOST_CHECK_EQUAL(col3[0],  3);
    BOOST_CHECK_EQUAL(col4[0],  4);
    BOOST_CHECK_EQUAL(col5[0],  5);

    delete table;
  }

  {
    TextTableDriver driver(NO_HEADER);
    table = driver.open(boost::filesystem::path(filename));

    BOOST_CHECK(table);
    BOOST_CHECK_EQUAL(table->nrCols(), size_t(5));
    BOOST_CHECK_EQUAL(table->typeId(0), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(1), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(2), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(3), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(4), TI_UINT1);

    table->createCols();
    dynamic_cast<TableDriver&>(driver).read(*table, filename);
    BOOST_CHECK_EQUAL(table->nrRecs(), size_t(1));

    Array<UINT1>& col1 = table->col<UINT1>(0);
    Array<UINT1>& col2 = table->col<UINT1>(1);
    Array<UINT1>& col3 = table->col<UINT1>(2);
    Array<UINT1>& col4 = table->col<UINT1>(3);
    Array<UINT1>& col5 = table->col<UINT1>(4);

    BOOST_CHECK_EQUAL(col1[0],  1);
    BOOST_CHECK_EQUAL(col2[0],  2);
    BOOST_CHECK_EQUAL(col3[0],  3);
    BOOST_CHECK_EQUAL(col4[0],  4);
    BOOST_CHECK_EQUAL(col5[0],  5);

    delete table;
  }

  {
    TextTableDriver driver(HEADER);
    table = driver.open(boost::filesystem::path(filename));

    BOOST_CHECK(table);
    BOOST_CHECK_EQUAL(table->nrCols(), size_t(5));
    BOOST_CHECK_EQUAL(table->typeId(0), TI_NR_TYPES);
    BOOST_CHECK_EQUAL(table->typeId(1), TI_NR_TYPES);
    BOOST_CHECK_EQUAL(table->typeId(2), TI_NR_TYPES);
    BOOST_CHECK_EQUAL(table->typeId(3), TI_NR_TYPES);
    BOOST_CHECK_EQUAL(table->typeId(4), TI_NR_TYPES);

    BOOST_CHECK_EQUAL(table->nrRecs(), size_t(0));
    BOOST_CHECK_EQUAL(table->title(0), "1");
    BOOST_CHECK_EQUAL(table->title(1), "2");
    BOOST_CHECK_EQUAL(table->title(2), "3");
    BOOST_CHECK_EQUAL(table->title(3), "4");
    BOOST_CHECK_EQUAL(table->title(4), "5");

    // Fails because the type id's of the columns are invalid.
    // driver.read(filename, *table);

    delete table;
  }
}



void dal::TextTableDriverTest::testTable6()
{
  std::string filename;
  TextTableDriver driver;
  Table* table;

  {
    filename = "table6.col";
    table = driver.open(boost::filesystem::path(filename));
    BOOST_CHECK(table);
    BOOST_CHECK_EQUAL(table->nrCols(), size_t(3));

    BOOST_CHECK_EQUAL(table->typeId(0), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(1), TI_INT1);
    BOOST_CHECK_EQUAL(table->typeId(2), TI_REAL4);

    table->createCols();
    dynamic_cast<TableDriver&>(driver).read(*table, filename);
    BOOST_CHECK_EQUAL(table->nrRecs(), size_t(6));

    Array<UINT1>& col1 = table->col<UINT1>(0);
    Array<INT1>& col2 = table->col<INT1>(1);
    Array<REAL4>& col3 = table->col<REAL4>(2);

    BOOST_CHECK_EQUAL(col1[0],  1);
    BOOST_CHECK_EQUAL(col1[1],  4);
    BOOST_CHECK_EQUAL(col1[2],  7);
    BOOST_CHECK_EQUAL(col1[3], 10);
    BOOST_CHECK_EQUAL(col1[4], 13);
    BOOST_CHECK_EQUAL(col1[5], 16);

    BOOST_CHECK_EQUAL(col2[0],  2);
    BOOST_CHECK_EQUAL(col2[1], -5);
    BOOST_CHECK_EQUAL(col2[2],  8);
    BOOST_CHECK_EQUAL(col2[3], 11);
    BOOST_CHECK_EQUAL(col2[4], 14);
    BOOST_CHECK_EQUAL(col2[5], 17);

    BOOST_CHECK_EQUAL(col3[0],  3.0);
    BOOST_CHECK_EQUAL(col3[1],  6.0);
    BOOST_CHECK_EQUAL(col3[2],  9.0);
    BOOST_CHECK_EQUAL(col3[3], 12.0);
    BOOST_CHECK_EQUAL(col3[4], 15.5);
    BOOST_CHECK_EQUAL(col3[5], 18.0);

    delete table;
  }

  DataSpace dataSpace(dynamic_cast<Driver&>(driver).dataSpace(filename));
  BOOST_CHECK_EQUAL(dataSpace.rank(), size_t(1));
  BOOST_CHECK(dataSpace.hasTime());

  Dimension const& dimension(dataSpace.dimension(0));
  BOOST_CHECK_EQUAL(dimension.value<size_t>(0), size_t(1));
  BOOST_CHECK_EQUAL(dimension.value<size_t>(1), size_t(16));
  BOOST_CHECK_EQUAL(dimension.value<size_t>(2), size_t(3));
}



void dal::TextTableDriverTest::testTable2eas()
{
  std::string filename = "table2.eas";
  TextTableDriver driver;
  bool exceptionCaught;

  boost::shared_ptr<Table> table(driver.open(boost::filesystem::path(
    filename)));
  BOOST_CHECK(!table);

  try {
    exceptionCaught = false;
    table.reset(dynamic_cast<TableDriver&>(driver).read(filename));
  }
  catch(Exception& exception) {
    BOOST_CHECK_EQUAL(exception.message(),
       "Data source " + filename + "(table):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
}



void dal::TextTableDriverTest::testDOSFormattedTable()
{
  // GRID	PC4	X	Y	KANTOREN	TOTARB	JOB30CAR	POP30CAR	GEOEDU
  // 1	1458	116	506	1	21	620464	1443475	0
  // 2	1458	117	506	1	21	620464	1443475	0
  // 3	1458	117	505	1	21	620464	1443475	0
  // 4	1969	101	505	174	1494	544246	1144715	-.093
  // 5	1969	102	505	174	1494	544246	1144715	-.093
  // 6	1969	102	505	174	1494	544246	1144715	-.093
  // 7	1969	103	505	174	1494	544246	1144715	-.093
  // 8	1969	103	505	174	1494	544246	1144715	-.093
  // 9	1969	104	505	174	1494	544246	1144715	-.093
  // 10	1968	104	505	2	30	641728	1401630	-.093

  std::string filename = "dosformat.col";
  TextTableDriver driver;
  boost::shared_ptr<Table> table(driver.open(
    boost::filesystem::path(filename)));
  BOOST_REQUIRE(table);
  BOOST_CHECK_EQUAL(table->nrCols(), size_t(9));
  BOOST_CHECK_EQUAL(table->title(4), "KANTOREN");

  table->setTypeId(0, TI_INT4);
  table->setTypeId(1, TI_INT4);
  table->setTypeId(2, TI_INT4);
  table->setTypeId(3, TI_INT4);
  table->setTypeId(4, TI_INT4);
  table->setTypeId(5, TI_INT4);
  table->setTypeId(6, TI_INT4);
  table->setTypeId(7, TI_INT4);
  table->setTypeId(8, TI_REAL4);
  table->createCols();
  dynamic_cast<TableDriver&>(driver).read(*table, filename);
  BOOST_CHECK_EQUAL(table->nrRecs(), size_t(10));

  Array<INT4>& col1 = table->col<INT4>(0);
  BOOST_CHECK_EQUAL(col1[0], 1);
  BOOST_CHECK_EQUAL(col1[1], 2);
  BOOST_CHECK_EQUAL(col1[2], 3);
  BOOST_CHECK_EQUAL(col1[3], 4);
}

namespace dal {

void TextTableDriverTest::testColumnWithEmptyValues()
{
  // For example, columns separated by tabs, some values given, some absent.
  // Should be possible, by splitting the record on the seperator. Empty value
  // is a missing value.
  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}


void TextTableDriverTest::testColumnWithQuiteSomeZeros()
{
  std::string filename = "table7.col";
  TextTableDriver driver;
  boost::shared_ptr<Table> table(driver.open(
    boost::filesystem::path(filename)));
  BOOST_REQUIRE(table);
  BOOST_CHECK_EQUAL(table->nrCols(), size_t(2));

  BOOST_CHECK_EQUAL(table->typeId(0), TI_UINT1);
  BOOST_CHECK_EQUAL(table->typeId(1), TI_REAL4);
}

} // namespace dal
