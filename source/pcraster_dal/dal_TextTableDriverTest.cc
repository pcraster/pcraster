#define BOOST_TEST_MODULE pcraster dal text_table_driver
#include <boost/test/unit_test.hpp>
#include "dal_Exception.h"
#include "dal_TextTableDriver.h"


BOOST_AUTO_TEST_CASE(description)
{
  using namespace dal;

  TextTableDriver const driver;
  BOOST_TEST(driver.description() == "Text table file format");
}


BOOST_AUTO_TEST_CASE(unexisting)
{
  using namespace dal;

  std::string const filename = "unexisting";
  TextTableDriver const driver;
  bool exceptionCaught = false;

  Table* table = driver.open(std::filesystem::path(filename));
  BOOST_TEST(!table);

  try {
    exceptionCaught = false;
    table = dynamic_cast<TableDriver const&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_TEST(exception.message() ==
       "Data source " + filename + "(table):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_TEST(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(empty)
{
  using namespace dal;

  std::string const filename = "emptyfile";
  TextTableDriver driver;
  bool exceptionCaught = false;

  Table* table = driver.open(std::filesystem::path(filename));
  BOOST_TEST(!table);

  try {
    exceptionCaught = false;
    table = dynamic_cast<TableDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_TEST(exception.message() ==
       "Data source " + filename + "(table):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_TEST(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(invalid_grammar)
{
  using namespace dal;

  std::string const filename = ":/:/:/:/:";
  TextTableDriver driver;
  bool exceptionCaught=false;

  Table* table=nullptr;

  try {
    exceptionCaught = false;
    table = driver.open(std::filesystem::path(filename));
#ifndef WIN32
    BOOST_TEST(!table);
#endif
    table = dynamic_cast<TableDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
// #ifdef WIN32
//     BOOST_TEST(exception.message() ==
//        "Pathname '" + filename + "': Not valid on the current platform");
// #else
    BOOST_TEST(exception.message() ==
       "Data source " + filename + "(table):\ncannot be opened");
// #endif
    exceptionCaught = true;
  }
  BOOST_TEST(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(table1)
{
  using namespace dal;

  std::string const filename = "table1.col";
  TextTableDriver driver;
  Table* table = nullptr;

  {
    table = driver.open(std::filesystem::path(filename));
    BOOST_TEST(table);
    BOOST_TEST(table->nrCols() == size_t(3));

    BOOST_TEST(table->typeId(0) == TI_UINT1);
    BOOST_TEST(table->typeId(1) == TI_INT1);
    BOOST_TEST(table->typeId(2) == TI_REAL4);

    table->createCols();
    dynamic_cast<TableDriver&>(driver).read(*table, filename);
    BOOST_TEST(table->nrRecs() == size_t(6));

    Array<UINT1>& col1 = table->col<UINT1>(0);
    Array<INT1>& col2 = table->col<INT1>(1);
    Array<REAL4>& col3 = table->col<REAL4>(2);

    BOOST_TEST(col1[0] ==  1);
    BOOST_TEST(col1[1] ==  4);
    BOOST_TEST(col1[2] ==  7);
    BOOST_TEST(col1[3] == 10);
    BOOST_TEST(col1[4] == 13);
    BOOST_TEST(col1[5] == 16);

    BOOST_TEST(col2[0] ==  2);
    BOOST_TEST(col2[1] == -5);
    BOOST_TEST(col2[2] ==  8);
    BOOST_TEST(col2[3] == 11);
    BOOST_TEST(col2[4] == 14);
    BOOST_TEST(col2[5] == 17);

    BOOST_TEST(col3[0] ==  3.0);
    BOOST_TEST(col3[1] ==  6.0);
    BOOST_TEST(col3[2] ==  9.0);
    BOOST_TEST(col3[3] == 12.0);
    BOOST_TEST(col3[4] == 15.5);
    BOOST_TEST(col3[5] == 18.0);

    delete table;
  }

  DataSpace dataSpace(dynamic_cast<Driver&>(driver).dataSpace(filename));
  BOOST_TEST(dataSpace.rank() == size_t(1));
  BOOST_TEST(dataSpace.hasTime());

  Dimension const& dimension(dataSpace.dimension(0));
  BOOST_TEST(dimension.value<size_t>(0) == size_t(1));
  BOOST_TEST(dimension.value<size_t>(1) == size_t(16));
  BOOST_TEST(dimension.value<size_t>(2) == size_t(3));
}


/*
cola colb colc
 1  2  3
 4 -5  6
 7  8  9
10 11 12
13 14 15.5
16 17 18
*/
BOOST_AUTO_TEST_CASE(table2)
{
  using namespace dal;

  std::string const filename = "table2.col";
  TextTableDriver driver;
  Table* table = driver.open(std::filesystem::path(filename));
  BOOST_TEST_REQUIRE(table);
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

  BOOST_TEST(table->title(0) == "cola");
  BOOST_TEST(col1[0] ==  1);
  BOOST_TEST(col1[1] ==  4);
  BOOST_TEST(col1[2] ==  7);
  BOOST_TEST(col1[3] == 10);
  BOOST_TEST(col1[4] == 13);
  BOOST_TEST(col1[5] == 16);

  BOOST_TEST(table->title(1) == "colb");
  BOOST_TEST(col2[0] ==  2);
  BOOST_TEST(col2[1] == -5);
  BOOST_TEST(col2[2] ==  8);
  BOOST_TEST(col2[3] == 11);
  BOOST_TEST(col2[4] == 14);
  BOOST_TEST(col2[5] == 17);

  BOOST_TEST(table->title(2) == "colc");
  BOOST_TEST(col3[0] ==  3.0);
  BOOST_TEST(col3[1] ==  6.0);
  BOOST_TEST(col3[2] ==  9.0);
  BOOST_TEST(col3[3] == 12.0);
  BOOST_TEST(col3[4] == 15.5);
  BOOST_TEST(col3[5] == 18.0);

  delete table;

  DataSpace dataSpace(dynamic_cast<Driver&>(driver).dataSpace(filename));
  BOOST_TEST(dataSpace.rank() == size_t(1));
  BOOST_TEST(dataSpace.hasTime());

  Dimension const& dimension(dataSpace.dimension(0));
  BOOST_TEST(dimension.value<size_t>(0) == size_t(1));
  BOOST_TEST(dimension.value<size_t>(1) == size_t(16));
  BOOST_TEST(dimension.value<size_t>(2) == size_t(3));
}


// 1 2 3 4 5
BOOST_AUTO_TEST_CASE(table5)
{
  using namespace dal;

  std::string const filename = "table5.col";
  Table* table = nullptr;

  {
    TextTableDriver driver(AUTO_HEADER);
    table = driver.open(std::filesystem::path(filename));

    BOOST_TEST(table);
    BOOST_TEST(table->nrCols() == size_t(5));
    BOOST_TEST(table->typeId(0) == TI_UINT1);
    BOOST_TEST(table->typeId(1) == TI_UINT1);
    BOOST_TEST(table->typeId(2) == TI_UINT1);
    BOOST_TEST(table->typeId(3) == TI_UINT1);
    BOOST_TEST(table->typeId(4) == TI_UINT1);

    table->createCols();
    dynamic_cast<TableDriver&>(driver).read(*table, filename);
    BOOST_TEST(table->nrRecs() == size_t(1));

    Array<UINT1>& col1 = table->col<UINT1>(0);
    Array<UINT1>& col2 = table->col<UINT1>(1);
    Array<UINT1>& col3 = table->col<UINT1>(2);
    Array<UINT1>& col4 = table->col<UINT1>(3);
    Array<UINT1>& col5 = table->col<UINT1>(4);

    BOOST_TEST(col1[0] ==  1);
    BOOST_TEST(col2[0] ==  2);
    BOOST_TEST(col3[0] ==  3);
    BOOST_TEST(col4[0] ==  4);
    BOOST_TEST(col5[0] ==  5);

    delete table;
  }

  {
    TextTableDriver driver(NO_HEADER);
    table = driver.open(std::filesystem::path(filename));

    BOOST_TEST(table);
    BOOST_TEST(table->nrCols() == size_t(5));
    BOOST_TEST(table->typeId(0) == TI_UINT1);
    BOOST_TEST(table->typeId(1) == TI_UINT1);
    BOOST_TEST(table->typeId(2) == TI_UINT1);
    BOOST_TEST(table->typeId(3) == TI_UINT1);
    BOOST_TEST(table->typeId(4) == TI_UINT1);

    table->createCols();
    dynamic_cast<TableDriver&>(driver).read(*table, filename);
    BOOST_TEST(table->nrRecs() == size_t(1));

    Array<UINT1>& col1 = table->col<UINT1>(0);
    Array<UINT1>& col2 = table->col<UINT1>(1);
    Array<UINT1>& col3 = table->col<UINT1>(2);
    Array<UINT1>& col4 = table->col<UINT1>(3);
    Array<UINT1>& col5 = table->col<UINT1>(4);

    BOOST_TEST(col1[0] ==  1);
    BOOST_TEST(col2[0] ==  2);
    BOOST_TEST(col3[0] ==  3);
    BOOST_TEST(col4[0] ==  4);
    BOOST_TEST(col5[0] ==  5);

    delete table;
  }

  {
    TextTableDriver const driver(HEADER);
    table = driver.open(std::filesystem::path(filename));

    BOOST_TEST(table);
    BOOST_TEST(table->nrCols() == size_t(5));
    BOOST_TEST(table->typeId(0) == TI_NR_TYPES);
    BOOST_TEST(table->typeId(1) == TI_NR_TYPES);
    BOOST_TEST(table->typeId(2) == TI_NR_TYPES);
    BOOST_TEST(table->typeId(3) == TI_NR_TYPES);
    BOOST_TEST(table->typeId(4) == TI_NR_TYPES);

    BOOST_TEST(table->nrRecs() == size_t(0));
    BOOST_TEST(table->title(0) == "1");
    BOOST_TEST(table->title(1) == "2");
    BOOST_TEST(table->title(2) == "3");
    BOOST_TEST(table->title(3) == "4");
    BOOST_TEST(table->title(4) == "5");

    // Fails because the type id's of the columns are invalid.
    // driver.read(filename, *table);

    delete table;
  }
}


BOOST_AUTO_TEST_CASE(table6)
{
  using namespace dal;

  std::string filename;
  TextTableDriver driver;
  Table* table = nullptr;

  {
    filename = "table6.col";
    table = driver.open(std::filesystem::path(filename));
    BOOST_TEST(table);
    BOOST_TEST(table->nrCols() == size_t(3));

    BOOST_TEST(table->typeId(0) == TI_UINT1);
    BOOST_TEST(table->typeId(1) == TI_INT1);
    BOOST_TEST(table->typeId(2) == TI_REAL4);

    table->createCols();
    dynamic_cast<TableDriver&>(driver).read(*table, filename);
    BOOST_TEST(table->nrRecs() == size_t(6));

    Array<UINT1>& col1 = table->col<UINT1>(0);
    Array<INT1>& col2 = table->col<INT1>(1);
    Array<REAL4>& col3 = table->col<REAL4>(2);

    BOOST_TEST(col1[0] ==  1);
    BOOST_TEST(col1[1] ==  4);
    BOOST_TEST(col1[2] ==  7);
    BOOST_TEST(col1[3] == 10);
    BOOST_TEST(col1[4] == 13);
    BOOST_TEST(col1[5] == 16);

    BOOST_TEST(col2[0] ==  2);
    BOOST_TEST(col2[1] == -5);
    BOOST_TEST(col2[2] ==  8);
    BOOST_TEST(col2[3] == 11);
    BOOST_TEST(col2[4] == 14);
    BOOST_TEST(col2[5] == 17);

    BOOST_TEST(col3[0] ==  3.0);
    BOOST_TEST(col3[1] ==  6.0);
    BOOST_TEST(col3[2] ==  9.0);
    BOOST_TEST(col3[3] == 12.0);
    BOOST_TEST(col3[4] == 15.5);
    BOOST_TEST(col3[5] == 18.0);

    delete table;
  }

  DataSpace dataSpace(dynamic_cast<Driver&>(driver).dataSpace(filename));
  BOOST_TEST(dataSpace.rank() == size_t(1));
  BOOST_TEST(dataSpace.hasTime());

  Dimension const& dimension(dataSpace.dimension(0));
  BOOST_TEST(dimension.value<size_t>(0) == size_t(1));
  BOOST_TEST(dimension.value<size_t>(1) == size_t(16));
  BOOST_TEST(dimension.value<size_t>(2) == size_t(3));
}


BOOST_AUTO_TEST_CASE(table2_eas)
{
  using namespace dal;

  std::string const filename = "table2.eas";
  TextTableDriver driver;
  bool exceptionCaught = false;

  std::shared_ptr<Table> table(driver.open(std::filesystem::path(
    filename)));
  BOOST_TEST(!table);

  try {
    exceptionCaught = false;
    table.reset(dynamic_cast<TableDriver&>(driver).read(filename));
  }
  catch(Exception& exception) {
    BOOST_TEST(exception.message() ==
       "Data source " + filename + "(table):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_TEST(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(dos_formatted)
{
  using namespace dal;

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

  std::string const filename = "dosformat.col";
  TextTableDriver driver;
  std::shared_ptr<Table> const table(driver.open(
    std::filesystem::path(filename)));
  BOOST_TEST_REQUIRE(table);
  BOOST_TEST(table->nrCols() == size_t(9));
  BOOST_TEST(table->title(4) == "KANTOREN");

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
  BOOST_TEST(table->nrRecs() == size_t(10));

  Array<INT4>& col1 = table->col<INT4>(0);
  BOOST_TEST(col1[0] == 1);
  BOOST_TEST(col1[1] == 2);
  BOOST_TEST(col1[2] == 3);
  BOOST_TEST(col1[3] == 4);
}


BOOST_AUTO_TEST_CASE(column_with_empty_values)
{
  using namespace dal;

  // For example, columns separated by tabs, some values given, some absent.
  // Should be possible, by splitting the record on the seperator. Empty value
  // is a missing value.
  bool const testImplemented = false;
  BOOST_TEST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(column_with_quite_some_zeros)
{
  using namespace dal;

  std::string const filename = "table7.col";
  TextTableDriver const driver;
  std::shared_ptr<Table> const table(driver.open(
    std::filesystem::path(filename)));
  BOOST_TEST_REQUIRE(table);
  BOOST_TEST(table->nrCols() == size_t(2));

  BOOST_TEST(table->typeId(0) == TI_UINT1);
  BOOST_TEST(table->typeId(1) == TI_REAL4);
}
