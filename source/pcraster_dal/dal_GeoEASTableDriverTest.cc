#define BOOST_TEST_MODULE pcraster dal geo_eas_table_driver_test
#include <boost/test/unit_test.hpp>
#include "dal_Exception.h"
#include "dal_GeoEASTableDriver.h"
#include "dal_TableDriver.h"


BOOST_AUTO_TEST_CASE(description)
{
  using namespace dal;

  GeoEASTableDriver driver;
  BOOST_CHECK_EQUAL(driver.description(), "Geo-EAS table file format");
}


BOOST_AUTO_TEST_CASE(unexisting)
{
  using namespace dal;

  std::string filename = "unexisting";
  GeoEASTableDriver driver;
  bool exceptionCaught;

  Table* table = dynamic_cast<Table*>(
         dynamic_cast<Driver&>(driver).open(filename));
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


BOOST_AUTO_TEST_CASE(empty)
{
  using namespace dal;

  std::string filename = "emptyfile";
  GeoEASTableDriver driver;
  bool exceptionCaught;

  Table* table = dynamic_cast<Table*>(
         dynamic_cast<Driver&>(driver).open(filename));
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


BOOST_AUTO_TEST_CASE(invalid_grammer)
{
  using namespace dal;

  std::string filename = ":/:/:/:/:";
  GeoEASTableDriver driver;
  bool exceptionCaught;

  Table* table = dynamic_cast<Table*>(
         dynamic_cast<Driver&>(driver).open(filename));
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



/*
echo  Special table >  table1.eas
echo  3             >> table1.eas
echo  One           >> table1.eas
echo  Two           >> table1.eas
echo  Three         >> table1.eas
echo  1  2  3       >> table1.eas
echo  4 -5  6       >> table1.eas
echo  7  8  9       >> table1.eas
echo 10 11 12       >> table1.eas
echo 13 14 15.5     >> table1.eas
echo 16 17 18       >> table1.eas
*/
BOOST_AUTO_TEST_CASE(table1)
{
  using namespace dal;

  std::string filename;
  GeoEASTableDriver driver;
  Table* table;

  {
    filename = "table1.eas";
    table = dynamic_cast<Table*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(table);
    BOOST_CHECK_EQUAL(table->title(), "Special table");
    BOOST_CHECK_EQUAL(table->nrCols(), size_t(3));
    BOOST_CHECK_EQUAL(table->title(0), "One");
    BOOST_CHECK_EQUAL(table->title(1), "Two");
    BOOST_CHECK_EQUAL(table->title(2), "Three");
    BOOST_CHECK_EQUAL(table->typeId(0), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(1), TI_INT1);
    BOOST_CHECK_EQUAL(table->typeId(2), TI_REAL4);

    // Why do we need to cast?
    table->createCols();
    static_cast<TableDriver&>(driver).read(*table, filename);
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
    BOOST_CHECK(pcr::isMV(col3[5]));

    delete table;
  }
}


BOOST_AUTO_TEST_CASE(table2)
{
  using namespace dal;

  std::string filename;
  GeoEASTableDriver driver;
  Table* table;

  {
    filename = "table2.eas";
    table = dynamic_cast<Table*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(table);
    BOOST_CHECK_EQUAL(table->title()  , "Attributes of the zones.");
    BOOST_CHECK_EQUAL(table->nrCols() , size_t(12));
    BOOST_CHECK_EQUAL(table->title(0) , "Zone number");
    BOOST_CHECK_EQUAL(table->title(1) , "Number of dwellings");
    BOOST_CHECK_EQUAL(table->title(2) , "Urbanity");
    BOOST_CHECK_EQUAL(table->title(3) , "Less then 300 m3, 2 rooms");
    BOOST_CHECK_EQUAL(table->title(4) , "Less then 300 m3, 3 rooms");
    BOOST_CHECK_EQUAL(table->title(5) , "Less then 300 m3, 4 rooms");
    BOOST_CHECK_EQUAL(table->title(6) , "Between 300 and 600 m3, 3 rooms");
    BOOST_CHECK_EQUAL(table->title(7) , "Between 300 and 600 m3, 4 rooms");
    BOOST_CHECK_EQUAL(table->title(8) , "Between 300 and 600 m3, 5 rooms");
    BOOST_CHECK_EQUAL(table->title(9) , "Greater than 600 m3, 4 rooms");
    BOOST_CHECK_EQUAL(table->title(10), "Greater than 600 m3, 5 rooms");
    BOOST_CHECK_EQUAL(table->title(11), "Greater than 600 m3, 6 rooms");

    BOOST_CHECK_EQUAL(table->typeId(0) , TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(1) , TI_UINT2);
    BOOST_CHECK_EQUAL(table->typeId(2) , TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(3) , TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(4) , TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(5) , TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(6) , TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(7) , TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(8) , TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(9) , TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(10), TI_UINT1);
    BOOST_CHECK_EQUAL(table->typeId(11), TI_UINT1);

    table->createCols();
    static_cast<TableDriver&>(driver).read(*table, filename);
    BOOST_CHECK_EQUAL(table->nrRecs(), size_t(8));

    Array<UINT1>& col1  = table->col<UINT1>(0);
    Array<UINT2>& col2  = table->col<UINT2>(1);
    Array<UINT1>& col3  = table->col<UINT1>(2);
    Array<UINT1>& col9  = table->col<UINT1>(8);
    Array<UINT1>& col12 = table->col<UINT1>(11);

    BOOST_CHECK_EQUAL(col1[0], 1);
    BOOST_CHECK_EQUAL(col1[1], 2);
    BOOST_CHECK_EQUAL(col1[2], 3);
    BOOST_CHECK_EQUAL(col1[3], 4);
    BOOST_CHECK_EQUAL(col1[4], 5);
    BOOST_CHECK_EQUAL(col1[5], 6);
    BOOST_CHECK_EQUAL(col1[6], 7);
    BOOST_CHECK_EQUAL(col1[7], 8);

    BOOST_CHECK_EQUAL(col2[0], 266);
    BOOST_CHECK_EQUAL(col2[1], 386);
    BOOST_CHECK_EQUAL(col2[2], 372);
    BOOST_CHECK_EQUAL(col2[3], 304);
    BOOST_CHECK_EQUAL(col2[4], 651);
    BOOST_CHECK_EQUAL(col2[5], 340);
    BOOST_CHECK_EQUAL(col2[6], 136);
    BOOST_CHECK_EQUAL(col2[7],  45);

    BOOST_CHECK_EQUAL(col3[0], 2);
    BOOST_CHECK_EQUAL(col3[1], 2);
    BOOST_CHECK_EQUAL(col3[2], 2);
    BOOST_CHECK_EQUAL(col3[3], 2);
    BOOST_CHECK_EQUAL(col3[4], 5);
    BOOST_CHECK_EQUAL(col3[5], 5);
    BOOST_CHECK_EQUAL(col3[6], 8);
    BOOST_CHECK_EQUAL(col3[7], 8);

    BOOST_CHECK_EQUAL(col9[0], 10);
    BOOST_CHECK_EQUAL(col9[1], 12);
    BOOST_CHECK_EQUAL(col9[2],  8);
    BOOST_CHECK_EQUAL(col9[3], 16);
    BOOST_CHECK_EQUAL(col9[4],  9);
    BOOST_CHECK_EQUAL(col9[5], 12);
    BOOST_CHECK_EQUAL(col9[6],  6);
    BOOST_CHECK_EQUAL(col9[7],  8);

    BOOST_CHECK_EQUAL(col12[0], 20);
    BOOST_CHECK_EQUAL(col12[1],  8);
    BOOST_CHECK_EQUAL(col12[2],  4);
    BOOST_CHECK_EQUAL(col12[3], 16);
    BOOST_CHECK_EQUAL(col12[4],  6);
    BOOST_CHECK_EQUAL(col12[5], 12);
    BOOST_CHECK_EQUAL(col12[6],  4);
    BOOST_CHECK_EQUAL(col12[7], 8);

    delete table;
  }
}
