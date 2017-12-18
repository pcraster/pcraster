#define BOOST_TEST_MODULE pcraster dal sql_table_driver
#include <boost/test/unit_test.hpp>
#include <boost/filesystem.hpp>
#include <QCoreApplication>
#include "dev_Utils.h"
#include "dal_Def.h"
#include "dal_Exception.h"
#include "dal_SQLTableDriver.h"
#define protected public
#include "dev_QtClient.h"
#include "dal_Client.h"


static int argc = 1;
static char const* argv[1] = {"/my/path/sql_table_driver_test"};
static dev::QtClient<QCoreApplication> qt_client(argc,
    const_cast<char**>(argv));
static dal::Client client("/my/path/sql_table_driver_test", true);


#ifndef QT_NO_SQL

struct Fixture
{

    Fixture()
#if defined(WIN32)
        : d_user(dev::environmentVariable("USERNAME"))
#else
        : d_user(dev::environmentVariable("LOGNAME"))
#endif

    {
        BOOST_REQUIRE(!d_user.empty());
    }

    ~Fixture()
    {
    }


    void test(
        std::string const& driverName)
    {
      // createDatabase(driverName, "MyTable");
      testUnexisting(driverName);
      testWrite(driverName);

      if(driverName == "QSQLITE") {
        testReadTemporal(driverName);
        testReadScenario(driverName);
        testReadScenarioTemporal(driverName);
        testReadQuantile(driverName);
      }

      // removeDatabase(driverName, "MyTable");
    }


    // void createDatabase(
    //          std::string const& driverName,
    //          std::string const& databaseName)
    // {
    //   SQLTableDriver driver(driverName);
    //
    //   BOOST_CHECK(!driver.databaseExists(databaseName));
    //   driver.addDatabase(databaseName);
    //   BOOST_CHECK(driver.databaseExists(databaseName));
    // }
    //
    //
    //
    // void removeDatabase(
    //          std::string const& driverName,
    //          std::string const& databaseName)
    // {
    //   SQLTableDriver driver(driverName);
    //
    //   BOOST_CHECK(driver.databaseExists(databaseName));
    //   driver.removeDatabase(databaseName);
    //   BOOST_CHECK(!driver.databaseExists(databaseName));
    // }


    void testUnexisting(
        std::string const& driverName)
    {
      using namespace dal;

      std::string name;
      SQLTableDriver driver(driverName);
      Table* table;

      {
        name = "";
        table = dynamic_cast<Table*>(dynamic_cast<Driver&>(driver).open(name));
        BOOST_CHECK(!table);
      }

      {
        name = "unexisting";
        table = dynamic_cast<Table*>(dynamic_cast<Driver&>(driver).open(name));
        BOOST_CHECK(!table);
      }

      {
        name = "DSN=unexisting;UID=" + d_user;
        table = dynamic_cast<Table*>(dynamic_cast<Driver&>(driver).open(name));
        BOOST_CHECK(!table);
      }
    }


    void testWrite(
        std::string const& driverName)
    {
      using namespace dal;

      // Creates MyTable/Example
      std::string name(d_user + ":MyTable/Example");
      SQLTableDriver driver(driverName);
      boost::shared_ptr<Table> outputTable;
      boost::shared_ptr<Table> inputTable;

      {
        std::vector<std::string> titles;
        std::vector<TypeId> typeIds;

        titles.push_back("name");
        typeIds.push_back(TI_STRING);

        titles.push_back("age");
        typeIds.push_back(TI_INT4);

        titles.push_back("room");
        typeIds.push_back(TI_INT4);

        titles.push_back("height");
        typeIds.push_back(TI_REAL8);

        outputTable.reset(new Table(titles, typeIds));
        outputTable->createCols();
        outputTable->col<std::string>(0).push_back("Piet-Jaapje");
        outputTable->col<std::string>(0).push_back("Fritz");
        outputTable->col<INT4>(1).push_back(0);
        outputTable->col<INT4>(1).push_back(50);
        outputTable->col<INT4>(2).push_back(1);
        outputTable->col<INT4>(2).push_back(2);
        outputTable->col<REAL8>(3).push_back(1.92);
        outputTable->col<REAL8>(3).push_back(1.85);

        BOOST_REQUIRE_NO_THROW(
          dynamic_cast<TableDriver&>(driver).write(*outputTable, name);
        )

        BOOST_WARN_MESSAGE(false, "Busy with SQLTableDriver...");
        /// BOOST_REQUIRE_NO_THROW(
        ///   inputTable.reset(dynamic_cast<TableDriver&>(driver).read(name));
        /// )
        /// BOOST_REQUIRE(inputTable);
        /// BOOST_CHECK(*inputTable == *outputTable);

        /// inputTable.reset(new Table(titles, typeIds));
        /// inputTable->createCols();
        /// dynamic_cast<TableDriver&>(driver).read(*inputTable, name);
        /// BOOST_CHECK(*inputTable == *outputTable);
        /// inputTable.reset();

        /// outputTable->clear();
        /// outputTable->col<std::string>(0).push_back("Reus");
        /// outputTable->col<INT4>(1).push_back(2);
        /// outputTable->col<INT4>(2).push_back(5);
        /// outputTable->col<REAL8>(3).push_back(2.11);

        /// BOOST_REQUIRE_NO_THROW(
        ///   dynamic_cast<TableDriver&>(driver).append(name, *outputTable);
        /// )

        /// BOOST_REQUIRE_NO_THROW(
        ///   inputTable.reset(dynamic_cast<TableDriver&>(driver).read(name));
        /// )
        /// BOOST_REQUIRE(inputTable);
        /// BOOST_CHECK_EQUAL(inputTable->nrCols(), size_t(4));
        /// BOOST_CHECK_EQUAL(inputTable->nrRecs(), size_t(3));

        /// Array<std::string> const* col1 = 0;
        /// BOOST_REQUIRE_NO_THROW(col1 = &inputTable->col<std::string>(0));
        /// BOOST_CHECK_EQUAL((*col1)[0], "Piet-Jaapje");
        /// BOOST_CHECK_EQUAL((*col1)[1], "Fritz");
        /// BOOST_CHECK_EQUAL((*col1)[2], "Reus");

        /// Array<INT4> const* col2 = 0;
        /// BOOST_REQUIRE_NO_THROW(col2 = &inputTable->col<INT4>(1));
        /// BOOST_CHECK_EQUAL((*col2)[0], 0);
        /// BOOST_CHECK_EQUAL((*col2)[1], 50);
        /// BOOST_CHECK_EQUAL((*col2)[2], 2);

        /// Array<INT4> const* col3 = 0;
        /// BOOST_REQUIRE_NO_THROW(col3 = &inputTable->col<INT4>(2));
        /// BOOST_CHECK_EQUAL((*col3)[0], 1);
        /// BOOST_CHECK_EQUAL((*col3)[1], 2);
        /// BOOST_CHECK_EQUAL((*col3)[2], 5);

        /// Array<REAL8> const* col4 = 0;
        /// BOOST_REQUIRE_NO_THROW(col4 = &inputTable->col<REAL8>(3));
        /// BOOST_CHECK(comparable((*col4)[0], 1.92));
        /// BOOST_CHECK(comparable((*col4)[1], 1.85));
        /// BOOST_CHECK(comparable((*col4)[2], 2.11));
      }

      // Selective read.
      {
        BOOST_WARN_MESSAGE(false, "Busy with SQLTableDriver...");
        // boost::shared_ptr<Table> table(dynamic_cast<Table*>(
        //      dynamic_cast<Driver&>(driver).open(name)));
        // BOOST_REQUIRE(table);

        // // We are not interested in the second column.
        // table->setTypeId(1, TI_NR_TYPES);
        // table->createCols();
        // dynamic_cast<TableDriver&>(driver).read(*table, name);
        // BOOST_CHECK_EQUAL(table->nrRecs(), size_t(3));

        // // Second column is not available.
        // BOOST_CHECK_THROW(table->col<INT4>(1), boost::bad_any_cast);

        // // Other columns are.
        // Array<std::string> const* col1 = 0;
        // BOOST_REQUIRE_NO_THROW(col1 = &table->col<std::string>(0));
        // BOOST_CHECK_EQUAL((*col1)[0], "Piet-Jaapje");
        // BOOST_CHECK_EQUAL((*col1)[1], "Fritz");
        // BOOST_CHECK_EQUAL((*col1)[2], "Reus");

        // Array<INT4> const* col3 = 0;
        // BOOST_REQUIRE_NO_THROW(col3 = &table->col<INT4>(2));
        // BOOST_CHECK_EQUAL((*col3)[0], 1);
        // BOOST_CHECK_EQUAL((*col3)[1], 2);
        // BOOST_CHECK_EQUAL((*col3)[2], 5);

        // Array<REAL8> const* col4 = 0;
        // BOOST_REQUIRE_NO_THROW(col4 = &table->col<REAL8>(3));
        // BOOST_CHECK(comparable((*col4)[0], 1.92));
        // BOOST_CHECK(comparable((*col4)[1], 1.85));
        // BOOST_CHECK(comparable((*col4)[2], 2.11));
      }
    }


    void testReadTemporal(
        std::string const& driverName)
    {
      using namespace dal;

      SQLTableDriver driver(driverName);

      std::string name = "dimensions/date/co2";
      DataSpace space;
      DataSpaceAddress address;
      boost::shared_ptr<Table> table;
      size_t dateId, attrId;

      {
        space.clear();
        address = space.address();
        BOOST_CHECK(driver.exists(name, space, address));

        // Read all.
        table.reset(driver.open(name, space, address));
        BOOST_REQUIRE(table);
        BOOST_CHECK_EQUAL(table->nrCols(), size_t(2));
        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(0));

        dateId = table->indexOf("date");
        BOOST_REQUIRE(dateId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(dateId), TI_INT4);
        BOOST_CHECK_EQUAL(table->title(dateId), "date");

        attrId = table->indexOf("co2");
        BOOST_REQUIRE(attrId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(attrId), TI_REAL4);
        BOOST_CHECK_EQUAL(table->title(attrId), "co2");

        BOOST_REQUIRE_NO_THROW(
          driver.read(*table, name, space, address);
        )

        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(3));

        BOOST_CHECK_EQUAL(table->col<INT4>(dateId)[0], INT4(1));
        BOOST_CHECK_EQUAL(table->col<INT4>(dateId)[1], INT4(2));
        BOOST_CHECK_EQUAL(table->col<INT4>(dateId)[2], INT4(3));

        BOOST_CHECK_CLOSE(table->col<REAL4>(attrId)[0], REAL4(1.11), REAL4(0.001));
        BOOST_CHECK_CLOSE(table->col<REAL4>(attrId)[1], REAL4(2.22), REAL4(0.001));
        BOOST_CHECK_CLOSE(table->col<REAL4>(attrId)[2], REAL4(3.33), REAL4(0.001));

        // Read selection.
        space.addDimension(Dimension(Time, size_t(1), size_t(3), size_t(1)));
        address = space.address();
        address.setCoordinate<size_t>(0, 2);

        BOOST_REQUIRE_NO_THROW(
          table.reset(driver.read(name, space, address));
        )

        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(1));
        BOOST_CHECK_CLOSE(table->col<REAL4>(1)[0], REAL4(2.22), REAL4(0.001));
      }
    }


    void testReadQuantile(
        std::string const& driverName)
    {
      using namespace dal;

      SQLTableDriver driver(driverName);

      std::string name = "dimensions/quantile/co2";
      DataSpace space;
      DataSpaceAddress address;
      boost::shared_ptr<Table> table;
      size_t quantileId, attrId;

      {
        space.clear();
        address = space.address();
        BOOST_CHECK(driver.exists(name, space, address));
      }

      {
        BOOST_REQUIRE(driver.exists(name, space, address));

        // Read all.
        space.clear();
        address = space.address();

        table.reset(driver.open(name, space, address));
        BOOST_REQUIRE(table);
        BOOST_CHECK_EQUAL(table->nrCols(), size_t(2));
        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(0));

        quantileId = table->indexOf("quantile");
        BOOST_REQUIRE(quantileId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(quantileId), TI_REAL4);
        BOOST_CHECK_EQUAL(table->title(quantileId), "quantile");

        attrId = table->indexOf("co2");
        BOOST_REQUIRE(attrId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(attrId), TI_REAL4);
        BOOST_CHECK_EQUAL(table->title(attrId), "co2");

        BOOST_REQUIRE_NO_THROW(
          driver.read(*table, name, space, address);
        )

        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(5));

        BOOST_CHECK_EQUAL(table->col<REAL4>(quantileId)[0], REAL4(0.01));
        BOOST_CHECK_EQUAL(table->col<REAL4>(quantileId)[1], REAL4(0.05));
        BOOST_CHECK_EQUAL(table->col<REAL4>(quantileId)[2], REAL4(0.50));
        BOOST_CHECK_EQUAL(table->col<REAL4>(quantileId)[3], REAL4(0.95));
        BOOST_CHECK_EQUAL(table->col<REAL4>(quantileId)[4], REAL4(0.99));

        BOOST_CHECK_EQUAL(table->col<REAL4>(attrId)[0], REAL4(1.01));
        BOOST_CHECK_EQUAL(table->col<REAL4>(attrId)[1], REAL4(1.05));
        BOOST_CHECK_EQUAL(table->col<REAL4>(attrId)[2], REAL4(1.50));
        BOOST_CHECK_EQUAL(table->col<REAL4>(attrId)[3], REAL4(1.95));
        BOOST_CHECK_EQUAL(table->col<REAL4>(attrId)[4], REAL4(1.99));

        // Read selection.
        space.addDimension(Dimension(CumulativeProbabilities, float(0.01),
             float(0.99), float(0.01)));
        address = space.address();
        address.setCoordinate<float>(0, 0.95f);

        table.reset(driver.open(name, space, address));
        BOOST_REQUIRE(table);
        BOOST_CHECK_EQUAL(table->nrCols(), size_t(2));
        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(0));

        quantileId = table->indexOf("quantile");
        BOOST_REQUIRE(quantileId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(quantileId), TI_REAL4);
        BOOST_CHECK_EQUAL(table->title(quantileId), "quantile");

        attrId = table->indexOf("co2");
        BOOST_REQUIRE(attrId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(attrId), TI_REAL4);
        BOOST_CHECK_EQUAL(table->title(attrId), "co2");

        BOOST_REQUIRE_NO_THROW(
          driver.read(*table, name, space, address);
        )

        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(1));
        BOOST_CHECK_CLOSE(table->col<REAL4>(attrId)[0], REAL4(1.95), REAL4(0.001));
      }
    }


    void testReadScenario(
        std::string const& driverName)
    {
      using namespace dal;

      SQLTableDriver driver(driverName);

      std::string name = "dimensions/scenario/co2";
      DataSpace space;
      DataSpaceAddress address;
      boost::shared_ptr<Table> table;
      size_t scenarioId, attrId;

      {
        space.clear();
        address = space.address();

        BOOST_CHECK(driver.exists(name, space, address));

        // Read all.
        table.reset(driver.open(name, space, address));
        BOOST_REQUIRE(table);
        BOOST_CHECK_EQUAL(table->nrCols(), size_t(2));
        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(0));

        scenarioId = table->indexOf("scenario");
        BOOST_REQUIRE(scenarioId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(scenarioId), TI_STRING);
        BOOST_CHECK_EQUAL(table->title(scenarioId), "scenario");

        attrId = table->indexOf("co2");
        BOOST_REQUIRE(attrId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(attrId), TI_REAL4);
        BOOST_CHECK_EQUAL(table->title(attrId), "co2");

        BOOST_REQUIRE_NO_THROW(
          table.reset(driver.read(name, space, address));
        )

        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(2));
        BOOST_CHECK_EQUAL(table->col<std::string>(scenarioId)[0],
             std::string("aap"));
        BOOST_CHECK_EQUAL(table->col<std::string>(scenarioId)[1],
             std::string("noot"));

        BOOST_CHECK_EQUAL(table->col<REAL4>(attrId)[0], REAL4(-1.11));
        BOOST_CHECK_EQUAL(table->col<REAL4>(attrId)[1], REAL4(-2.22));

        // Read selection.
        std::set<std::string> scenarios;
        scenarios.insert("aap");
        scenarios.insert("noot");

        space.addDimension(Dimension(Scenarios, scenarios));
        address = space.address();
        address.setCoordinate<std::string>(0, "noot");

        BOOST_REQUIRE_NO_THROW(
          table.reset(driver.read(name, space, address));
        )

        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(1));
        BOOST_CHECK_CLOSE(table->col<REAL4>(1)[0], REAL4(-2.22), REAL4(0.001));
      }
    }


    void testReadScenarioTemporal(
             std::string const& driverName)
    {
      using namespace dal;

      SQLTableDriver driver(driverName);

      std::string name = "dimensions/scenario_date/co2";
      DataSpace space;
      DataSpaceAddress address;
      boost::shared_ptr<Table> table;
      size_t scenarioId, dateId, attrId;

      {
        space.clear();
        address = space.address();
        BOOST_CHECK(driver.exists(name, space, address));
      }

      {
        space.clear();
        std::set<std::string> scenarios;
        scenarios.insert("aap");
        scenarios.insert("noot");
        space.addDimension(Dimension(Scenarios, scenarios));
        address = space.address();
        address.setCoordinate<std::string>(0, "noot");

        BOOST_CHECK(driver.exists(name, space, address));
      }

      {
        space.clear();
        space.addDimension(Dimension(Time, size_t(1), size_t(3), size_t(1)));
        address = space.address();
        address.setCoordinate<size_t>(0, 2);

        BOOST_CHECK(driver.exists(name, space, address));
      }

      {
        space.clear();
        std::set<std::string> scenarios;
        scenarios.insert("aap");
        scenarios.insert("noot");
        space.addDimension(Dimension(Scenarios, scenarios));
        space.addDimension(Dimension(Time, size_t(1), size_t(3), size_t(1)));
        address = space.address();
        address.setCoordinate<std::string>(0, "noot");
        address.setCoordinate<size_t>(1, 2);

        BOOST_REQUIRE(driver.exists(name, space, address));

        // Read selection.
        // Read one time step from one scenario.
        table.reset(driver.open(name, space, address));
        BOOST_REQUIRE(table);
        BOOST_CHECK_EQUAL(table->nrCols(), size_t(3));
        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(0));

        scenarioId = table->indexOf("scenario");
        BOOST_REQUIRE(scenarioId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(scenarioId), TI_STRING);
        BOOST_CHECK_EQUAL(table->title(scenarioId), "scenario");

        dateId = table->indexOf("date");
        BOOST_REQUIRE(dateId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(dateId), TI_INT4);
        BOOST_CHECK_EQUAL(table->title(dateId), "date");

        attrId = table->indexOf("co2");
        BOOST_REQUIRE(attrId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(attrId), TI_REAL4);
        BOOST_CHECK_EQUAL(table->title(attrId), "co2");

        BOOST_REQUIRE_NO_THROW(
          driver.read(*table, name, space, address);
        )

        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(1));
        BOOST_CHECK_CLOSE(table->col<REAL4>(2)[0], REAL4(2.02), REAL4(0.001));

        // Read selection.
        // Get values per scenario for a timestep.
        address.unsetCoordinate(0);

        table.reset(driver.open(name, space, address));
        BOOST_REQUIRE(table);
        BOOST_CHECK_EQUAL(table->nrCols(), size_t(3));
        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(0));

        scenarioId = table->indexOf("scenario");
        BOOST_REQUIRE(scenarioId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(scenarioId), TI_STRING);
        BOOST_CHECK_EQUAL(table->title(scenarioId), "scenario");

        dateId = table->indexOf("date");
        BOOST_REQUIRE(dateId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(dateId), TI_INT4);
        BOOST_CHECK_EQUAL(table->title(dateId), "date");

        attrId = table->indexOf("co2");
        BOOST_REQUIRE(attrId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(attrId), TI_REAL4);
        BOOST_CHECK_EQUAL(table->title(attrId), "co2");

        BOOST_REQUIRE_NO_THROW(
          driver.read(*table, name, space, address);
        )

        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(2));

        BOOST_CHECK_EQUAL(table->col<std::string>(scenarioId)[0], "aap");
        BOOST_CHECK_EQUAL(table->col<std::string>(scenarioId)[1], "noot");

        BOOST_CHECK_CLOSE(table->col<REAL4>(attrId)[0], REAL4(2.01), REAL4(0.001));
        BOOST_CHECK_CLOSE(table->col<REAL4>(attrId)[1], REAL4(2.02), REAL4(0.001));

        // Read selection.
        // Get values per time step for a scenario.
        address.setCoordinate<std::string>(0, "noot");
        address.unsetCoordinate(1);

        table.reset(driver.open(name, space, address));
        BOOST_REQUIRE(table);
        BOOST_CHECK_EQUAL(table->nrCols(), size_t(3));
        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(0));

        scenarioId = table->indexOf("scenario");
        BOOST_REQUIRE(scenarioId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(scenarioId), TI_STRING);
        BOOST_CHECK_EQUAL(table->title(scenarioId), "scenario");

        dateId = table->indexOf("date");
        BOOST_REQUIRE(dateId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(dateId), TI_INT4);
        BOOST_CHECK_EQUAL(table->title(dateId), "date");

        attrId = table->indexOf("co2");
        BOOST_REQUIRE(attrId < table->nrCols());
        BOOST_CHECK_EQUAL(table->typeId(attrId), TI_REAL4);
        BOOST_CHECK_EQUAL(table->title(attrId), "co2");

        BOOST_REQUIRE_NO_THROW(
          driver.read(*table, name, space, address);
        )

        BOOST_CHECK_EQUAL(table->nrRecs(), size_t(2));

        BOOST_CHECK_EQUAL(table->col<INT4>(dateId)[0], 1);
        BOOST_CHECK_EQUAL(table->col<INT4>(dateId)[1], 2);

        BOOST_CHECK_CLOSE(table->col<REAL4>(attrId)[0], REAL4(1.02), REAL4(0.001));
        BOOST_CHECK_CLOSE(table->col<REAL4>(attrId)[1], REAL4(2.02), REAL4(0.001));
      }
    }

    std::string d_user;

};


// //! suite
// boost::unit_test::test_suite*dal::SQLTableDriverTest::suite()
// {
//   boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
//   boost::shared_ptr<SQLTableDriverTest> instance(new SQLTableDriverTest());
//
//   suite->add(BOOST_CLASS_TEST_CASE(
//          &SQLTableDriverTest::testUserVar, instance));
//   suite->add(BOOST_CLASS_TEST_CASE(
//          &SQLTableDriverTest::testDriverIsAvailable, instance));
//   suite->add(BOOST_CLASS_TEST_CASE(&SQLTableDriverTest::testConstructor,
//          instance));
//
//   if(SQLTableDriver::driverIsAvailable("QSQLITE")) {
//     suite->add(BOOST_CLASS_TEST_CASE(&SQLTableDriverTest::testSQLite,
//          instance));
//   }
//
//   // TODO uncomment. first figure out this odbc data set naming stuff again.
//   // if(SQLTableDriver::driverIsAvailable("QODBC3")) {
//   //   suite->add(BOOST_CLASS_TEST_CASE(&SQLTableDriverTest::testODBC,
//   //        instance));
//   // }
//
//   // if(SQLTableDriver::driverIsAvailable("QPSQL7")) {
//   //   suite->add(BOOST_CLASS_TEST_CASE(&SQLTableDriverTest::testPostgreSQL,
//   //        instance));
//   // }
//
//   return suite;
// }


// //! ctor
// SQLTableDriverTest::SQLTableDriverTest()
//
//  : d_user(dev::environmentVariableSet("USER")
//      ? dev::environmentVariable("USER")
//      : dev::environmentVariable("LOGNAME"))
//
// {
// }


BOOST_FIXTURE_TEST_SUITE(sql_table_driver, Fixture)

// BOOST_AUTO_TEST_CASE(user_var)
// {
//   using namespace dal;
//
//   BOOST_REQUIRE(!d_user.empty());
// }


BOOST_AUTO_TEST_CASE(driver_is_available)
{
  using namespace dal;

  // Some drivers we always want to support.
  BOOST_REQUIRE(SQLTableDriver::driverIsAvailable("QSQLITE"));
  // BOOST_REQUIRE(SQLTableDriver::driverIsAvailable("QODBC3"));

  // One that does not exist.
  BOOST_CHECK(!SQLTableDriver::driverIsAvailable("QDOESNOTEXIST"));
}


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace dal;

  BOOST_CHECK_NO_THROW(SQLTableDriver("QSQLITE"));

  bool exceptionThrown;

  try {
    SQLTableDriver driver("bla");
    exceptionThrown = false;
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(),
         "SQL table driver for bla: Not available");
  }

  BOOST_CHECK(exceptionThrown);
}


BOOST_AUTO_TEST_CASE(odbc)
{
  using namespace dal;

  // TODO Fix. first figure out this odbc data set naming stuff again.
  return;

  if(!SQLTableDriver::driverIsAvailable("QODBC3")) {
      return;
  }

  test("QODBC3");
}


BOOST_AUTO_TEST_CASE(sqlite)
{
  using namespace dal;

  if(!SQLTableDriver::driverIsAvailable("QSQLITE")) {
      return;
  }

  test("QSQLITE");

  SQLTableDriver driver("QSQLITE");

  // When opening a connection to a not existing database, this driver default
  // creates an empty file. We don't want this behaviour when we are just
  // opening (probing) a dataset.
  // This code checks whether this behaviour still works.
  {
    std::string name = "DoesNotExist";
    BOOST_CHECK(!boost::filesystem::exists(name));

    boost::shared_ptr<Dataset> dataset(dynamic_cast<Driver const&>(
         driver).open(name));

    BOOST_CHECK(!dataset);
    BOOST_CHECK(!boost::filesystem::exists(name));
  }

  // Read a time series from a table in an SQLite database.
  {
    std::string name = "MyDatabase.sql3/timesteps/co2";

    BOOST_REQUIRE(boost::filesystem::exists("MyDatabase.sql3"));
    BOOST_CHECK(dynamic_cast<Driver const&>(driver).exists(name));

    DataSpace space = dynamic_cast<Driver const&>(driver).dataSpace(name);
    BOOST_REQUIRE_EQUAL(space.rank(), 1u);
    BOOST_CHECK_EQUAL(space.dimension(0).meaning(), Time);
    BOOST_REQUIRE_EQUAL(space.dimension(0).nrValues(), 3u);
    BOOST_CHECK_EQUAL(space.dimension(0).value<size_t>(0), 1u);
    BOOST_CHECK_EQUAL(space.dimension(0).value<size_t>(1), 3u);
    BOOST_CHECK_EQUAL(space.dimension(0).value<size_t>(2), 1u);

    boost::shared_ptr<Dataset> dataset(dynamic_cast<Driver const&>(
         driver).open(name));
    BOOST_CHECK(dataset);

    Table* table = dynamic_cast<Table*>(dataset.get());
    BOOST_REQUIRE(table);
    BOOST_CHECK_EQUAL(table->nrCols(), 2u);
    BOOST_CHECK_EQUAL(table->title(0), "date");
    BOOST_CHECK_EQUAL(table->title(1), "co2");
    BOOST_CHECK_EQUAL(table->typeId(0), TI_INT4);
    BOOST_CHECK_EQUAL(table->typeId(1), TI_REAL4);

    BOOST_CHECK_NO_THROW(dynamic_cast<TableDriver const&>(driver).read(
         *table, name));

    Array<INT4> const& date(table->col<INT4>(0));
    BOOST_CHECK_EQUAL(date.size(), 3u);

    Array<REAL4> const& co2(table->col<REAL4>(1));
    BOOST_CHECK_EQUAL(co2.size(), 3u);

    BOOST_CHECK_EQUAL(date[0], 1);
    BOOST_CHECK_EQUAL(date[1], 2);
    BOOST_CHECK_EQUAL(date[2], 3);

    BOOST_CHECK_CLOSE(co2[0], REAL4(1.1), REAL4(0.001));
    BOOST_CHECK_CLOSE(co2[1], REAL4(2.2), REAL4(0.001));
    BOOST_CHECK_CLOSE(co2[2], REAL4(3.3), REAL4(0.001));
  }

  // Read quantiles from a table in an SQLite database.
  {
    std::string name = "MyDatabase.sql3/quantiles/co2";

    BOOST_REQUIRE(boost::filesystem::exists("MyDatabase.sql3"));
    BOOST_CHECK(dynamic_cast<Driver const&>(driver).exists(name));

    DataSpace space = dynamic_cast<Driver const&>(driver).dataSpace(name);
    BOOST_REQUIRE_EQUAL(space.rank(), 1u);
    BOOST_CHECK_EQUAL(space.dimension(0).meaning(), CumulativeProbabilities);
    BOOST_REQUIRE_EQUAL(space.dimension(0).nrValues(), 3u);
    BOOST_CHECK_CLOSE(space.dimension(0).value<REAL4>(0), REAL4(0.10),
         REAL4(0.01));
    BOOST_CHECK_CLOSE(space.dimension(0).value<REAL4>(1), REAL4(0.90),
         REAL4(0.01));
    BOOST_CHECK_CLOSE(space.dimension(0).value<REAL4>(2), REAL4(0.05),
         REAL4(0.01));

    boost::shared_ptr<Dataset> dataset(dynamic_cast<Driver const&>(
         driver).open(name));
    BOOST_CHECK(dataset);

    Table* table = dynamic_cast<Table*>(dataset.get());
    BOOST_REQUIRE(table);
    BOOST_CHECK_EQUAL(table->nrCols(), 2u);
    BOOST_CHECK_EQUAL(table->title(0), "quantile");
    BOOST_CHECK_EQUAL(table->title(1), "co2");
    BOOST_CHECK_EQUAL(table->typeId(0), TI_REAL4);
    BOOST_CHECK_EQUAL(table->typeId(1), TI_REAL4);

    BOOST_CHECK_NO_THROW(dynamic_cast<TableDriver const&>(driver).read(
         *table, name));

    Array<REAL4> const& quantile(table->col<REAL4>(0));
    BOOST_CHECK_EQUAL(quantile.size(), 5u);

    Array<REAL4> const& co2(table->col<REAL4>(1));
    BOOST_CHECK_EQUAL(co2.size(), 5u);

    BOOST_CHECK_CLOSE(quantile[0], REAL4(0.10), REAL4(0.01));
    BOOST_CHECK_CLOSE(quantile[1], REAL4(0.25), REAL4(0.01));
    BOOST_CHECK_CLOSE(quantile[2], REAL4(0.50), REAL4(0.01));
    BOOST_CHECK_CLOSE(quantile[3], REAL4(0.75), REAL4(0.01));
    BOOST_CHECK_CLOSE(quantile[4], REAL4(0.90), REAL4(0.01));

    BOOST_CHECK_CLOSE(co2[0], REAL4(1.1), REAL4(0.01));
    BOOST_CHECK_CLOSE(co2[1], REAL4(2.2), REAL4(0.01));
    BOOST_CHECK_CLOSE(co2[2], REAL4(3.3), REAL4(0.01));
    BOOST_CHECK_CLOSE(co2[3], REAL4(4.4), REAL4(0.01));
    BOOST_CHECK_CLOSE(co2[4], REAL4(5.5), REAL4(0.01));
  }
}


BOOST_AUTO_TEST_CASE(postgresql)
{
  using namespace dal;

  // TODO Fix.
  return;

  if(!SQLTableDriver::driverIsAvailable("QPSQL7")) {
      return;
  }

  test("QPSQL7");
}

BOOST_AUTO_TEST_SUITE_END()

#endif // QT_NO_SQL
