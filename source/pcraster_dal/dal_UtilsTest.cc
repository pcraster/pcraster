#define BOOST_TEST_MODULE pcraster dal utils
#include <boost/test/unit_test.hpp>
#include "dal_CSFRasterDriver.h"
#include "dal_Dal.h"
#include "dal_DataSpace.h"
#include "dal_DataSpaceAddress.h"
#include "dal_Exception.h"
#include "dal_Utils.h"
#include "dal_Client.h"


class ClientWrapper : public dal::Client {
public:
  ClientWrapper(boost::filesystem::path const& prefix,
                   bool addAllDrivers=false,
                   bool cacheDatasetInfo=true)
  : dal::Client(prefix) {
  }
};

struct Fixture
{

    Fixture()
    {
        static ClientWrapper client("/my/path/utils_test", true);
    }

    ~Fixture()
    {
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(dataset_type)
{
  using namespace dal;

  DatasetType type;
  std::string name;
  DataSpace space;
  Dal dal(false);
  Driver* driver = new CSFRasterDriver();
  dal.add(driver);

  {
    name = "soil";
    // TODO
    // Default extension is not added currently.
    // type = dal.datasetType(name);           // Finds soil.map.
    // BOOST_CHECK_EQUAL(type, RASTER);

    std::vector<size_t> timeSteps;
    timeSteps.push_back(size_t(1));
    timeSteps.push_back(size_t(10));
    timeSteps.push_back(size_t(1));

    space.addDimension(Dimension(Time, timeSteps));
    BOOST_CHECK(space.hasTime());

    type = dal.datasetType(name, space);    // Finds soil0000.010.
    BOOST_CHECK_EQUAL(type, RASTER);
  }

  dal.remove(driver);
  delete driver;
}


BOOST_AUTO_TEST_CASE(split_name_and_selection)
{
  using namespace dal;

  {
    std::string name = "table{1,3,q}";
    boost::tuple<std::string, std::vector<std::string> > tuple =
           splitNameAndSelection(name);

    BOOST_CHECK_EQUAL(boost::get<0>(tuple), "table");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple).size(), size_t(3));
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[0], "1");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[1], "3");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[2], "q");
  }

  {
    std::string name = "table{}";
    boost::tuple<std::string, std::vector<std::string> > tuple =
           splitNameAndSelection(name);

    BOOST_CHECK_EQUAL(boost::get<0>(tuple), "table");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple).size(), size_t(0));
  }

  {
    std::string name = "table{1}";
    boost::tuple<std::string, std::vector<std::string> > tuple =
           splitNameAndSelection(name);

    BOOST_CHECK_EQUAL(boost::get<0>(tuple), "table");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple).size(), size_t(1));
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[0], "1");
  }

  {
    std::string name = "table{1,2}";
    boost::tuple<std::string, std::vector<std::string> > tuple =
           splitNameAndSelection(name);

    BOOST_CHECK_EQUAL(boost::get<0>(tuple), "table");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple).size(), size_t(2));
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[0], "1");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[1], "2");
  }

  {
    std::string name = "table{1, 2}";
    boost::tuple<std::string, std::vector<std::string> > tuple =
           splitNameAndSelection(name);

    BOOST_CHECK_EQUAL(boost::get<0>(tuple), "table");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple).size(), size_t(2));
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[0], "1");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[1], "2");
  }

  {
    std::string name = "table { 1, 2 }";
    boost::tuple<std::string, std::vector<std::string> > tuple =
           splitNameAndSelection(name);

    BOOST_CHECK_EQUAL(boost::get<0>(tuple), "table ");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple).size(), size_t(2));
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[0], "1");
    BOOST_CHECK_EQUAL(boost::get<1>(tuple)[1], "2");
  }

  {
    // Input string is not trimmed.
    std::string name = "table{1, 2} ";
    boost::tuple<std::string, std::vector<std::string> > tuple =
           splitNameAndSelection(name);

    BOOST_CHECK_EQUAL(boost::get<0>(tuple), name);
    BOOST_CHECK_EQUAL(boost::get<1>(tuple).size(), size_t(0));
  }
}


BOOST_AUTO_TEST_CASE(data_space_to_string)
{
  using namespace dal;

  DataSpace space;
  std::vector<size_t> timeSteps;
  timeSteps.push_back(1);
  timeSteps.push_back(10);
  timeSteps.push_back(1);
  space.addDimension(Dimension(Time, timeSteps));

  BOOST_CHECK_EQUAL(dataSpaceToString(space), "/time[1, 10, 1]");
}


BOOST_AUTO_TEST_CASE(data_space_to_field_names)
{
  using namespace dal;

  DataSpace space;
  std::set<std::string> fieldNames;

  std::set<std::string> scenarios;
  scenarios.insert("aap");
  scenarios.insert("noot");
  scenarios.insert("mies");
  Dimension scenario(Scenarios, scenarios);

  Dimension time(Time, size_t(1), size_t(10), size_t(1));
  Dimension uncertainty(CumulativeProbabilities, 0.01f, 0.99f, 0.01f);

  {
    fieldNames.clear();

    BOOST_CHECK(dataSpaceToFieldNames(space) == fieldNames);
  }

  {
    space.clear();
    space.addDimension(scenario);

    fieldNames.clear();
    fieldNames.insert("scenario");

    BOOST_CHECK(dataSpaceToFieldNames(space) == fieldNames);
  }

  {
    space.clear();
    space.addDimension(uncertainty);

    fieldNames.clear();
    fieldNames.insert("quantile");

    BOOST_CHECK(dataSpaceToFieldNames(space) == fieldNames);
  }

  {
    space.clear();
    space.addDimension(time);

    fieldNames.clear();
    fieldNames.insert("date");

    BOOST_CHECK(dataSpaceToFieldNames(space) == fieldNames);
  }

  {
    space.clear();
    space.addDimension(scenario);
    space.addDimension(time);
    space.addDimension(uncertainty);

    fieldNames.clear();
    fieldNames.insert("scenario");
    fieldNames.insert("date");
    fieldNames.insert("quantile");

    BOOST_CHECK(dataSpaceToFieldNames(space) == fieldNames);
  }
}


BOOST_AUTO_TEST_CASE(data_space_address_to_sql_query)
{
  using namespace dal;

  DataSpace space;
  DataSpaceAddress address;
  std::string tableName = "MyTable";
  std::string fieldName = "Co2";

  std::set<std::string> scenarios;
  scenarios.insert("aap");
  scenarios.insert("noot");
  scenarios.insert("mies");
  Dimension scenario(Scenarios, scenarios);

  std::vector<size_t> timeSteps;
  timeSteps.push_back(1);
  timeSteps.push_back(10);
  timeSteps.push_back(1);
  Dimension time(Time, timeSteps);

  std::vector<float> quantiles;
  quantiles.push_back(0.01f);
  quantiles.push_back(0.99f);
  quantiles.push_back(0.01f);
  Dimension uncertainty(CumulativeProbabilities, quantiles);

  {
    BOOST_CHECK_EQUAL(dataSpaceAddressToSqlQuery(space, address, tableName,
         fieldName),
         "SELECT Co2 FROM MyTable");
  }

  {
    space.clear();
    space.addDimension(scenario);
    address = space.address();

    BOOST_CHECK_EQUAL(dataSpaceAddressToSqlQuery(space, address, tableName,
         fieldName),
         "SELECT scenario,Co2 FROM MyTable");

    address.setCoordinate<std::string>(0, "aap");

    BOOST_CHECK_EQUAL(dataSpaceAddressToSqlQuery(space, address, tableName,
         fieldName),
         "SELECT scenario,Co2 FROM MyTable WHERE scenario='aap'");
  }

  {
    space.clear();
    space.addDimension(uncertainty);
    address = space.address();

    BOOST_CHECK_EQUAL(dataSpaceAddressToSqlQuery(space, address, tableName,
         fieldName),
         "SELECT quantile,Co2 FROM MyTable");

    address.setCoordinate<float>(0, 0.5);

    BOOST_CHECK_EQUAL(dataSpaceAddressToSqlQuery(space, address, tableName,
         fieldName),
         "SELECT quantile,Co2 FROM MyTable WHERE quantile=0.5");
  }

  {
    space.clear();
    space.addDimension(time);
    address = space.address();

    BOOST_CHECK_EQUAL(dataSpaceAddressToSqlQuery(space, address, tableName,
         fieldName),
         "SELECT date,Co2 FROM MyTable");

    address.setCoordinate<size_t>(0, 3);

    BOOST_CHECK_EQUAL(dataSpaceAddressToSqlQuery(space, address, tableName,
         fieldName),
         "SELECT date,Co2 FROM MyTable WHERE date=3");
  }

  {
    space.clear();
    space.addDimension(scenario);
    space.addDimension(time);
    space.addDimension(uncertainty);
    address = space.address();

    BOOST_CHECK_EQUAL(dataSpaceAddressToSqlQuery(space, address, tableName,
         fieldName),
         "SELECT scenario,date,quantile,Co2 FROM MyTable");

    address.setCoordinate<std::string>(0, "aap");
    address.setCoordinate<size_t>(1, 3);
    address.setCoordinate<float>(2, 0.5);

    BOOST_CHECK_EQUAL(dataSpaceAddressToSqlQuery(space, address, tableName,
         fieldName),
         "SELECT scenario,date,quantile,Co2 FROM MyTable "
              "WHERE scenario='aap' AND date=3 AND quantile=0.5");
  }
}
