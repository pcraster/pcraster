#define BOOST_TEST_MODULE pcraster dal table_dal
#include <boost/test/unit_test.hpp>
#include "dal_Table.h"
#include "dal_TableDal.h"
#include "dal_TableDriver.h"
#define protected public
#include "dal_Client.h"



struct Fixture
{

    Fixture()
    {
        static dal::Client client("/my/path/table_dal_test", true);
    }

    ~Fixture()
    {
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(test)
{
  using namespace dal;

  {
    // name = currentPathname + sometablename;
    std::string name = "table1.eas";
    TableDal dal;
    BOOST_CHECK(dal.hasDriverByName("Geo-EAS"));
    BOOST_CHECK_EQUAL(dal.driverByName("Geo-EAS")->datasetType(), TABLE);
    boost::shared_ptr<Table> table;
    boost::tie(table, boost::tuples::ignore) = dal.open(name);
    BOOST_CHECK(table.get());
  }
}
