#define BOOST_TEST_MODULE pcraster dal table_dal
#include <boost/test/unit_test.hpp>
#include "dal_Table.h"
#include "dal_TableDal.h"
#include "dal_TableDriver.h"
#include "dal_Client.h"


class ClientWrapper : public dal::Client {
public:
  ClientWrapper(std::filesystem::path const& prefix,
                   bool  /*addAllDrivers*/=false,
                   bool  /*cacheDatasetInfo*/=true)
  : dal::Client(prefix) {
  }
};

struct Fixture
{

    Fixture()
    {
        static ClientWrapper const client("/my/path/table_dal_test", true);
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
    std::string const name = "table1.eas";
    TableDal dal;
    BOOST_TEST(dal.hasDriverByName("Geo-EAS"));
    BOOST_CHECK_EQUAL(dal.driverByName("Geo-EAS")->datasetType(), TABLE);
    std::shared_ptr<Table> table;
    std::tie(table, std::ignore) = dal.open(name);
    BOOST_TEST(table.get());
  }
}
