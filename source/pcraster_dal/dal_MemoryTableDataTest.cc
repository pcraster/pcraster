#define BOOST_TEST_MODULE pcraster dal memory_table_data
#include <boost/test/unit_test.hpp>
#include "dal_MemoryTableData.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace dal;

  std::vector<std::string> titles;
  titles.push_back("col1");
  titles.push_back("col2");
  titles.push_back("col3");

  std::vector<TypeId> typeIds;
  typeIds.push_back(TI_STRING);
  typeIds.push_back(TI_UINT1);
  typeIds.push_back(TI_REAL4);

  // Empty data space.
  {
    boost::shared_ptr<Table> table1(new Table("table1", titles, typeIds));
    DataSpace space;
    DataSpaceAddress address;
    MemoryTableData data(space, address, table1.get());
    BOOST_CHECK(data.dataSpace() == space);
    BOOST_CHECK(data.exists(address));
    Table const* tmpTable1(data.table(address));
    BOOST_CHECK(tmpTable1);
    BOOST_CHECK(*tmpTable1 == *table1);
    BOOST_CHECK(tmpTable1 == table1.get()); // Table returned pointer passed in.
  }

  // Data space with one time dimension.
  std::vector<size_t> timeSteps;
  timeSteps.push_back(1);
  timeSteps.push_back(100);
  timeSteps.push_back(1);
  Dimension timeDimension(Time, timeSteps);

  {
    boost::shared_ptr<Table> table1(new Table("table1", titles, typeIds));
    DataSpace space;
    space.addDimension(timeDimension);
    DataSpaceAddress address(space.address());
    address.setCoordinate<size_t>(0, 5);
    MemoryTableData data(space, address, table1.get());
    BOOST_CHECK(data.dataSpace() == space);
    BOOST_CHECK(data.exists(address));
    Table const* tmpTable1(data.table(address));
    BOOST_CHECK(tmpTable1);
    BOOST_CHECK(*tmpTable1 == *table1);
    BOOST_CHECK(tmpTable1 == table1.get()); // Table returned pointer passed in.

    address.setCoordinate<size_t>(0, 4);
    BOOST_CHECK(!data.exists(address));
  }

  // Data space with scenarios/time dimensions.
  std::set<std::string> scenarios;
  scenarios.insert("aap");
  scenarios.insert("noot");
  scenarios.insert("mies");
  Dimension scenarioDimension(Scenarios, scenarios);

  {
    boost::shared_ptr<Table> table1(new Table("table1", titles, typeIds));
    DataSpace space;
    space.addDimension(scenarioDimension);
    space.addDimension(timeDimension);
    DataSpaceAddress address(space.address());
    address.setCoordinate<std::string>(0, "aap");
    address.setCoordinate<size_t>(1, 5);
    MemoryTableData data(space, address, table1.get());
    BOOST_CHECK(data.dataSpace() == space);
    BOOST_CHECK(data.exists(address));
    Table const* tmpTable1(data.table(address));
    BOOST_CHECK(tmpTable1);
    BOOST_CHECK(*tmpTable1 == *table1);
    BOOST_CHECK(tmpTable1 == table1.get()); // Table returned pointer passed in.

    address.setCoordinate<std::string>(0, "noot");
    BOOST_CHECK(!data.exists(address));

    address.setCoordinate<std::string>(0, "aap");
    BOOST_CHECK(data.exists(address));

    address.setCoordinate<size_t>(1, 4);
    BOOST_CHECK(!data.exists(address));
  }
}
