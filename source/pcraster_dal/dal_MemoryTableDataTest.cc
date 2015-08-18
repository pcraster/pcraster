#ifndef INCLUDED_DAL_MEMORYTABLEDATATEST
#include "dal_MemoryTableDataTest.h"
#define INCLUDED_DAL_MEMORYTABLEDATATEST
#endif

// Library headers.
#include <boost/shared_ptr.hpp>

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
#ifndef INCLUDED_DAL_MEMORYTABLEDATA
#include "dal_MemoryTableData.h"
#define INCLUDED_DAL_MEMORYTABLEDATA
#endif



/*!
  \file
  This file contains the implementation of the MemoryTableDataTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYTABLEDATA MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*MemoryTableDataTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MemoryTableDataTest> instance(new MemoryTableDataTest());

  suite->add(BOOST_CLASS_TEST_CASE(&MemoryTableDataTest::testConstructor, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYTABLEDATA MEMBERS
//------------------------------------------------------------------------------

//! ctor
MemoryTableDataTest::MemoryTableDataTest(
         )
{
}



//! setUp
void MemoryTableDataTest::setUp()
{
}



//! tearDown
void MemoryTableDataTest::tearDown()
{
}



void MemoryTableDataTest::testConstructor()
{
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

} // namespace dal

