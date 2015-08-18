#ifndef INCLUDED_DAL_DATASPACEADDRESSMAPPERTEST
#include "dal_DataSpaceAddressMapperTest.h"
#define INCLUDED_DAL_DATASPACEADDRESSMAPPERTEST
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
#ifndef INCLUDED_DAL_DATASPACEADDRESSMAPPER
#include "dal_DataSpaceAddressMapper.h"
#define INCLUDED_DAL_DATASPACEADDRESSMAPPER
#endif



/*!
  \file
  This file contains the implementation of the DataSpaceAddressMapperTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASPACEADDRESSMAPPER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*DataSpaceAddressMapperTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DataSpaceAddressMapperTest> instance(new DataSpaceAddressMapperTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DataSpaceAddressMapperTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DATASPACEADDRESSMAPPER MEMBERS
//------------------------------------------------------------------------------

//! ctor
DataSpaceAddressMapperTest::DataSpaceAddressMapperTest(
         )
{
}



//! setUp
void DataSpaceAddressMapperTest::setUp()
{
}



//! tearDown
void DataSpaceAddressMapperTest::tearDown()
{
}



void DataSpaceAddressMapperTest::test()
{
  {
    std::set<std::string> scenarios;
    std::vector<size_t> timeSteps, rows, cols;
    scenarios.insert("aap");
    timeSteps.push_back(1);
    timeSteps.push_back(100);
    timeSteps.push_back(1);
    rows.push_back(1);
    rows.push_back(100);
    rows.push_back(1);
    cols.push_back(1);
    cols.push_back(100);
    cols.push_back(1);

    DataSpace space;
    space.addDimension(Dimension(Scenarios, scenarios));
    space.addDimension(Dimension(Time, timeSteps));
    space.addDimension(Dimension(Space, rows));
    space.addDimension(Dimension(Space, cols));

    DataSpaceAddressMapper mapper(space);

    DataSpaceAddress address = space.address();
    address.setCoordinate<std::string>(0, "aap");
    address.setCoordinate<size_t>(1, 50);
    address.setCoordinate<size_t>(2, 12);
    address.setCoordinate<size_t>(3, 13);

    BOOST_CHECK_EQUAL(mapper.toString(address, 0), "aap");
    BOOST_CHECK_EQUAL(mapper.toString(address, 1), "50");
    BOOST_CHECK_EQUAL(mapper.toString(address, 2), "12");
    BOOST_CHECK_EQUAL(mapper.toString(address, 3), "13");
    BOOST_CHECK_EQUAL(mapper.toString(address), "/aap/50/12/13");
  }
}



} // namespace dal

