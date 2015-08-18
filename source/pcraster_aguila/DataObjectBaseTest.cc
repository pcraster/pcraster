#include "DataObjectBaseTest.h"

// External headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// Project headers.

// Module headers.
#include "ag_RasterDataSources.h"



/*!
  \file
  This file contains the implementation of the DataObjectBaseTest class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAOBJECTBASETEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* DataObjectBaseTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DataObjectBaseTest> instance(
         new DataObjectBaseTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataObjectBaseTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DATAOBJECTBASETEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
DataObjectBaseTest::DataObjectBaseTest()
{
}



void DataObjectBaseTest::test()
{
  std::string name("dataset1/aap/scalar");
  dal::DataSpace space;
  space.addDimension(dal::Dimension(dal::Time, size_t(10), size_t(20),
         size_t(1)));

  RasterDataSources dataSources;
  BOOST_CHECK_EQUAL(dataSources.size(), size_t(0));
  BOOST_CHECK(dataSources.empty());

  DataGuide guide1 = dataSources.add(name, space);
  BOOST_CHECK_EQUAL(dataSources.size(), size_t(1));
  BOOST_CHECK(!dataSources.empty());

  DataGuide guide2 = dataSources.add(name, space);
  BOOST_CHECK_EQUAL(dataSources.size(), size_t(1));
  BOOST_CHECK(!dataSources.empty());

  BOOST_CHECK_EQUAL(dataSources._manager.size(), size_t(1));

  dataSources.clear();
  BOOST_CHECK_EQUAL(dataSources.size(), size_t(0));
  BOOST_CHECK(dataSources.empty());

  BOOST_CHECK(dataSources._manager.empty());
}

} // namespace ag

