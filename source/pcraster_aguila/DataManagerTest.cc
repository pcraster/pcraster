#include "DataManagerTest.h"

// External headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// Project headers.

// Module headers.
#include "ag_DataManager.h"
#include "ag_Raster.h"



/*!
  \file
  This file contains the implementation of the DataManagerTest class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAMANAGERTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* DataManagerTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DataManagerTest> instance(
         new DataManagerTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataManagerTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DATAMANAGERTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
DataManagerTest::DataManagerTest()
{
}



void DataManagerTest::test()
{
  std::string name("dataset1/aap/scalar");
  dal::DataSpace space;
  space.addDimension(dal::Dimension(dal::Time, size_t(10), size_t(20),
         size_t(1)));

  Raster raster(name, space);
  DataInfo<Raster> data(&raster, VS_SCALAR);

  DataManager<Raster> manager(geo::STACK);
  BOOST_CHECK_EQUAL(manager.size(), size_t(0));
  BOOST_CHECK(manager.empty());

  DataGuide guide1 = manager.add(data);
  BOOST_CHECK_EQUAL(manager.size(), size_t(1));
  BOOST_CHECK(!manager.empty());

  // 2nd guide is pointing to the same data as the first.
  DataGuide guide2 = manager.add(data);
  BOOST_CHECK_EQUAL(manager.size(), size_t(1));
  BOOST_CHECK(!manager.empty());

  BOOST_CHECK(guide1 == guide2);

  BOOST_CHECK(manager.isValid(guide1));
  BOOST_CHECK(manager.exists(&raster));
  BOOST_CHECK_EQUAL(&manager.data(guide1), &raster);

  manager.remove(guide1);

  BOOST_CHECK_EQUAL(manager.size(), size_t(0));
  BOOST_CHECK(manager.empty());
}

} // namespace ag

