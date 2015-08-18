#ifndef INCLUDED_DAL_DALTEST
#include "dal_DalTest.h"
#define INCLUDED_DAL_DALTEST
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>
#define INCLUDED_BOOST_FILESYSTEM
#endif

#include <boost/shared_ptr.hpp>

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
#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif

#ifndef INCLUDED_DAL_DATASET
#include "dal_Dataset.h"
#define INCLUDED_DAL_DATASET
#endif

#ifndef INCLUDED_DAL_TEXTTABLEDRIVER
#include "dal_TextTableDriver.h"
#define INCLUDED_DAL_TEXTTABLEDRIVER
#endif



/*!
  \file
  This file contains the implementation of the DalTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DAL MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::DalTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DalTest> instance(new DalTest());

  suite->add(BOOST_CLASS_TEST_CASE(
         &DalTest::testNotExisting, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DalTest::testTable, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DalTest::testDatasetDriverTupleManagement, instance));

  return suite;
}



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF DAL MEMBERS
//------------------------------------------------------------------------------

//! ctor
DalTest::DalTest()
{
}



//! setUp
void DalTest::setUp()
{
}



//! tearDown
void DalTest::tearDown()
{
}



void DalTest::testNotExisting(
         std::string const& name)
{
  BOOST_CHECK(!boost::filesystem::exists(name));

  Dal dal(true);
  boost::shared_ptr<Dataset> dataset;
  Driver* driver;
  boost::tie(dataset, driver) = dal.open(name);

  BOOST_CHECK(!dataset);
  BOOST_CHECK(!boost::filesystem::exists(name));
  BOOST_CHECK(!driver);
}



void DalTest::testNotExisting()
{
  testNotExisting("DoesNotExist.map");
  testNotExisting("DoesNotExist");
}



void DalTest::testTable()
{
  std::string filename;
  boost::shared_ptr<Dataset> dataset;
  Driver* driver;

  Dal dal;
  TextTableDriver tableDriver;
  dal.add(&tableDriver);

  {
    filename = "doesnotexist";
    boost::tie(dataset, driver) = dal.open(filename);
    BOOST_CHECK(!dataset);
    BOOST_CHECK(!driver);
  }

  {
    filename = "emptyfile";
    boost::tie(dataset, driver) = dal.open(filename);
    BOOST_CHECK(!dataset);
    BOOST_CHECK(!driver);
  }

  {
    filename = "table1.col";
    boost::tie(dataset, driver) = dal.open(filename);
    BOOST_CHECK(dataset);
    BOOST_CHECK(driver);
    // BOOST_CHECK_EQUAL(dataset->name(), filename);
    BOOST_CHECK_EQUAL(dataset->type(), TABLE);

    boost::shared_ptr<Table> table = boost::dynamic_pointer_cast<Table>(
        dataset);
    BOOST_CHECK(table);
    // More tests in TableTest.
  }
}



void DalTest::testDatasetDriverTupleManagement()
{
  Dal dal(true);

  // Read the same raster multiple times.
  std::string filename = "dtmsmall.map";

  BOOST_CHECK(dal._driversByDataset.empty());

  for(size_t i = 0; i < 10; ++i) {
    boost::shared_ptr<Dataset> dataset;
    boost::tie(dataset, boost::tuples::ignore) = dal.open(filename);
    BOOST_CHECK(dataset);
    BOOST_CHECK_EQUAL(dal._driversByDataset.size(), size_t(1));
  }

  dal._driversByDataset.clear();

  boost::shared_ptr<Dataset> dataset;
  boost::tie(dataset, boost::tuples::ignore) = dal.open(filename);
  BOOST_CHECK(dataset);
  dal.removeDriverFromCache(dal.driverByDataset(filename, DataSpace()));
  BOOST_CHECK(dal._driversByDataset.empty());
}


} // namespace dal
