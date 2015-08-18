#ifndef INCLUDED_DAL_DATASPACEITERATORTEST
#include "dal_DataSpaceIteratorTest.h"
#define INCLUDED_DAL_DATASPACEITERATORTEST
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

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
#ifndef INCLUDED_DAL_DATASPACEITERATOR
#include "dal_DataSpaceIterator.h"
#define INCLUDED_DAL_DATASPACEITERATOR
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



/*!
  \file
  This file contains the implementation of the DataSpaceIteratorTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASPACEITERATOR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::DataSpaceIteratorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DataSpaceIteratorTest> instance(new DataSpaceIteratorTest());

  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testEmptySpace, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testScenarios, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testRangeOfCumProbabilities, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testSamples, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testTime, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testSpace, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testFeatureSpace, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testScenariosSamples, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testScenarioCumProbabilities, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &DataSpaceIteratorTest::testSpaceWithEmptyDimensions, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DATASPACEITERATOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::DataSpaceIteratorTest::DataSpaceIteratorTest()
{
}



//! setUp
void dal::DataSpaceIteratorTest::setUp()
{
  std::set<std::string> scenarios;
  scenarios.insert("aap");
  scenarios.insert("noot");
  scenarios.insert("mies");
  d_scenarios = Dimension(Scenarios, scenarios);

  std::vector<float> quantiles;
  quantiles.push_back(0.01f);
  quantiles.push_back(0.99f);
  quantiles.push_back(0.01f);
  d_quantiles = Dimension(CumulativeProbabilities, quantiles);

  std::vector<size_t> samples;
  samples.push_back(3);
  samples.push_back(9);
  samples.push_back(2);
  d_samples = Dimension(Samples, samples);

  std::vector<size_t> timeSteps;
  timeSteps.push_back(1);
  timeSteps.push_back(10);
  timeSteps.push_back(1);
  d_timeSteps = Dimension(Time, timeSteps);

  d_raster = Dimension(Space, RegularDiscretisation, RasterDimensions(3, 2));

  d_feature = Dimension(Space, BorderedDiscretisation,
         SpaceDimensions(6.6, -5.5, 6.6, -5.5));
}



//! tearDown
void dal::DataSpaceIteratorTest::tearDown()
{
  d_space.clear();
}



void dal::DataSpaceIteratorTest::testEmptySpace()
{
  BOOST_CHECK(d_space.begin() == d_space.end());
}



void dal::DataSpaceIteratorTest::testScenarios()
{
  setUp();
  d_space.addDimension(d_scenarios);

  DataSpaceIterator it;
  DataSpaceAddress address;

  it = d_space.begin();
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  address = d_space.address();

  address.setCoordinate<std::string>(0, "aap");
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<std::string>(0, "mies");
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<std::string>(0, "noot");
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  BOOST_CHECK(it == d_space.end());
  BOOST_CHECK(it != d_space.begin());

  // ---------------------------------------------------------------------------

  --it;
  address.setCoordinate<std::string>(0, "noot");
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<std::string>(0, "mies");
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<std::string>(0, "aap");
  BOOST_CHECK(d_space.equal(*it, address));
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  tearDown();
}



void dal::DataSpaceIteratorTest::testRangeOfCumProbabilities()
{
  setUp();
  d_space.addDimension(d_quantiles);

  DataSpaceIterator it;
  DataSpaceAddress address;

  it = d_space.begin(); // 0.01
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  address = d_space.address();

  for(size_t i = 0; i < 89; ++i) {
    ++it;
  } // 0.9

  address.setCoordinate<float>(0, 0.9f);
  BOOST_CHECK(d_space.equal(*it, address));

  while(it != d_space.end()) {
    ++it;
  }

  BOOST_CHECK(it != d_space.begin());
  BOOST_CHECK(it == d_space.end());

  --it;
  address.setCoordinate<float>(0, 0.99f);
  BOOST_CHECK(d_space.equal(*it, address));
  tearDown();
}



void dal::DataSpaceIteratorTest::testSamples()
{
  setUp();
  d_space.addDimension(d_samples);

  DataSpaceIterator it;
  DataSpaceAddress address;

  it = d_space.begin();
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  address = d_space.address();

  address.setCoordinate<size_t>(0, 3);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  BOOST_CHECK(it == d_space.end());
  BOOST_CHECK(it != d_space.begin());

  --it;
  address.setCoordinate<size_t>(0, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 3);
  BOOST_CHECK(d_space.equal(*it, address));
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  tearDown();
}



void dal::DataSpaceIteratorTest::testTime()
{
  setUp();
  d_space.addDimension(d_timeSteps);

  DataSpaceIterator it;
  DataSpaceAddress address;

  it = d_space.begin();
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  address = d_space.address();

  address.setCoordinate<size_t>(0, 1);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 2);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 3);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 4);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 6);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 8);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 10);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  BOOST_CHECK(it == d_space.end());
  BOOST_CHECK(it != d_space.begin());

  // ---------------------------------------------------------------------------

  --it;
  address.setCoordinate<size_t>(0, 10);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 8);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 6);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 4);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 3);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 2);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(0, 1);
  BOOST_CHECK(d_space.equal(*it, address));
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  tearDown();
}



void dal::DataSpaceIteratorTest::testSpace()
{
  setUp();
  d_space.addDimension(d_raster);

  DataSpaceIterator it;
  DataSpaceAddress address;

  it = d_space.begin();
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  address = d_space.address();

  // First row.
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(0.5, -0.5));
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(1.5, -0.5));
  BOOST_CHECK(d_space.equal(*it, address));

  // Second row.
  ++it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(0.5, -1.5));
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(1.5, -1.5));
  BOOST_CHECK(d_space.equal(*it, address));

  // Third row.
  ++it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(0.5, -2.5));
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(1.5, -2.5));
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  BOOST_CHECK(it == d_space.end());
  BOOST_CHECK(it != d_space.begin());

  // ---------------------------------------------------------------------------

  // Third row.
  --it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(1.5, -2.5));
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(0.5, -2.5));
  BOOST_CHECK(d_space.equal(*it, address));

  // Second row.
  --it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(1.5, -1.5));
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(0.5, -1.5));
  BOOST_CHECK(d_space.equal(*it, address));

  // First row.
  --it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(1.5, -0.5));
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(0.5, -0.5));
  BOOST_CHECK(d_space.equal(*it, address));
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  tearDown();
}



void dal::DataSpaceIteratorTest::testFeatureSpace()
{
  setUp();
  d_space.addDimension(d_feature);

  DataSpaceIterator it;
  DataSpaceAddress address;

  it = d_space.begin();
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  address = d_space.address();

  address.setCoordinate<SpatialCoordinate>(0, SpatialCoordinate(6.6, -5.5));
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  BOOST_CHECK(it == d_space.end());
  BOOST_CHECK(it != d_space.begin());

  --it;
  BOOST_CHECK(d_space.equal(*it, address));
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());

  tearDown();
}



void dal::DataSpaceIteratorTest::testScenariosSamples()
{
  setUp();
  d_space.addDimension(d_scenarios);
  d_space.addDimension(d_samples);

  DataSpaceIterator it;
  DataSpaceAddress address;

  it = d_space.begin();
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  address = d_space.address();

  address.setCoordinate<std::string>(0, "aap");
  address.setCoordinate<size_t>(1, 3);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(1, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(1, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(1, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<std::string>(0, "mies");
  address.setCoordinate<size_t>(1, 3);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(1, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(1, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(1, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<std::string>(0, "noot");
  address.setCoordinate<size_t>(1, 3);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(1, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(1, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(1, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  BOOST_CHECK(it == d_space.end());
  BOOST_CHECK(it != d_space.begin());

  // ---------------------------------------------------------------------------

  --it;
  address.setCoordinate<std::string>(0, "noot");
  address.setCoordinate<size_t>(1, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(1, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(1, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(1, 3);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<std::string>(0, "mies");
  address.setCoordinate<size_t>(1, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(1, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(1, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(1, 3);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<std::string>(0, "aap");
  address.setCoordinate<size_t>(1, 9);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(1, 7);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(1, 5);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<size_t>(1, 3);
  BOOST_CHECK(d_space.equal(*it, address));
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  tearDown();
}



void dal::DataSpaceIteratorTest::testScenarioCumProbabilities()
{
  setUp();
  d_space.addDimension(d_scenarios);
  d_space.addDimension(d_quantiles);

  DataSpaceIterator it;
  DataSpaceAddress address;

  it = d_space.begin();
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  address = d_space.address();

  address.setCoordinate<std::string>(0, "aap");
  address.setCoordinate<float>(1, 0.01f);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<float>(1, 0.02f);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<float>(1, 0.03f);
  BOOST_CHECK(d_space.equal(*it, address));

  for(size_t i = 0; i < 96; ++i) {
    ++it;
  }

  address.setCoordinate<float>(1, 0.99f);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<std::string>(0, "mies");
  address.setCoordinate<float>(1, 0.01f);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<float>(1, 0.02f);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<float>(1, 0.03f);
  BOOST_CHECK(d_space.equal(*it, address));

  for(size_t i = 0; i < 96; ++i) {
    ++it;
  }

  address.setCoordinate<float>(1, 0.99f);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<std::string>(0, "noot");
  address.setCoordinate<float>(1, 0.01f);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<float>(1, 0.02f);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  address.setCoordinate<float>(1, 0.03f);
  BOOST_CHECK(d_space.equal(*it, address));

  for(size_t i = 0; i < 96; ++i) {
    ++it;
  }

  address.setCoordinate<float>(1, 0.99f);
  BOOST_CHECK(d_space.equal(*it, address));

  ++it;
  BOOST_CHECK(it == d_space.end());
  BOOST_CHECK(it != d_space.begin());

  // ---------------------------------------------------------------------------

  --it;
  address.setCoordinate<std::string>(0, "noot");
  address.setCoordinate<float>(1, 0.99f);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<float>(1, 0.98f);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<float>(1, 0.97f);
  BOOST_CHECK(d_space.equal(*it, address));

  for(size_t i = 0; i < 96; ++i) {
    --it;
  }

  address.setCoordinate<float>(1, 0.01f);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<std::string>(0, "mies");
  address.setCoordinate<float>(1, 0.99f);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<float>(1, 0.98f);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<float>(1, 0.97f);
  BOOST_CHECK(d_space.equal(*it, address));

  for(size_t i = 0; i < 96; ++i) {
    --it;
  }

  address.setCoordinate<float>(1, 0.01f);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<std::string>(0, "aap");
  address.setCoordinate<float>(1, 0.99f);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<float>(1, 0.98f);
  BOOST_CHECK(d_space.equal(*it, address));

  --it;
  address.setCoordinate<float>(1, 0.97f);
  BOOST_CHECK(d_space.equal(*it, address));

  for(size_t i = 0; i < 96; ++i) {
    --it;
  }

  address.setCoordinate<float>(1, 0.01f);
  BOOST_CHECK(d_space.equal(*it, address));
  BOOST_CHECK(it == d_space.begin());
  BOOST_CHECK(it != d_space.end());
  tearDown();
}



void dal::DataSpaceIteratorTest::testSpaceWithEmptyDimensions()
{
  setUp();
  DataSpace space;

  std::vector<size_t> timeSteps;
  timeSteps.push_back(10);
  timeSteps.push_back(12);
  timeSteps.push_back(1);

  RasterDimensions cells(3, 3);

  space.addDimension(Dimension(Time, timeSteps));
  space.addDimension(Dimension(Space, RegularDiscretisation, cells));

  DataSpaceAddress address(space.address());
  DataSpaceIterator it = space.begin();

  // First time step, first row, first col
  address.setCoordinate<size_t>(0, 10);
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.5, -0.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(1.5, -0.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(2.5, -0.5));
  BOOST_CHECK(space.equal(*it, address));

  // First time step, second row, first col
  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.5, -1.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(1.5, -1.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(2.5, -1.5));
  BOOST_CHECK(space.equal(*it, address));

  // First time step, third row, first col
  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.5, -2.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(1.5, -2.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(2.5, -2.5));
  BOOST_CHECK(space.equal(*it, address));

  // Second time step, first row, first col
  address.setCoordinate<size_t>(0, 11);
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.5, -0.5));
  ++it;
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(1.5, -0.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(2.5, -0.5));
  BOOST_CHECK(space.equal(*it, address));

  // Second time step, second row, first col
  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.5, -1.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(1.5, -1.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(2.5, -1.5));
  BOOST_CHECK(space.equal(*it, address));

  // Second time step, third row, first col
  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.5, -2.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(1.5, -2.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(2.5, -2.5));
  BOOST_CHECK(space.equal(*it, address));

  // Third time step, first row, first col
  address.setCoordinate<size_t>(0, 12);
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.5, -0.5));
  ++it;
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(1.5, -0.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(2.5, -0.5));
  BOOST_CHECK(space.equal(*it, address));

  // Third time step, second row, first col
  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.5, -1.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(1.5, -1.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(2.5, -1.5));
  BOOST_CHECK(space.equal(*it, address));

  // Third time step, third row, first col
  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.5, -2.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(1.5, -2.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(2.5, -2.5));
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  BOOST_CHECK(it == space.end());

  // ---------------------------------------------------------------------------
  // Same iteration, but now with the space dimensions unset. In effect we
  // are iterating only over time.

  space.eraseDimension(Space);

  it = space.begin();
  address = space.address();
  address.setCoordinate<size_t>(0, 10);
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 11);
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  address.setCoordinate<size_t>(0, 12);
  BOOST_CHECK(space.equal(*it, address));

  ++it;
  BOOST_CHECK(it == space.end());
  tearDown();
}

