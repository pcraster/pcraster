#define BOOST_TEST_MODULE pcraster dal data_space_iterator
#include <boost/test/unit_test.hpp>
#include <iostream>
#include "dal_DataSpace.h"
#include "dal_DataSpaceIterator.h"
#include "dal_Dimension.h"
#include "dal_RasterDimensions.h"
#include "dal_Utils.h"


struct Fixture
{

    Fixture()
    {
        using namespace dal;

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

    ~Fixture()
    {
        // d_space.clear();
    }

    dal::Dimension d_scenarios;
    dal::Dimension d_quantiles;
    dal::Dimension d_samples;
    dal::Dimension d_timeSteps;
    dal::Dimension d_raster;
    dal::Dimension d_feature;
    dal::DataSpace d_space;

};


BOOST_FIXTURE_TEST_SUITE(data_space_iterator, Fixture)


BOOST_AUTO_TEST_CASE(empty_space)
{
  using namespace dal;

  BOOST_CHECK(d_space.begin() == d_space.end());
}


BOOST_AUTO_TEST_CASE(scenarios)
{
  using namespace dal;

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
}


BOOST_AUTO_TEST_CASE(range_of_cum_probabilities)
{
  using namespace dal;

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
}


BOOST_AUTO_TEST_CASE(samples)
{
  using namespace dal;

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
}


BOOST_AUTO_TEST_CASE(time)
{
  using namespace dal;

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
}


BOOST_AUTO_TEST_CASE(space)
{
  using namespace dal;

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
}


BOOST_AUTO_TEST_CASE(feature_space)
{
  using namespace dal;

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
}


BOOST_AUTO_TEST_CASE(scenarios_samples)
{
  using namespace dal;

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
}


BOOST_AUTO_TEST_CASE(scenario_cum_probabilities)
{
  using namespace dal;

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
}


BOOST_AUTO_TEST_CASE(space_with_empty_dimensions)
{
  using namespace dal;

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
}

BOOST_AUTO_TEST_SUITE_END()
