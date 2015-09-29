#define BOOST_TEST_MODULE pcraster dal data_space_test
#include <boost/test/unit_test.hpp>
#include "dal_DataSpace.h"
#include "dal_DataSpaceAddress.h"
#include "dal_Dimension.h"
#include "dal_RasterDimensions.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  {
    // scenario / time / space / space

    std::set<std::string> scenarios;
    std::vector<size_t> timeSteps;
    scenarios.insert("aap");
    timeSteps.push_back(1);
    timeSteps.push_back(100);
    timeSteps.push_back(1);

    RasterDimensions rasterDimensions(100, 100);

    DataSpace space;
    space.addDimension(Dimension(Scenarios, scenarios));
    space.addDimension(Dimension(Time, timeSteps));
    space.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));
    BOOST_CHECK_EQUAL(space.rank(), size_t(3));
    BOOST_CHECK(space.isSpatial());
    BOOST_CHECK(space.hasRaster());
    BOOST_CHECK(space.hasTime());
    BOOST_CHECK_EQUAL(space.nrWideDimensions(), size_t(2));

    DataSpaceAddress address = space.address();
    address.setCoordinate<std::string>(0, "aap");
    address.setCoordinate<size_t>(1, 50);
    address.setCoordinate<SpatialCoordinate>(2, SpatialCoordinate(44.4, -55.5));

    DataSpace space2(space, address);
    BOOST_CHECK_EQUAL(space2.size(), space.size());
    BOOST_CHECK_EQUAL(space2.nrWideDimensions(), size_t(0));

    for(size_t i = 0; i < space2.size(); ++i) {
      BOOST_CHECK_EQUAL(space2.dimension(i).coordinateType(),
         space.dimension(i).coordinateType());
      BOOST_CHECK_EQUAL(space2.dimension(i).meaning(),
         space.dimension(i).meaning());
      BOOST_CHECK_EQUAL(space2.dimension(i).discretisation(),
         space.dimension(i).discretisation());
    }

    BOOST_CHECK_EQUAL(space2.dimension(0).nrValues(), size_t(1));
    BOOST_CHECK_EQUAL(space2.dimension(0).value<std::string>(0), "aap");
    BOOST_CHECK_EQUAL(space2.dimension(1).nrValues(), size_t(3));
    BOOST_CHECK_EQUAL(space2.dimension(1).value<size_t>(0), size_t(50));
    BOOST_CHECK_EQUAL(space2.dimension(1).value<size_t>(1), size_t(50));
    BOOST_CHECK_EQUAL(space2.dimension(1).value<size_t>(2), size_t(1));
    BOOST_CHECK(space2.dimension(2).value<RasterDimensions>(0) ==
         RasterDimensions(1, 1, 1.0, 44.0, -55.0));
  }
}


BOOST_AUTO_TEST_CASE(merge)
{
  using namespace dal;

  std::set<std::string> scenarios;
  scenarios.insert("aap");

  RasterDimensions rasterDimensions(100, 60);

  Dimension scenariosDimension(Scenarios, scenarios);
  Dimension samplesDimension(Samples, size_t(1), size_t(1000), size_t(1));
  Dimension cummulativeProbabilitiesDimension(CumulativeProbabilities,
         float(0.01), float(0.99), float(0.01));
  Dimension timeDimension(Time, size_t(1), size_t(1000), size_t(1));
  Dimension spaceDimension(Space, RegularDiscretisation, rasterDimensions);

  {
    DataSpace space1, space2, space3;

    space1.addDimension(scenariosDimension);
    space1.addDimension(timeDimension);
    space1.addDimension(spaceDimension);

    BOOST_CHECK(space2.isEmpty());
    BOOST_CHECK(space3.isEmpty());

    space2 |= space1;
    BOOST_CHECK_EQUAL(space2.rank(), size_t(3));
    BOOST_CHECK(space2.hasScenarios());
    BOOST_CHECK(space2.hasTime());
    BOOST_CHECK(space2.isSpatial());
    BOOST_CHECK(space2.hasRaster());
    BOOST_CHECK(space2 == space1);

    // Again.
    space2 |= space1;
    BOOST_CHECK_EQUAL(space2.rank(), size_t(3));
    BOOST_CHECK(space2.hasScenarios());
    BOOST_CHECK(space2.hasTime());
    BOOST_CHECK(space2.isSpatial());
    BOOST_CHECK(space2.hasRaster());
    BOOST_CHECK(space2 == space1);

    // Other way around.
    space1 |= space2;
    BOOST_CHECK_EQUAL(space1.rank(), size_t(3));
    BOOST_CHECK(space1.hasScenarios());
    BOOST_CHECK(space1.hasTime());
    BOOST_CHECK(space1.isSpatial());
    BOOST_CHECK(space1.hasRaster());
    BOOST_CHECK(space1 == space2);

    // Merge with empty space.
    space1 |= space3;
    BOOST_CHECK(space3.isEmpty());
    BOOST_CHECK_EQUAL(space1.rank(), size_t(3));
    BOOST_CHECK(space1.hasScenarios());
    BOOST_CHECK(space1.hasTime());
    BOOST_CHECK(space1.isSpatial());
    BOOST_CHECK(space1.hasRaster());
    BOOST_CHECK(space1 == space2);
  }

  // Without Scenarios, Samples and CumulativeProbabilities.
  // - Time (time series)
  // - Space (raster)
  // - Time/Space (dynamic raster: stack)
  {
    DataSpace space1, space2;

    space1.addDimension(timeDimension);
    space2.addDimension(spaceDimension);

    space1 |= space2;
    BOOST_CHECK_EQUAL(space1.dimension(0).meaning(), Time);
    BOOST_CHECK_EQUAL(space1.dimension(1).meaning(), Space);
    BOOST_CHECK(space1.dimension(1).value<RasterDimensions>(0) ==
         rasterDimensions);

    // The other way around.
    space1.clear();
    space1.addDimension(timeDimension);

    space2 |= space1;
    BOOST_CHECK_EQUAL(space2.dimension(0).meaning(), Time);
    BOOST_CHECK_EQUAL(space2.dimension(1).meaning(), Space);
    BOOST_CHECK(space2.dimension(1).value<RasterDimensions>(0) ==
         rasterDimensions);
  }

  // With Scenarios.
  // - Scenarios/Space (for each scenario a raster)
  // - Scenarios/Time (for each scenario a time series)
  // - Scenarios/Time/Space (for each scenario a stack)
  {
    DataSpace space1, space2;

    space1.addDimension(scenariosDimension);
    space2.addDimension(timeDimension);

    space1 |= space2;
    BOOST_CHECK_EQUAL(space1.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space1.dimension(1).meaning(), Time);

    space1.clear();
    space1.addDimension(scenariosDimension);

    space2 |= space1;
    BOOST_CHECK_EQUAL(space2.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space2.dimension(1).meaning(), Time);
  }

  // With Samples.
  // - Scenarios/Samples/Space
  // - Scenarios/Samples/Time
  // - Scenarios/Samples/Time/Space
  {
    DataSpace space1, space2, space3;

    space1.addDimension(scenariosDimension);
    space2.addDimension(samplesDimension);
    space3.addDimension(timeDimension);

    space1 |= space2;
    BOOST_CHECK_EQUAL(space1.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space1.dimension(1).meaning(), Samples);

    space1 |= space3;
    BOOST_CHECK_EQUAL(space1.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space1.dimension(1).meaning(), Samples);
    BOOST_CHECK_EQUAL(space1.dimension(2).meaning(), Time);

    space1.clear();
    space1.addDimension(scenariosDimension);

    space2 |= space1;
    BOOST_CHECK_EQUAL(space2.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space2.dimension(1).meaning(), Samples);

    space2 |= space3;
    BOOST_CHECK_EQUAL(space2.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space2.dimension(1).meaning(), Samples);
    BOOST_CHECK_EQUAL(space2.dimension(2).meaning(), Time);

    space2.clear();
    space2.addDimension(samplesDimension);

    space3 |= space1;
    BOOST_CHECK_EQUAL(space3.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space3.dimension(1).meaning(), Time);

    space3 |= space2;
    BOOST_CHECK_EQUAL(space3.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space3.dimension(1).meaning(), Samples);
    BOOST_CHECK_EQUAL(space3.dimension(2).meaning(), Time);
  }

  // With CumulativeProbabilities.
  // - Scenarios/CumulativeProbabilities/Space
  // - Scenarios/Time/CumulativeProbabilities
  // - Scenarios/Time/CumulativeProbabilities/Space
  {
    DataSpace space1, space2, space3;

    space1.addDimension(scenariosDimension);
    space2.addDimension(timeDimension);
    space3.addDimension(cummulativeProbabilitiesDimension);

    space1 |= space2;
    BOOST_CHECK_EQUAL(space1.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space1.dimension(1).meaning(), Time);

    space1 |= space3;
    BOOST_CHECK_EQUAL(space1.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space1.dimension(1).meaning(), Time);
    BOOST_CHECK_EQUAL(space1.dimension(2).meaning(), CumulativeProbabilities);

    space1.clear();
    space1.addDimension(scenariosDimension);

    space2 |= space1;
    BOOST_CHECK_EQUAL(space2.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space2.dimension(1).meaning(), Time);

    space2 |= space3;
    BOOST_CHECK_EQUAL(space2.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space2.dimension(1).meaning(), Time);
    BOOST_CHECK_EQUAL(space2.dimension(2).meaning(), CumulativeProbabilities);

    space2.clear();
    space2.addDimension(timeDimension);

    space3 |= space1;
    BOOST_CHECK_EQUAL(space3.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space3.dimension(1).meaning(), CumulativeProbabilities);

    space3 |= space2;
    BOOST_CHECK_EQUAL(space3.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(space3.dimension(1).meaning(), Time);
    BOOST_CHECK_EQUAL(space3.dimension(2).meaning(), CumulativeProbabilities);
  }

  {
    // Because of bug (only in release mode, see arrow below):
    // space1: /scenarios{0}
    //         /time[1986, 2003, 1]
    //         /cumulative probabilities[0.01, 0.99, 0.01]
    //         /space[1, 87, 1]/[1, 55, 1]
    // space2: /scenarios{5000}
    //         /time[1986, 2003, 1]
    //         /cumulative probabilities[0.01, 0.99, 0.01]
    //         /space[1, 87, 1]/[1, 55, 1]
    // result: /scenarios{0, 5000}
    //         /time[1986, 2003, 1]
    //         /cumulative probabilities[0.01, 0.99, -2.14748e-07]   <---------
    //         /space[1, 87, 1]/[1, 55, 1]
    DataSpace space1, space2;

    std::set<std::string> scenarios;
    std::vector<size_t> timeSteps;
    std::vector<float>quantiles;
    scenarios.insert("0");
    timeSteps.push_back(1986);
    timeSteps.push_back(2003);
    timeSteps.push_back(1);
    quantiles.push_back(0.01f);
    quantiles.push_back(0.99f);
    quantiles.push_back(0.01f);

    RasterDimensions rasterDimensions(87, 55);

    space1.addDimension(Dimension(Scenarios, scenarios));
    space1.addDimension(Dimension(Time, timeSteps));
    space1.addDimension(Dimension(CumulativeProbabilities, quantiles));
    space1.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    scenarios.clear();
    scenarios.insert("5000");
    space2.addDimension(Dimension(Scenarios, scenarios));
    space2.addDimension(Dimension(Time, timeSteps));
    space2.addDimension(Dimension(CumulativeProbabilities, quantiles));
    space2.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    space1 |= space2;
    BOOST_CHECK_EQUAL(space1.size(), size_t(4));
    BOOST_CHECK_EQUAL(space1.dimension(2).nrValues(), size_t(3));
    BOOST_CHECK(comparable<float>(space1.dimension(2).value<float>(2), 0.01f));
  }

  // Merge space with different discretizations (raster and vector).
  // Merging a raster and a vector data space should result in a data space
  // with info about both types of dimensions. Raster info must come before
  // vector info.
  {
    DataSpace resultSpace, rasterSpace, vectorSpace;

    RasterDimensions rasterDimensions(87, 55);
    rasterSpace.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    SpaceDimensions featureDimensions(555.5, 222.2, 666.6, -111.1);
    vectorSpace.addDimension(Dimension(Space, BorderedDiscretisation,
         featureDimensions));

    {
      resultSpace = rasterSpace | vectorSpace;
      BOOST_CHECK_EQUAL(resultSpace.size(), size_t(2));
      BOOST_CHECK(resultSpace.dimension(0).value<RasterDimensions>(0) ==
         rasterDimensions);
      BOOST_CHECK(resultSpace.dimension(1).value<SpaceDimensions>(0) ==
         featureDimensions);
    }

    {
      resultSpace = vectorSpace | rasterSpace;
      BOOST_CHECK_EQUAL(resultSpace.size(), size_t(2));
      BOOST_CHECK(resultSpace.dimension(0).value<RasterDimensions>(0) ==
         rasterDimensions);
      BOOST_CHECK(resultSpace.dimension(1).value<SpaceDimensions>(0) ==
         featureDimensions);
    }
  }

  // Real world example than failed.
  // space1: scenarios/time/probabilities/raster/features
  // space2: scenarios/time/probabilities/features
  // result: scenarios/time/probabilities/raster/features
  {
    DataSpace resultSpace, space1, space2;

    std::set<std::string> scenarios;
    scenarios.insert("1000");
    SpaceDimensions featureDimensions(464260, 6.18012e+06, 791405, 5.68706e+06);
    RasterDimensions rasterDimensions(87, 55, 5000.0, 464000.0, 6131500.0);

    space2.addDimension(Dimension(Scenarios, scenarios));
    space2.addDimension(Dimension(Time, size_t(1986), size_t(2003), size_t(1)));
    space2.addDimension(Dimension(CumulativeProbabilities, float(0.01),
         float(0.99), float(0.01)));
    space2.addDimension(Dimension(Space, BorderedDiscretisation,
         featureDimensions));

    scenarios.insert("0");
    scenarios.insert("5000");
    scenarios.insert("25000");

    space1.addDimension(Dimension(Scenarios, scenarios));
    space1.addDimension(Dimension(Time, size_t(1986), size_t(2003), size_t(1)));
    space1.addDimension(Dimension(CumulativeProbabilities, float(0.01),
         float(0.99), float(0.01)));
    space1.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));
    space1.addDimension(Dimension(Space, BorderedDiscretisation,
         featureDimensions));

    resultSpace = space1 | space2;

    BOOST_CHECK_EQUAL(resultSpace.size(), size_t(5));
    BOOST_CHECK_EQUAL(resultSpace.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(resultSpace.dimension(1).meaning(), Time);
    BOOST_CHECK_EQUAL(resultSpace.dimension(2).meaning(),
         CumulativeProbabilities);
    BOOST_CHECK_EQUAL(resultSpace.dimension(3).meaning(), Space);
    BOOST_CHECK_EQUAL(resultSpace.dimension(3).discretisation(),
         RegularDiscretisation);
    BOOST_CHECK_EQUAL(resultSpace.dimension(4).meaning(), Space);
    BOOST_CHECK_EQUAL(resultSpace.dimension(4).discretisation(),
         BorderedDiscretisation);

    // And the other way around.
    resultSpace = space2 | space1;

    BOOST_CHECK_EQUAL(resultSpace.size(), size_t(5));
    BOOST_CHECK_EQUAL(resultSpace.dimension(0).meaning(), Scenarios);
    BOOST_CHECK_EQUAL(resultSpace.dimension(1).meaning(), Time);
    BOOST_CHECK_EQUAL(resultSpace.dimension(2).meaning(),
         CumulativeProbabilities);
    BOOST_CHECK_EQUAL(resultSpace.dimension(3).meaning(), Space);
    BOOST_CHECK_EQUAL(resultSpace.dimension(3).discretisation(),
         RegularDiscretisation);
    BOOST_CHECK_EQUAL(resultSpace.dimension(4).meaning(), Space);
    BOOST_CHECK_EQUAL(resultSpace.dimension(4).discretisation(),
         BorderedDiscretisation);
  }
}


BOOST_AUTO_TEST_CASE(intersect)
{
  using namespace dal;

  std::set<std::string> scenarios;
  scenarios.insert("aap");
  scenarios.insert("noot");
  scenarios.insert("mies");

  std::vector<size_t> timeSteps;
  timeSteps.push_back(size_t(1));
  timeSteps.push_back(size_t(100));
  timeSteps.push_back(size_t(1));

  RasterDimensions rasterDimensions(100, 100);

  // Result of intersecting an empty space with whatever other space is
  // the empty space.
  {
    DataSpace space1;
    DataSpace space2;
    space1 &= space2;
    BOOST_CHECK(space1.isEmpty());

    space2.addDimension(Dimension(Scenarios, scenarios));
    space2.addDimension(Dimension(Time, timeSteps));
    space2.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    space1 &= space2;
    BOOST_CHECK(space1.isEmpty());
  }

  // Result of intersecting whatever space with an empty space is the
  // empty space.
  {
    DataSpace space1;
    DataSpace space2;

    space1.addDimension(Dimension(Scenarios, scenarios));
    space1.addDimension(Dimension(Time, timeSteps));
    space1.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    space1 &= space2;
    BOOST_CHECK(space1.isEmpty());
  }

  // Result of intersecting two spaces with different dimensions is an
  // empty space.
  {
    DataSpace space1;
    space1.addDimension(Dimension(Scenarios, scenarios));

    DataSpace space2;
    space2.addDimension(Dimension(Time, timeSteps));

    space1 &= space2;
    BOOST_CHECK(space1.isEmpty());
  }

  // Result of intersecting two space with the same dimensions is this
  // set of equal dimensions.
  {
    DataSpace space1;
    DataSpace space2;

    space1.addDimension(Dimension(Scenarios, scenarios));
    space1.addDimension(Dimension(Time, timeSteps));
    space1.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    space2.addDimension(Dimension(Scenarios, scenarios));

    space1 &= space2;
    BOOST_CHECK_EQUAL(space1.size(), size_t(1));
    BOOST_CHECK_EQUAL(space1.dimension(0).meaning(), Scenarios);
  }

  {
    DataSpace space1;
    DataSpace space2;

    space1.addDimension(Dimension(Scenarios, scenarios));
    space1.addDimension(Dimension(Time, timeSteps));
    space1.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    space2.addDimension(Dimension(Time, timeSteps));

    space1 &= space2;
    BOOST_CHECK_EQUAL(space1.size(), size_t(1));
    BOOST_CHECK_EQUAL(space1.dimension(0).meaning(), Time);
  }

  {
    DataSpace space1;
    DataSpace space2;

    space1.addDimension(Dimension(Scenarios, scenarios));
    space1.addDimension(Dimension(Time, timeSteps));
    space1.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    space2.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    space1 &= space2;
    BOOST_CHECK_EQUAL(space1.size(), size_t(1));
    BOOST_CHECK_EQUAL(space1.dimension(0).meaning(), Space);
  }

  // 
  {
  }
}


BOOST_AUTO_TEST_CASE(trim)
{
  using namespace dal;

  std::set<std::string> scenarios;
  std::vector<size_t> timeSteps;
  scenarios.insert("aap");
  timeSteps.push_back(1);
  timeSteps.push_back(100);
  timeSteps.push_back(1);

  RasterDimensions rasterDimensions(100, 100);

  DataSpace appSpace;
  appSpace.addDimension(Dimension(Scenarios, scenarios));
  appSpace.addDimension(Dimension(Time, timeSteps));
  appSpace.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

  {
    // Trim an address in scenario / time / space / space to
    // address in space / space.

    RasterDimensions rasterDimensions(10, 55);

    DataSpace dataSpace;
    dataSpace.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    DataSpaceAddress address(3);
    address.setCoordinate<std::string>(0, "aap");
    address.setCoordinate<size_t>(1, 5);
    address.setCoordinate<SpatialCoordinate>(2, SpatialCoordinate(5.5, 6.6));

    address = dataSpace.trim(appSpace, address);
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    BOOST_CHECK(address.coordinate<SpatialCoordinate>(0) ==
         SpatialCoordinate(5.5, 6.6));
  }

  {
    // Trim an address in scenario / time / space / space to
    // address in scenario / time.

    std::set<std::string> scenarios;
    std::vector<size_t> timeSteps;
    scenarios.insert("aap");
    timeSteps.push_back(5);
    timeSteps.push_back(10);
    timeSteps.push_back(1);

    DataSpace dataSpace;
    dataSpace.addDimension(Dimension(Scenarios, scenarios));
    dataSpace.addDimension(Dimension(Time, timeSteps));

    DataSpaceAddress address(3);
    address.setCoordinate<std::string>(0, "aap");
    address.setCoordinate<size_t>(1, 5);
    address.setCoordinate<SpatialCoordinate>(2, SpatialCoordinate(5.5, 6.6));

    address = dataSpace.trim(appSpace, address);
    BOOST_CHECK_EQUAL(address.size(), size_t(2));
    BOOST_CHECK_EQUAL(address.coordinate<std::string>(0), "aap");
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(1), size_t(5));
  }

  {
    // Trim an address in scenario / time / space / space to
    // address in time.

    std::vector<size_t> timeSteps;
    timeSteps.push_back(9);
    timeSteps.push_back(20);
    timeSteps.push_back(1);

    DataSpace dataSpace;
    dataSpace.addDimension(Dimension(Time, timeSteps));

    DataSpaceAddress address(3);
    address.setCoordinate<std::string>(0, "aap");
    address.setCoordinate<size_t>(1, 5);
    address.setCoordinate<SpatialCoordinate>(2, SpatialCoordinate(5.5, 6.6));

    address = dataSpace.trim(appSpace, address);
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0), size_t(5));
  }

  {
    // Trim an address to a space with less dimensions.

    std::vector<size_t> timeSteps, cells;
    timeSteps.push_back(9);
    timeSteps.push_back(11);
    timeSteps.push_back(1);

    RasterDimensions rasterDimensions(10, 10);

    DataSpace fromSpace, toSpace;
    fromSpace.addDimension(Dimension(Time, timeSteps));
    fromSpace.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));
    toSpace = fromSpace;
    toSpace.eraseDimension(Space);

    DataSpaceAddress address(2);
    address.setCoordinate<size_t>(0, 9);
    address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(5.5, 6.6));
    address = toSpace.trim(fromSpace, address);
    BOOST_CHECK_EQUAL(address.size(), size_t(1));

    BOOST_CHECK(address.isValid(0));
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0), size_t(9));
  }

  {
    // NOTE: Tests updated to new approach to spatial dimensions. Comment
    // NOTE: left, but refers to previous approach.

    // A test from an Aguila use case.
    // space:
    //   /scenarios{0, 10000, 25000, 5000}
    //     /time[1986, 2003, 1]
    //       /cumulative probabilities[0.02, 0.98, 0.01]
    //         /space[1, 87, 1]
    //         /space[1, 55, 1]
    // address: (0, 1986, 0.5, 1, 1)
    //
    // trim to:
    // space:
    //   /space[1, 87, 1]
    //   /space[1, 55, 1]
    //
    // must result in:
    // address: (1, 1)

    std::set<std::string> scenarios;
    scenarios.insert("0");
    scenarios.insert("100000");
    scenarios.insert("25000");
    scenarios.insert("5000");

    std::vector<size_t> timeSteps;
    timeSteps.push_back(1986);
    timeSteps.push_back(2003);
    timeSteps.push_back(1);

    std::vector<float>quantiles;
    quantiles.push_back(0.01f);
    quantiles.push_back(0.99f);
    quantiles.push_back(0.01f);

    RasterDimensions rasterDimensions(87, 55);

    DataSpace fromSpace;
    fromSpace.addDimension(Dimension(Scenarios, scenarios));
    fromSpace.addDimension(Dimension(Time, timeSteps));
    fromSpace.addDimension(Dimension(CumulativeProbabilities, quantiles));
    fromSpace.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    DataSpace toSpace;
    toSpace.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    // address: (0, 1986, 0.5, (5.5, 6.6))
    DataSpaceAddress address(4);
    address.setCoordinate<std::string>(0, "0");
    address.setCoordinate<size_t>(1, 1986);
    address.setCoordinate<float>(2, 0.5f);
    address.setCoordinate<SpatialCoordinate>(3, SpatialCoordinate(5.5, 6.6));
    address = toSpace.trim(fromSpace, address);

    BOOST_REQUIRE(toSpace.isValid(address));
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    BOOST_REQUIRE(address.isValid(0));
    BOOST_CHECK(address.coordinate<SpatialCoordinate>(0) ==
         SpatialCoordinate(5.5, 6.6));

    // Additional space dimensions.
    SpaceDimensions featureDimension(5.68706e+06, 791405.0, 6.18012e+06,
         464260.0);

    // Same source space, add these:
    //   space[464260, 791405]
    //   space[5.68706e+06, 6.18012e+06]
    fromSpace.addDimension(Dimension(Space, BorderedDiscretisation,
         featureDimension));

    toSpace.clear();
    toSpace.addDimension(Dimension(Space, BorderedDiscretisation,
         featureDimension));

    // trim to: /space[464260, 791405]/space[5.68706e+06, 6.18012e+06]
    // address: (0, 1986, 0.5, (1, 1), (464260, 5687057))
    address.resize(5);
    address.setCoordinate<std::string>(0, "0");
    address.setCoordinate<size_t>(1, 1986);
    address.setCoordinate<float>(2, 0.5f);
    address.setCoordinate<SpatialCoordinate>(3, SpatialCoordinate(464260.0,
         5687057.0));
    address.setCoordinate<SpatialCoordinate>(4, SpatialCoordinate(464260.0,
         5687057.0));

    address = toSpace.trim(fromSpace, address);

    BOOST_REQUIRE(toSpace.isValid(address));
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    BOOST_REQUIRE(address.isValid(0));
    BOOST_CHECK(address.coordinate<SpatialCoordinate>(0) ==
         SpatialCoordinate(464260.0, 5687057.0));
  }
}


BOOST_AUTO_TEST_CASE(initialise_invalid_coordinates)
{
  using namespace dal;

  DataSpace space;
  DataSpaceAddress address;

  {
    // Empty space.
    address = space.address();
    BOOST_CHECK(space.isValid(address));
    address = space.initialiseInvalidCoordinates(address);
    BOOST_CHECK(space.isValid(address));
    BOOST_CHECK_EQUAL(address.size(), size_t(0));
  }

  std::set<std::string> scenarios;
  scenarios.insert("aap");
  scenarios.insert("noot");
  scenarios.insert("mies");
  space.addDimension(Dimension(Scenarios, scenarios));

  {
    // One scenario dimension added.
    address = space.address();
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    BOOST_CHECK(!space.isValid(address));
    BOOST_CHECK(!address.isValid(0));

    address = space.initialiseInvalidCoordinates(address);
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    BOOST_CHECK(space.isValid(address));
    BOOST_CHECK(address.isValid(0));
    BOOST_CHECK_EQUAL(address.coordinate<std::string>(0), "aap");
  }

  std::vector<size_t> samples;
  samples.push_back(3);
  samples.push_back(9);
  samples.push_back(2);
  space.addDimension(Dimension(Samples, samples));

  {
    // One samples dimension added.
    address = space.address();
    BOOST_CHECK_EQUAL(address.size(), size_t(2));
    BOOST_CHECK(!space.isValid(address));
    BOOST_CHECK(!address.isValid(0));
    BOOST_CHECK(!address.isValid(1));

    address = space.initialiseInvalidCoordinates(address);
    BOOST_CHECK_EQUAL(address.size(), size_t(2));
    BOOST_CHECK(space.isValid(address));
    BOOST_CHECK(address.isValid(0));
    BOOST_CHECK(address.isValid(1));
    BOOST_CHECK_EQUAL(address.coordinate<std::string>(0), "aap");
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(1), size_t(3));
  }

  std::vector<size_t> timeSteps;
  timeSteps.push_back(1);
  timeSteps.push_back(100);
  timeSteps.push_back(1);
  space.addDimension(Dimension(Time, timeSteps));

  {
    // One time dimension added.
    address = space.address();
    BOOST_CHECK_EQUAL(address.size(), size_t(3));
    BOOST_CHECK(!space.isValid(address));
    BOOST_CHECK(!address.isValid(0));
    BOOST_CHECK(!address.isValid(1));
    BOOST_CHECK(!address.isValid(2));

    address = space.initialiseInvalidCoordinates(address);
    BOOST_CHECK_EQUAL(address.size(), size_t(3));
    BOOST_CHECK(space.isValid(address));
    BOOST_CHECK(address.isValid(0));
    BOOST_CHECK(address.isValid(1));
    BOOST_CHECK(address.isValid(2));
    BOOST_CHECK_EQUAL(address.coordinate<std::string>(0), "aap");
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(1), size_t(3));
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(2), size_t(1));
  }

  space.eraseDimension(Samples);

  std::vector<float> quantiles;
  quantiles.push_back(0.01f);
  quantiles.push_back(0.99f);
  quantiles.push_back(0.01f);
  space.addDimension(Dimension(CumulativeProbabilities, quantiles));

  {
    // One cumulative probability dimension added.
    address = space.address();
    BOOST_CHECK_EQUAL(address.size(), size_t(3));
    BOOST_CHECK(!space.isValid(address));
    BOOST_CHECK(!address.isValid(0));
    BOOST_CHECK(!address.isValid(1));
    BOOST_CHECK(!address.isValid(2));

    address = space.initialiseInvalidCoordinates(address);
    BOOST_CHECK_EQUAL(address.size(), size_t(3));
    BOOST_CHECK(space.isValid(address));
    BOOST_CHECK(address.isValid(0));
    BOOST_CHECK(address.isValid(1));
    BOOST_CHECK(address.isValid(2));
    BOOST_CHECK_EQUAL(address.coordinate<std::string>(0), "aap");
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(1), size_t(1));
    BOOST_CHECK_CLOSE(address.coordinate<float>(2), float(0.50), 0.001f);
  }

  RasterDimensions rasterDimensions(96, 95, 3.0, 5.5, 6.6);
  space.addDimension(Dimension(Space, RegularDiscretisation, rasterDimensions));

  {
    // One space dimension added.
    address = space.address();
    BOOST_CHECK_EQUAL(address.size(), size_t(4));
    BOOST_CHECK(!space.isValid(address));
    BOOST_CHECK(!address.isValid(0));
    BOOST_CHECK(!address.isValid(1));
    BOOST_CHECK(!address.isValid(2));
    BOOST_CHECK(!address.isValid(3));

    address = space.initialiseInvalidCoordinates(address);
    BOOST_CHECK_EQUAL(address.size(), size_t(4));
    BOOST_CHECK(space.isValid(address));
    BOOST_CHECK(address.isValid(0));
    BOOST_CHECK(address.isValid(1));
    BOOST_CHECK(address.isValid(2));
    BOOST_CHECK(address.isValid(3));
    BOOST_CHECK_EQUAL(address.coordinate<std::string>(0), "aap");
    BOOST_CHECK_EQUAL(address.coordinate<size_t>(1), size_t(1));
    BOOST_CHECK_CLOSE(address.coordinate<float>(2), float(0.50), 0.001f);
    BOOST_CHECK(address.coordinate<SpatialCoordinate>(3) ==
         SpatialCoordinate(5.5, 6.6));
  }

  {
    DataSpace space;
    DataSpaceAddress address;
    std::vector<float> quantiles;
    quantiles.push_back(0.90f);
    quantiles.push_back(0.95f);
    quantiles.push_back(0.99f);
    space.addDimension(Dimension(CumulativeProbabilities, quantiles));

    address = space.address();
    address = space.initialiseInvalidCoordinates(address);
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    BOOST_CHECK(space.isValid(address));
    BOOST_CHECK(address.isValid(0));
    BOOST_CHECK_EQUAL(address.coordinate<float>(0), float(0.90));
  }

  {
    DataSpace space;
    DataSpaceAddress address;
    std::vector<float> quantiles;
    quantiles.push_back(0.01f);
    quantiles.push_back(0.99f);
    quantiles.push_back(0.01f);
    space.addDimension(Dimension(CumulativeProbabilities, quantiles));

    address = space.address();
    address = space.initialiseInvalidCoordinates(address);
    BOOST_CHECK_EQUAL(address.size(), size_t(1));
    BOOST_CHECK(space.isValid(address));
    BOOST_CHECK(address.isValid(0));
    BOOST_CHECK_CLOSE(address.coordinate<float>(0), float(0.5), 0.001f);
  }
}


BOOST_AUTO_TEST_CASE(equals)
{
  using namespace dal;

  DataSpace space1, space2;

  RasterDimensions rasterDimensions(96, 95, 3.0, 5.5, 6.6);

  space1.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

  BOOST_CHECK(space1 == space1);
  BOOST_CHECK(space2 == space2);
  BOOST_CHECK(space1 != space2);

  space2 = space1;

  BOOST_CHECK(space1 == space2);
}


BOOST_AUTO_TEST_CASE(replace_dimension)
{
  using namespace dal;

  DataSpace space;

  std::vector<size_t> timeSteps;
  timeSteps.push_back(1);
  timeSteps.push_back(100);
  timeSteps.push_back(1);
  space.addDimension(Dimension(Time, timeSteps));
  BOOST_CHECK_EQUAL(space.dimension(0).meaning(), Time);

  RasterDimensions rasterDimensions(96, 95, 3.0, 5.5, 6.6);
  space.replaceDimension(0,
         Dimension(Space, RegularDiscretisation, rasterDimensions));

  BOOST_CHECK_EQUAL(space.dimension(0).meaning(), Space);
}


BOOST_AUTO_TEST_CASE(contains)
{
  using namespace dal;

  {
    std::vector<size_t> timeSteps;
    timeSteps.push_back( 9);
    timeSteps.push_back(21);
    timeSteps.push_back( 1);

    RasterDimensions rasterDimensions(3, 2, 3.0);

    DataSpace space;
    space.addDimension(Dimension(Time, timeSteps));
    space.addDimension(Dimension(Space, RegularDiscretisation,
         rasterDimensions));

    DataSpaceAddress address(space.address());
    BOOST_CHECK_EQUAL(address.size(), size_t(2));
    BOOST_CHECK(!space.contains(address));

    address.setCoordinate<size_t>(0, 9);
    BOOST_CHECK(!space.contains(address));

    // North west corner.
    address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.0, 0.0));
    BOOST_CHECK(space.contains(address));

    // South east corner.
    address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(6.0, -9.0));
    BOOST_CHECK(space.contains(address));

    // Outside of area.
    address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(-1.0, -1.0));
    BOOST_CHECK(!space.contains(address));

    address.setCoordinate<SpatialCoordinate>(1, SpatialCoordinate(0.0, 0.0));
    BOOST_CHECK(space.contains(address));

    address.setCoordinate<size_t>(0, 8);
    BOOST_CHECK(!space.contains(address));

    address.setCoordinate<size_t>(0, 11);
    BOOST_CHECK(space.contains(address));
  }

  {
    DataSpace space;
    std::vector<size_t> timeSteps, rows, cols;
    timeSteps.push_back( 9);
    timeSteps.push_back(21);
    timeSteps.push_back( 2);
    space.addDimension(Dimension(Time, timeSteps));

    DataSpaceAddress address(space.address());
    BOOST_CHECK(!space.contains(address));

    address.setCoordinate<size_t>(0, 9);
    BOOST_CHECK(space.contains(address));

    address.setCoordinate<size_t>(0, 10);
    BOOST_CHECK(!space.contains(address));

    address.setCoordinate<size_t>(0, 11);
    BOOST_CHECK(space.contains(address));
  }

  {
    DataSpace space;
    std::vector<float> quantiles;
    quantiles.push_back(float(0.1));
    quantiles.push_back(float(0.9));
    quantiles.push_back(float(0.01));
    space.addDimension(Dimension(CumulativeProbabilities, quantiles));

    DataSpaceAddress address(space.address());
    BOOST_CHECK(!space.contains(address));

    address.setCoordinate<float>(0, 0.1f);
    BOOST_CHECK(space.contains(address));
  }
}
