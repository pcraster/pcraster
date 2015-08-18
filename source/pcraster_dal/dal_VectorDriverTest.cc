#ifndef INCLUDED_DAL_VECTORDRIVERTEST
#include "dal_VectorDriverTest.h"
#define INCLUDED_DAL_VECTORDRIVERTEST
#endif

// External headers.
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

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_VECTORDRIVER
#include "dal_VectorDriver.h"
#define INCLUDED_DAL_VECTORDRIVER
#endif



/*!
  \file
  This file contains the implementation of the VectorDriverTest class.
*/

namespace {

} // Anonymous namespace



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VECTORDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* VectorDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<VectorDriverTest> instance(
         new VectorDriverTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &VectorDriverTest::testExists, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &VectorDriverTest::testOpen, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &VectorDriverTest::testDataSpace, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &VectorDriverTest::testRead, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &VectorDriverTest::testExtremes, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF VECTORDRIVERTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
VectorDriverTest::VectorDriverTest()
{
}



void VectorDriverTest::testExists()
{
  VectorDriver driver;

  // Static data.
  {
    BOOST_CHECK( dynamic_cast<Driver const&>(driver).exists("vector"));
    BOOST_CHECK( dynamic_cast<Driver const&>(driver).exists("vector.map"));

    BOOST_CHECK(!dynamic_cast<Driver const&>(driver).exists("vector_x"));
    BOOST_CHECK(!dynamic_cast<Driver const&>(driver).exists("vector_y"));
    BOOST_CHECK(!dynamic_cast<Driver const&>(driver).exists("vector_x.map"));
    BOOST_CHECK(!dynamic_cast<Driver const&>(driver).exists("vector_y.map"));
    BOOST_CHECK(!dynamic_cast<Driver const&>(driver).exists("DoesNotExist"));
  }

  // Temporal data.
  {
    DataSpace space;
    space.addDimension(Dimension(Time, size_t(1), size_t(5), size_t(1)));
    DataSpaceAddress address(space.address());

    address.setCoordinate<size_t>(0, 1);
    BOOST_CHECK( dynamic_cast<Driver const&>(driver).exists("vector", space,
              address));

    address.setCoordinate<size_t>(0, 2);
    BOOST_CHECK( dynamic_cast<Driver const&>(driver).exists("vector", space,
              address));

    address.setCoordinate<size_t>(0, 3);
    BOOST_CHECK( dynamic_cast<Driver const&>(driver).exists("vector", space,
              address));
    BOOST_CHECK( dynamic_cast<Driver const&>(driver).exists("vector.map", space,
              address));

    address.setCoordinate<size_t>(0, 4);
    BOOST_CHECK(!dynamic_cast<Driver const&>(driver).exists("vector", space,
              address));
    BOOST_CHECK(!dynamic_cast<Driver const&>(driver).exists("vector.map", space,
              address));

  }
}



void VectorDriverTest::testOpen()
{
  VectorDriver driver;
  boost::shared_ptr<Vector> vector;

  // Static data.
  {
    vector.reset(dynamic_cast<Vector*>(
         dynamic_cast<Driver&>(driver).open("vector")));
    BOOST_REQUIRE(vector);

    BOOST_CHECK_EQUAL(vector->nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(vector->nrCols(), size_t(2));
    BOOST_CHECK_EQUAL(vector->cellSize(), 50.0);
    BOOST_CHECK_EQUAL(vector->west(), 100000.0);
    BOOST_CHECK_EQUAL(vector->south(), 199850.0);
    BOOST_CHECK_EQUAL(vector->typeId(), TI_REAL4);
    BOOST_CHECK(!vector->hasExtremes());

    vector.reset();
  }

  // Temporal data.
  {
    DataSpace space;
    space.addDimension(Dimension(Time, size_t(1), size_t(5), size_t(1)));
    DataSpaceAddress address(space.address());

    address.setCoordinate<size_t>(0, 2);
    vector.reset(dynamic_cast<Vector*>(
         dynamic_cast<Driver&>(driver).open("vector", space, address)));
    BOOST_REQUIRE(vector);

    BOOST_CHECK_EQUAL(vector->nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(vector->nrCols(), size_t(2));
    BOOST_CHECK_EQUAL(vector->cellSize(), 50.0);
    BOOST_CHECK_EQUAL(vector->west(), 100000.0);
    BOOST_CHECK_EQUAL(vector->south(), 199850.0);
    BOOST_CHECK_EQUAL(vector->typeId(), TI_REAL4);
    BOOST_CHECK(!vector->hasExtremes());

    vector.reset();
  }
}



void VectorDriverTest::testDataSpace()
{
  VectorDriver driver;
  DataSpace space;

  // Static data.
  {
    BOOST_REQUIRE_NO_THROW(
      space = dynamic_cast<Driver&>(driver).dataSpace("vector");
    )

    BOOST_CHECK_EQUAL(space.size(), size_t(1));

    Dimension dimension(space.dimension(0));
    BOOST_CHECK_EQUAL(dimension.meaning(), Space);
    BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(1));
    BOOST_CHECK_EQUAL(dimension.coordinateType(), NumericalCoordinates);
    BOOST_CHECK_EQUAL(dimension.discretisation(), RegularDiscretisation);

    RasterDimensions const& rasterDimensions(
         space.dimension(0).value<RasterDimensions>(0));

    BOOST_CHECK_EQUAL(rasterDimensions.nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(rasterDimensions.nrCols(), size_t(2));
    BOOST_CHECK_CLOSE(rasterDimensions.cellSize(), 50.0, 0.001);
    BOOST_CHECK_CLOSE(rasterDimensions.west(), 100000.0, 0.001);
    BOOST_CHECK_CLOSE(rasterDimensions.south(), 199850.0, 0.001);
  }

  // Temporal data.
  {
    DataSpace space;
    space.addDimension(Dimension(Time, size_t(1), size_t(5), size_t(1)));
    DataSpaceAddress address(space.address());

    address.setCoordinate<size_t>(0, 2);

    BOOST_REQUIRE_NO_THROW(
      space = dynamic_cast<Driver&>(driver).dataSpace("vector", space, address);
    )

    BOOST_CHECK_EQUAL(space.size(), size_t(1));

    Dimension dimension(space.dimension(0));
    BOOST_CHECK_EQUAL(dimension.meaning(), Space);
    BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(1));
    BOOST_CHECK_EQUAL(dimension.coordinateType(), NumericalCoordinates);
    BOOST_CHECK_EQUAL(dimension.discretisation(), RegularDiscretisation);

    RasterDimensions const& rasterDimensions(
         space.dimension(0).value<RasterDimensions>(0));

    BOOST_CHECK_EQUAL(rasterDimensions.nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(rasterDimensions.nrCols(), size_t(2));
    BOOST_CHECK_CLOSE(rasterDimensions.cellSize(), 50.0, 0.001);
    BOOST_CHECK_CLOSE(rasterDimensions.west(), 100000.0, 0.001);
    BOOST_CHECK_CLOSE(rasterDimensions.south(), 199850.0, 0.001);
  }
}



void VectorDriverTest::testRead()
{
  VectorDriver driver;
  boost::shared_ptr<Vector> vector;

  // Test direct read.
  {
    BOOST_REQUIRE_NO_THROW(
      vector.reset(dynamic_cast<Vector*>(
           dynamic_cast<Driver&>(driver).read("vector")));
    )

    BOOST_REQUIRE(vector);

    BOOST_CHECK_EQUAL(vector->nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(vector->nrCols(), size_t(2));
    BOOST_CHECK_EQUAL(vector->cellSize(), 50.0);
    BOOST_CHECK_EQUAL(vector->west(), 100000.0);
    BOOST_CHECK_EQUAL(vector->south(), 199850.0);
    BOOST_CHECK_EQUAL(vector->typeId(), TI_REAL4);

    BOOST_CHECK_CLOSE(vector->x<REAL4>(0, 0), REAL4( 1.1), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->x<REAL4>(0, 1), REAL4( 0.0), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->x<REAL4>(1, 0), REAL4( 1.1), REAL4(0.001));
    BOOST_CHECK(pcr::isMV(vector->x<REAL4>(1, 1)));
    BOOST_CHECK_CLOSE(vector->x<REAL4>(2, 0), REAL4( 1.1), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->x<REAL4>(2, 1), REAL4(-1.1), REAL4(0.001));

    BOOST_CHECK_CLOSE(vector->y<REAL4>(0, 0), REAL4( 0.0), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->y<REAL4>(0, 1), REAL4(-2.2), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->y<REAL4>(1, 0), REAL4(-2.2), REAL4(0.001));
    BOOST_CHECK(pcr::isMV(vector->y<REAL4>(1, 1)));
    BOOST_CHECK_CLOSE(vector->y<REAL4>(2, 0), REAL4( 0.0), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->y<REAL4>(2, 1), REAL4(-2.2), REAL4(0.001));

    vector.reset();
  }

  // Test read after open.
  {
    vector.reset(dynamic_cast<Vector*>(
         dynamic_cast<Driver&>(driver).open("vector")));
    BOOST_REQUIRE(vector);

    BOOST_REQUIRE_NO_THROW(
      driver.read(*vector, "vector", DataSpace(), DataSpaceAddress());
    )

    BOOST_CHECK_EQUAL(vector->nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(vector->nrCols(), size_t(2));
    BOOST_CHECK_EQUAL(vector->cellSize(), 50.0);
    BOOST_CHECK_EQUAL(vector->west(), 100000.0);
    BOOST_CHECK_EQUAL(vector->south(), 199850.0);
    BOOST_CHECK_EQUAL(vector->typeId(), TI_REAL4);

    BOOST_CHECK_CLOSE(vector->x<REAL4>(0, 0), REAL4( 1.1), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->x<REAL4>(0, 1), REAL4( 0.0), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->x<REAL4>(1, 0), REAL4( 1.1), REAL4(0.001));
    BOOST_CHECK(pcr::isMV(vector->x<REAL4>(1, 1)));
    BOOST_CHECK_CLOSE(vector->x<REAL4>(2, 0), REAL4( 1.1), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->x<REAL4>(2, 1), REAL4(-1.1), REAL4(0.001));

    BOOST_CHECK_CLOSE(vector->y<REAL4>(0, 0), REAL4( 0.0), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->y<REAL4>(0, 1), REAL4(-2.2), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->y<REAL4>(1, 0), REAL4(-2.2), REAL4(0.001));
    BOOST_CHECK(pcr::isMV(vector->y<REAL4>(1, 1)));
    BOOST_CHECK_CLOSE(vector->y<REAL4>(2, 0), REAL4( 0.0), REAL4(0.001));
    BOOST_CHECK_CLOSE(vector->y<REAL4>(2, 1), REAL4(-2.2), REAL4(0.001));

    vector.reset();
  }

  // Test error message for unexisting data.
  {
    bool exceptionCaught;

    try {
      exceptionCaught = false;
      dynamic_cast<Driver const&>(driver).read("DoesNotExist");
    }
    catch(Exception& exception) {
      BOOST_CHECK_EQUAL(exception.message(),
          "Data source DoesNotExist(vector):\ncannot be opened");
      exceptionCaught = true;
    }

    BOOST_CHECK(exceptionCaught);
  }
}



void VectorDriverTest::testExtremes()
{
  VectorDriver driver;
  boost::shared_ptr<Vector> vector;
  boost::any min, max;

  {
    BOOST_REQUIRE(driver.extremes(min, max, TI_REAL4, "vector"));
    BOOST_CHECK_CLOSE(boost::any_cast<REAL4>(min), REAL4(1.1), REAL4(0.001));
    BOOST_CHECK_CLOSE(boost::any_cast<REAL4>(max), REAL4(2.459674784),
         REAL4(0.001));
  }
}

} // namespace dal

