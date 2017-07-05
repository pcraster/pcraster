#define BOOST_TEST_MODULE pcraster dal vector_driver
#include <boost/test/unit_test.hpp>
#include "dal_Exception.h"
#include "dal_VectorDriver.h"
#include "dal_Client.h"

class ClientWrapper : public dal::Client {
public:
  ClientWrapper(boost::filesystem::path const& prefix,
                   bool addAllDrivers=false,
                   bool cacheDatasetInfo=true)
  : dal::Client(prefix) {
  }
};


struct Fixture
{

    Fixture()
    {
        static ClientWrapper client("/my/path/vector_driver_test", true);
    }

    ~Fixture()
    {
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(exists)
{
  using namespace dal;

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


BOOST_AUTO_TEST_CASE(open)
{
  using namespace dal;

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


BOOST_AUTO_TEST_CASE(data_space)
{
  using namespace dal;

  VectorDriver driver;
  DataSpace space;

  // Static data.
  {
    BOOST_REQUIRE_NO_THROW(
      space = dynamic_cast<Driver&>(driver).dataSpace("vector");
    );

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
    );

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


BOOST_AUTO_TEST_CASE(read_)
{
  using namespace dal;

  VectorDriver driver;
  boost::shared_ptr<Vector> vector;

  // Test direct read.
  {
    BOOST_REQUIRE_NO_THROW(
      vector.reset(dynamic_cast<Vector*>(
           dynamic_cast<Driver&>(driver).read("vector")));
    );

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
    );

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


BOOST_AUTO_TEST_CASE(extremes)
{
  using namespace dal;

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
