#define BOOST_TEST_MODULE pcraster dal memory_raster_driver
#include <boost/test/unit_test.hpp>
#include <boost/scoped_array.hpp>
#include "dal_Def.h"
#include "dal_Exception.h"
#include "dal_Library.h"
#include "dal_MemoryRasterData.h"
#include "dal_MemoryRasterDriver.h"
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
        static ClientWrapper client("/my/path/memory_raster_driver_test", true);
    }

    ~Fixture()
    {
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(empty_data_space)
{
  using namespace dal;

  boost::scoped_array<REAL4> cells(new REAL4[6]);
  cells[0] =  1.0;
  cells[1] =  3.0;
  cells[2] =  5.0;
  cells[3] =  7.0;
  cells[4] =  9.0;
  cells[5] = 11.0;

  std::vector<boost::any> values;
  values.push_back(cells.get());

  DataSpace space;
  TypeId typeId = TI_REAL4;
  size_t nrRows = 3;
  size_t nrCols = 2;
  double cellSize = 15.0;
  double north = 0.0;
  double west = 0.0;

  MemoryRasterData data(values, space, typeId, nrRows, nrCols, cellSize,
       west, north);

  MemoryRasterDriver driver(&(library()->memoryDataPool()));

  boost::shared_ptr<Raster> raster;
  BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists("data1"));
  raster.reset(dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open("data1")));
  BOOST_CHECK(!raster.get());
  library()->memoryDataPool().add("data1", data);
  BOOST_CHECK(dynamic_cast<Driver&>(driver).exists("data1"));
  raster.reset(dynamic_cast<RasterDriver&>(driver).read("data1"));
  BOOST_CHECK(raster.get());
  BOOST_CHECK_EQUAL(raster->cellSize(), cellSize);
  BOOST_CHECK_EQUAL(raster->nrRows(), nrRows);
  BOOST_CHECK_EQUAL(raster->nrCols(), nrCols);
  BOOST_CHECK(comparable<double>(raster->north(), north));
  BOOST_CHECK(comparable<double>(raster->west(), west));
  BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(0),  1.0));
  BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(3),  7.0));
  BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(5), 11.0));
  library()->memoryDataPool().remove("data1", space);
}


BOOST_AUTO_TEST_CASE(same_name)
{
  using namespace dal;

  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  // Doesn't work anymore after getting rid for support of ExactDiscretisation
  // for quantiles.
  BOOST_WARN(false);
  return;

  // Fill global memory pool with rasters.
  boost::scoped_array<REAL4> q1(new REAL4[6]);
  boost::scoped_array<REAL4> q5(new REAL4[6]);
  boost::scoped_array<REAL4> q9(new REAL4[6]);
  q1[0] =  1.0; q5[0] =  2.0; q9[0] =  3.0;
  q1[1] =  3.0; q5[1] =  4.0; q9[1] =  5.0;
  q1[2] =  5.0; q5[2] =  6.0; q9[2] =  7.0;
  q1[3] =  7.0; q5[3] =  8.0; q9[3] =  9.0;
  q1[4] =  9.0; q5[4] = 10.0; q9[4] = 11.0;
  q1[5] = 11.0; q5[5] = 12.0; q9[5] = 13.0;

  std::vector<boost::any> values;
  DataSpace space;
  TypeId typeId = TI_REAL4;
  size_t nrRows = 3;
  size_t nrCols = 2;
  double cellSize = 15.0;
  double north = 0.0;
  double west = 0.0;

  std::vector<float> quantiles;
  quantiles.push_back(0.1f);
  quantiles.push_back(0.5f);
  quantiles.push_back(0.9f);
  space.addDimension(Dimension(CumulativeProbabilities, quantiles));

  typedef boost::tuple<float, std::vector<boost::any> > FloatTuple;

  std::vector<boost::any> tmp;
  tmp.push_back(q1.get());
  values.push_back(FloatTuple(0.1f, tmp));
  tmp.clear();
  tmp.push_back(q5.get());
  values.push_back(FloatTuple(0.5f, tmp));
  tmp.clear();
  tmp.push_back(q9.get());
  values.push_back(FloatTuple(0.9f, tmp));

  MemoryRasterData data(values, space, typeId, nrRows, nrCols, cellSize,
       west, north);

  MemoryRasterDriver driver(&(library()->memoryDataPool()));

  // Before adding the data to the pool it cannot be found by the driver.
  DataSpaceAddress address(space.address());
  address.setCoordinate<float>(0, 0.5f);
  BOOST_CHECK(!driver.exists("data1", space, address));

  library()->memoryDataPool().add("data1", data);

  // Query the memory pool in various ways by use of the driver.
  boost::shared_ptr<Raster> raster;
  raster.reset(dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open("data1")));
  BOOST_CHECK(!raster.get());

  BOOST_CHECK(driver.exists("data1", space, address));
  raster.reset(dynamic_cast<RasterDriver&>(driver).read("data1", space, address));
  BOOST_CHECK(raster.get());
  BOOST_CHECK_EQUAL(raster->cellSize(), cellSize);
  BOOST_CHECK_EQUAL(raster->nrRows(), nrRows);
  BOOST_CHECK_EQUAL(raster->nrCols(), nrCols);
  BOOST_CHECK(comparable<double>(raster->north(), north));
  BOOST_CHECK(comparable<double>(raster->west(), west));
  BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(0),  2.0));
  BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(3),  8.0));
  BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(5), 12.0));

  // Delete the raster, values in memory should still be available.
  raster.reset();

  BOOST_CHECK(driver.exists("data1", space, address));
  raster.reset(dynamic_cast<RasterDriver&>(driver).read("data1", space, address));
  BOOST_CHECK(raster.get());
  BOOST_CHECK_EQUAL(raster->nrRows(), nrRows);
  BOOST_CHECK(comparable<double>(raster->west(), west));
  BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(5), 12.0));

  library()->memoryDataPool().remove("data1", space);
}
