#define BOOST_TEST_MODULE pcraster dal raster_dal
#include <boost/test/unit_test.hpp>
#include "dal_Raster.h"
#include "dal_RasterDal.h"
#include "dev_GDalClient.h"
#include "dal_Client.h"


class ClientWrapper : public dal::Client {
public:
  ClientWrapper(boost::filesystem::path const& prefix,
                   bool addAllDrivers=false,
                   bool cacheDatasetInfo=true)
  : dal::Client(prefix) {
  }
};


class GDalClientWrapper : public dev::GDalClient {
public:
  GDalClientWrapper() : dev::GDalClient() {
  }
};


struct Fixture
{

    Fixture()
    {
        static GDalClientWrapper gdal_client;
        static ClientWrapper client("/my/path/raster_dal_test", true);
    }

    ~Fixture()
    {
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(supported_drivers)
{
  using namespace dal;

  RasterDal dal(true);

  BOOST_CHECK(!dal.driverByName("NoSuchDriver"));

  // Add the drivers needed by client code.
  BOOST_CHECK(dal.driverByName("CSF"));
  BOOST_CHECK(!dal.driverByName("PCRaster"));

// #ifdef WIN32
//   BOOST_WARN(dal.driverByName("HDF4Image"));
//   BOOST_WARN(dal.driverByName("HDF4"));
// #else
//   BOOST_CHECK(dal.driverByName("HDF4Image"));
//   BOOST_CHECK(dal.driverByName("HDF4"));
// #endif
}


BOOST_AUTO_TEST_CASE(esri_ascii_grid1)
{
  using namespace dal;

  std::string filename = "esriasciigrid1.asc";
  RasterDal dal(true);
  boost::shared_ptr<Raster> raster;

  {
    boost::tie(raster, boost::tuples::ignore) = dal.open(filename);
    BOOST_REQUIRE(raster);
    BOOST_CHECK_EQUAL(raster->nrRows(), size_t(4));
    BOOST_CHECK_EQUAL(raster->nrCols(), size_t(3));
    BOOST_CHECK_EQUAL(raster->cellSize(), 10.0);
    BOOST_CHECK_EQUAL(raster->west(), 3.0);
    BOOST_CHECK_EQUAL(raster->south(), 4.0);
    BOOST_CHECK_EQUAL(raster->typeId(), TI_INT4);
  }

  {
    raster = dal.read(filename, TI_INT4);
    BOOST_REQUIRE(raster);
    INT4 const* cells = static_cast<INT4 const*>(raster->cells());
    BOOST_REQUIRE(cells);

    BOOST_CHECK_EQUAL(cells[0], 1);
    BOOST_CHECK_EQUAL(cells[1], 2);
    BOOST_CHECK_EQUAL(cells[2], 3);
    BOOST_CHECK_EQUAL(cells[3], 4);
    BOOST_CHECK(pcr::isMV(cells[4]));
    BOOST_CHECK_EQUAL(cells[5], 6);
    BOOST_CHECK_EQUAL(cells[6], 7);
    BOOST_CHECK_EQUAL(cells[7], 8);
    BOOST_CHECK_EQUAL(cells[8], 9);
    BOOST_CHECK_EQUAL(cells[9], 10);
    BOOST_CHECK_EQUAL(cells[10], 11);
    BOOST_CHECK_EQUAL(cells[11], 12);
  }
}


BOOST_AUTO_TEST_CASE(hdf4_image1)
{
  using namespace dal;

  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}
