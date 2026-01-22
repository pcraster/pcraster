#define BOOST_TEST_MODULE pcraster dal raster_dal
#include <boost/test/unit_test.hpp>
#include "dal_Raster.h"
#include "dal_RasterDal.h"
#include "dev_GDalClient.h"
#include "dal_Client.h"


class ClientWrapper : public dal::Client {
public:
  ClientWrapper(std::filesystem::path const& prefix,
                   bool  /*addAllDrivers*/=false,
                   bool  /*cacheDatasetInfo*/=true)
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
        static GDalClientWrapper const gdal_client;
        static ClientWrapper const client("/my/path/raster_dal_test", true);
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

  BOOST_TEST(!dal.driverByName("NoSuchDriver"));

  // Add the drivers needed by client code.
  BOOST_TEST(dal.driverByName("CSF"));
  BOOST_TEST(!dal.driverByName("PCRaster"));

// #ifdef WIN32
//   BOOST_TEST_WARN(dal.driverByName("HDF4Image"));
//   BOOST_TEST_WARN(dal.driverByName("HDF4"));
// #else
//   BOOST_TEST(dal.driverByName("HDF4Image"));
//   BOOST_TEST(dal.driverByName("HDF4"));
// #endif
}


BOOST_AUTO_TEST_CASE(esri_ascii_grid1)
{
  using namespace dal;

  std::string const filename = "esriasciigrid1.asc";
  RasterDal const dal(true);
  std::shared_ptr<Raster> raster;

  {
    std::tie(raster, std::ignore) = dal.open(filename);
    BOOST_TEST_REQUIRE(raster);
    BOOST_TEST(raster->nrRows() == size_t(4));
    BOOST_TEST(raster->nrCols() == size_t(3));
    BOOST_TEST(raster->cellSize() == 10.0);
    BOOST_TEST(raster->west() == 3.0);
    BOOST_TEST(raster->south() == 4.0);
    BOOST_TEST(raster->typeId() == TI_INT4);
  }

  {
    raster = dal.read(filename, TI_INT4);
    BOOST_TEST_REQUIRE(raster);
    INT4 const* cells = static_cast<INT4 const*>(raster->cells());
    BOOST_TEST_REQUIRE(cells);

    BOOST_TEST(cells[0] == 1);
    BOOST_TEST(cells[1] == 2);
    BOOST_TEST(cells[2] == 3);
    BOOST_TEST(cells[3] == 4);
    BOOST_TEST(pcr::isMV(cells[4]));
    BOOST_TEST(cells[5] == 6);
    BOOST_TEST(cells[6] == 7);
    BOOST_TEST(cells[7] == 8);
    BOOST_TEST(cells[8] == 9);
    BOOST_TEST(cells[9] == 10);
    BOOST_TEST(cells[10] == 11);
    BOOST_TEST(cells[11] == 12);
  }
}


BOOST_AUTO_TEST_CASE(hdf4_image1)
{
  using namespace dal;

  bool const testImplemented = false;
  BOOST_TEST_WARN(testImplemented);
}
