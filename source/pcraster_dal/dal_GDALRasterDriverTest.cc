#define BOOST_TEST_MODULE pcraster dal gdal_raster_driver
#include <boost/test/unit_test.hpp>
#include <gdal_priv.h>
#ifdef _MSC_VER
  #ifndef INCLUDED_ENVIRON
  #include "environ.h" // M_PI
  #define INCLUDED_ENVIRON
  #endif
#endif
#include "dev_GDalClient.h"
#include "dal_Exception.h"
#include "dal_GDALRasterDriver.h"
#include "dal_Utils.h"
#include "dal_Client.h"


class ClientWrapper : public dal::Client {
public:
  ClientWrapper(boost::filesystem::path const& prefix,
                   bool addAllDrivers=false,
                   bool cacheDatasetInfo=true)
  : dal::Client(prefix) {
  }
};


namespace detail {

void testUInt1Raster1(
         dal::RasterDriver& driver,
         std::string const& name)
{
  boost::shared_ptr<dal::Raster> raster;

  // Open.
  raster.reset(driver.open(name));
  BOOST_REQUIRE(raster);
  BOOST_CHECK_EQUAL(raster->nrRows(), size_t(3));
  BOOST_CHECK_EQUAL(raster->nrCols(), size_t(3));
  BOOST_CHECK_EQUAL(raster->cellSize(), 1.0);
  BOOST_CHECK_EQUAL(raster->west(), 0.0);
  BOOST_CHECK_EQUAL(raster->south(), -3.0);
  BOOST_CHECK_EQUAL(raster->typeId(), dal::TI_UINT1);

  // Read.
  raster.reset(driver.read(name));
  BOOST_REQUIRE(raster);
  UINT1 const* cells = static_cast<UINT1 const*>(raster->cells());
  BOOST_REQUIRE(cells);
  BOOST_CHECK_EQUAL(cells[0], 1);
  BOOST_CHECK_EQUAL(cells[1], 0);
  BOOST_CHECK_EQUAL(cells[2], 1);
  BOOST_CHECK(pcr::isMV(cells[3]));
  BOOST_CHECK_EQUAL(cells[4], 1);
  BOOST_CHECK_EQUAL(cells[5], 1);
  BOOST_CHECK_EQUAL(cells[6], 1);
  BOOST_CHECK_EQUAL(cells[7], 1);
  BOOST_CHECK_EQUAL(cells[8], 0);
}


void testInt4Raster1(
         dal::RasterDriver& driver,
         std::string const& name)
{
  boost::shared_ptr<dal::Raster> raster;

  // Open.
  raster.reset(driver.open(name));
  BOOST_REQUIRE(raster);
  BOOST_CHECK_EQUAL(raster->nrRows(), size_t(5));
  BOOST_CHECK_EQUAL(raster->nrCols(), size_t(5));
  BOOST_CHECK_EQUAL(raster->cellSize(), 2.0);
  BOOST_CHECK_EQUAL(raster->west(), 0.0);
  BOOST_CHECK_EQUAL(raster->south(), -10.0);
  BOOST_CHECK_EQUAL(raster->typeId(), dal::TI_INT4);

  // Read.
  raster.reset(driver.read(name));
  BOOST_REQUIRE(raster);
  INT4 const* cells = static_cast<INT4 const*>(raster->cells());
  BOOST_REQUIRE(cells);
  BOOST_CHECK_EQUAL(cells[0], 2);
  BOOST_CHECK_EQUAL(cells[1], 6);
  BOOST_CHECK_EQUAL(cells[2], 2);
  BOOST_CHECK_EQUAL(cells[3], 2);
  BOOST_CHECK(pcr::isMV(cells[4]));
  BOOST_CHECK_EQUAL(cells[5], 6);
  BOOST_CHECK_EQUAL(cells[6], 6);
  BOOST_CHECK_EQUAL(cells[7], 2);
  BOOST_CHECK_EQUAL(cells[8], 2);
  BOOST_CHECK_EQUAL(cells[9], 2);
  BOOST_CHECK_EQUAL(cells[10], 6);
  BOOST_CHECK_EQUAL(cells[11], 6);
  BOOST_CHECK_EQUAL(cells[12], 0);
  BOOST_CHECK_EQUAL(cells[13], 0);
  BOOST_CHECK_EQUAL(cells[14], 0);
  BOOST_CHECK_EQUAL(cells[15], 6);
  BOOST_CHECK_EQUAL(cells[16], 6);
  BOOST_CHECK_EQUAL(cells[17], 0);
  BOOST_CHECK_EQUAL(cells[18], 0);
  BOOST_CHECK_EQUAL(cells[19], 0);
  BOOST_CHECK_EQUAL(cells[20], 6);
  BOOST_CHECK_EQUAL(cells[21], 3);
  BOOST_CHECK_EQUAL(cells[22], 3);
  BOOST_CHECK_EQUAL(cells[23], 4);
  BOOST_CHECK_EQUAL(cells[24], 4);
}


void testInt4Raster2(
         dal::RasterDriver& driver,
         std::string const& name)
{
  boost::shared_ptr<dal::Raster> raster;

  // Open.
  raster.reset(driver.open(name));
  BOOST_REQUIRE(raster);
  BOOST_CHECK_EQUAL(raster->nrRows(), size_t(3));
  BOOST_CHECK_EQUAL(raster->nrCols(), size_t(3));
  BOOST_CHECK_EQUAL(raster->cellSize(), 4.0);
  BOOST_CHECK_EQUAL(raster->west(), 10.0);
  BOOST_CHECK_EQUAL(raster->south(), -2.0);
  BOOST_CHECK_EQUAL(raster->typeId(), dal::TI_INT4);

  // Read.
  raster.reset(driver.read(name));
  BOOST_REQUIRE(raster);
  INT4 const* cells = static_cast<INT4 const*>(raster->cells());
  BOOST_REQUIRE(cells);
  BOOST_CHECK_EQUAL(cells[0], 1);
  BOOST_CHECK_EQUAL(cells[1], 2);
  BOOST_CHECK_EQUAL(cells[2], 3);
  BOOST_CHECK_EQUAL(cells[3], 4);
  BOOST_CHECK_EQUAL(cells[4], 5);
  BOOST_CHECK_EQUAL(cells[5], 6);
  BOOST_CHECK_EQUAL(cells[6], 7);
  BOOST_CHECK(pcr::isMV(cells[7]));
  BOOST_CHECK_EQUAL(cells[8], 9);
}


void testReal4Raster1(
         dal::RasterDriver& driver,
         std::string const& name)
{
  boost::shared_ptr<dal::Raster> raster;

  // Open.
  raster.reset(driver.open(name));
  BOOST_REQUIRE(raster);
  BOOST_CHECK_EQUAL(raster->nrRows(), size_t(3));
  BOOST_CHECK_EQUAL(raster->nrCols(), size_t(3));
  BOOST_CHECK_EQUAL(raster->cellSize(), 1.0);
  BOOST_CHECK_EQUAL(raster->west(), 0.0);
  BOOST_CHECK_EQUAL(raster->south(), -3.0);
  BOOST_CHECK_EQUAL(raster->typeId(), dal::TI_REAL4);

  // Read.
  raster.reset(driver.read(name));
  BOOST_REQUIRE(raster);
  REAL4 const* cells = static_cast<REAL4 const*>(raster->cells());
  BOOST_REQUIRE(cells);
  BOOST_CHECK_EQUAL(cells[0], REAL4(2));
  BOOST_CHECK_EQUAL(cells[1], REAL4(-7));
  BOOST_CHECK_EQUAL(cells[2], REAL4(3.5));
  BOOST_CHECK_EQUAL(cells[3], REAL4(-8.5));
  BOOST_CHECK_EQUAL(cells[4], REAL4(3.6));
  BOOST_CHECK(pcr::isMV(cells[5]));
  BOOST_CHECK_EQUAL(cells[6], REAL4(0.0));
  BOOST_CHECK_EQUAL(cells[7], REAL4(14));
  BOOST_CHECK_EQUAL(cells[8], REAL4(-0.8));
}


void testReal4Raster2(
         dal::RasterDriver& driver,
         std::string const& name)
{
  boost::shared_ptr<dal::Raster> raster;

  // Open.
  raster.reset(driver.open(name));
  BOOST_REQUIRE(raster);
  BOOST_CHECK_EQUAL(raster->nrRows(), size_t(3));
  BOOST_CHECK_EQUAL(raster->nrCols(), size_t(3));
  BOOST_CHECK_EQUAL(raster->cellSize(), 1.0);
  BOOST_CHECK_EQUAL(raster->west(), 0.0);
  BOOST_CHECK_EQUAL(raster->south(), -3.0);
  BOOST_CHECK_EQUAL(raster->typeId(), dal::TI_REAL4);

  // Read.
  raster.reset(driver.read(name));
  BOOST_REQUIRE(raster);
  REAL4 const* cells = static_cast<REAL4 const*>(raster->cells());
  BOOST_REQUIRE(cells);

  BOOST_CHECK(dal::comparable(REAL4(cells[0] * 180.0 / M_PI), REAL4(280.0F)));
  BOOST_CHECK(dal::comparable(REAL4(cells[1] * 180.0 / M_PI), REAL4(25.0)));
  BOOST_CHECK(dal::comparable(REAL4(cells[2] * 180.0 / M_PI), REAL4(11.0)));
  BOOST_CHECK(dal::comparable(REAL4(cells[3] * 180.0 / M_PI), REAL4(68.0)));
// BOOST_CHECK(dal::comparable(REAL4(cells[4] * 180.0 / M_PI), REAL4(-1.0)));
  BOOST_CHECK(dal::comparable(REAL4(cells[5] * 180.0 / M_PI), REAL4(0.0)));
  BOOST_CHECK(pcr::isMV(cells[6]));
// BOOST_CHECK(dal::comparable(REAL4(cells[7] * 180.0 / M_PI), REAL4(-1.0)));
  BOOST_CHECK(dal::comparable(REAL4(cells[8] * 180.0 / M_PI), REAL4(7.0)));
}


void testUInt1Raster2(
         dal::RasterDriver& driver,
         std::string const& name)
{
  boost::shared_ptr<dal::Raster> raster;

  // Open.
  raster.reset(driver.open(name));
  BOOST_REQUIRE(raster);
  BOOST_CHECK_EQUAL(raster->nrRows(), size_t(5));
  BOOST_CHECK_EQUAL(raster->nrCols(), size_t(5));
  BOOST_CHECK_EQUAL(raster->cellSize(), 1.0);
  BOOST_CHECK_EQUAL(raster->west(), 0.0);
  BOOST_CHECK_EQUAL(raster->south(), -5.0);
  BOOST_CHECK_EQUAL(raster->typeId(), dal::TI_UINT1);

  // Read.
  raster.reset(driver.read(name));
  BOOST_REQUIRE(raster);
  UINT1 const* cells = static_cast<UINT1 const*>(raster->cells());
  BOOST_REQUIRE(cells);
  BOOST_CHECK_EQUAL(cells[0], 2);
  BOOST_CHECK_EQUAL(cells[1], 2);
  BOOST_CHECK_EQUAL(cells[2], 2);
  BOOST_CHECK_EQUAL(cells[3], 1);
  BOOST_CHECK_EQUAL(cells[4], 1);

  BOOST_CHECK_EQUAL(cells[5], 2);
  BOOST_CHECK_EQUAL(cells[6], 2);
  BOOST_CHECK_EQUAL(cells[7], 1);
  BOOST_CHECK_EQUAL(cells[8], 1);
  BOOST_CHECK_EQUAL(cells[9], 1);

  BOOST_CHECK_EQUAL(cells[10], 3);
  BOOST_CHECK_EQUAL(cells[11], 2);
  BOOST_CHECK_EQUAL(cells[12], 1);
  BOOST_CHECK_EQUAL(cells[13], 4);
  BOOST_CHECK_EQUAL(cells[14], 1);

  BOOST_CHECK_EQUAL(cells[15], 3);
  BOOST_CHECK_EQUAL(cells[16], 2);
  BOOST_CHECK_EQUAL(cells[17], 1);
  BOOST_CHECK_EQUAL(cells[18], 4);
  BOOST_CHECK_EQUAL(cells[19], 4);

  BOOST_CHECK_EQUAL(cells[20], 6);
  BOOST_CHECK_EQUAL(cells[21], 5);
  BOOST_CHECK_EQUAL(cells[22], 4);
  BOOST_CHECK_EQUAL(cells[23], 4);
  BOOST_CHECK_EQUAL(cells[24], 4);
}


void testAllMVRaster(
         dal::RasterDriver& driver,
         std::string const& name)
{
  boost::shared_ptr<dal::Raster> raster;

  // Open.
  raster.reset(driver.open(name));
  BOOST_REQUIRE(raster);
  BOOST_CHECK_EQUAL(raster->nrRows(), size_t(3));
  BOOST_CHECK_EQUAL(raster->nrCols(), size_t(2));
  BOOST_CHECK_EQUAL(raster->cellSize(), 50.0);
  BOOST_CHECK_EQUAL(raster->west(), 100000.0);
  BOOST_CHECK_EQUAL(raster->south(), 199850.0);
  BOOST_CHECK_EQUAL(raster->typeId(), dal::TI_UINT1);

  raster.reset(driver.read(name));
  BOOST_REQUIRE(raster);
  UINT1 const* cells = static_cast<UINT1 const*>(raster->cells());
  BOOST_REQUIRE(cells);
  BOOST_CHECK(pcr::isMV(cells[0]));
  BOOST_CHECK(pcr::isMV(cells[1]));
  BOOST_CHECK(pcr::isMV(cells[2]));
  BOOST_CHECK(pcr::isMV(cells[3]));
  BOOST_CHECK(pcr::isMV(cells[4]));
  BOOST_CHECK(pcr::isMV(cells[5]));
}


void testTemporalRaster(
         dal::RasterDriver& driver,
         std::string const& name)
{
  boost::shared_ptr<dal::Raster> raster;

  dal::DataSpace space;
  {
    std::vector<size_t> timeSteps;
    timeSteps.push_back(1);
    timeSteps.push_back(100);
    timeSteps.push_back(1);
    dal::Dimension time(dal::Time, timeSteps);
    space.addDimension(time);
  }

  dal::DataSpaceQueryResult queryResult = dynamic_cast<dal::Driver const&>(
         driver).search(name, space, dal::HaltOnFirstItemFound);
  BOOST_REQUIRE(queryResult);
  raster.reset(dynamic_cast<dal::Raster*>(driver.open(name, space,
         queryResult.address())));

  BOOST_REQUIRE(raster);
  BOOST_CHECK_EQUAL(raster->nrRows(), size_t(3));
  BOOST_CHECK_EQUAL(raster->nrCols(), size_t(2));
  BOOST_CHECK_EQUAL(raster->cellSize(), 50.0);
  BOOST_CHECK_EQUAL(raster->west(), 100000.0);
  BOOST_CHECK_EQUAL(raster->south(), 199850.0);
  BOOST_CHECK_EQUAL(raster->typeId(), dal::TI_INT4);

  dal::DataSpaceAddress address(queryResult.address());
  BOOST_REQUIRE_EQUAL(address.size(), size_t(1));
  BOOST_CHECK_EQUAL(address.coordinate<size_t>(0), size_t(10));
}

} // namespace detail


struct Fixture:
    private dev::GDalClient,
    private ClientWrapper

{

    Fixture()
        : dev::GDalClient(),
          ClientWrapper("/my/path/gdal_raster_driver_test", true)
    {
        GetGDALDriverManager()->AutoLoadDrivers();
    }

    ~Fixture()
    {
    }

};


BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(description)
{
  using namespace dal;

  GDALDriverManager* manager = GetGDALDriverManager();
  assert(manager->GetDriverCount() > 0);
  GDALDriver* gdalDriver = manager->GetDriver(0);

  GDALRasterDriver driver(gdalDriver);
  BOOST_CHECK_EQUAL(driver.description(),
         std::string("GDAL raster driver for ") + gdalDriver->GetDescription());
}


BOOST_AUTO_TEST_CASE(unexisting)
{
  using namespace dal;

  std::string filename = "unexisting";
  GDALRasterDriver driver("PCRaster");

  Raster* raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
  BOOST_CHECK(!raster);

  bool exceptionCaught;
  try {
    exceptionCaught = false;
    raster = dynamic_cast<RasterDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_CHECK_EQUAL(exception.message(),
         "Data source " + filename + "(raster):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(empty)
{
  using namespace dal;

  std::string filename = "emptyfile";
  GDALRasterDriver driver("PCRaster");

  Raster* raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
  BOOST_CHECK(!raster);

  bool exceptionCaught;
  try {
    exceptionCaught = false;
    raster = dynamic_cast<RasterDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_CHECK_EQUAL(exception.message(),
        "Data source " + filename + "(raster):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
  delete raster;
}


// Only works when a gdal library including the PCRaster driver is
// installed!
BOOST_AUTO_TEST_CASE(pcraster)
{
  using namespace dal;

  if(GDALRasterDriver::driverIsAvailable("PCRaster")) {
    // Try to open and read all kinds (boolean, ordinal, nominal, scalar,
    // directional, ldd) of rasters. Test values, including missing values.
    GDALRasterDriver driver("PCRaster");
    detail::testUInt1Raster1(driver, "boolean.Result.map");
    detail::testUInt1Raster2(driver, "accu.Ldd.imap");
    detail::testInt4Raster1(driver, "areaarea.Class.imap");
    detail::testInt4Raster2(driver, "map2col.PCRmap2.imap");
    detail::testReal4Raster1(driver, "abs.Expr.imap");
    detail::testReal4Raster2(driver, "nodirection.Expr.imap");
    detail::testAllMVRaster(driver, "allmv.pcrmap");
    detail::testTemporalRaster(driver, "soil");
  }
}



//   // if(GDALRasterDriver::driverIsAvailable("HDF4Image")) {
// void dal::GDALRasterDriverTest::testHdf4Image()
// {
//   GDALRasterDriver driver("HDF4Image");
//   detail::testUInt1Raster1(driver, "boolean.hdf4");
//   detail::testUInt1Raster2(driver, "ldd.hdf4");
//   detail::testInt4Raster1(driver, "nominal.hdf4");
//   detail::testInt4Raster2(driver, "ordinal.hdf4");
//   detail::testReal4Raster1(driver, "scalar.hdf4");
//   detail::testReal4Raster2(driver, "directional.hdf4");
//   detail::testAllMVRaster(driver, "allmv.hdf4");
//   detail::testTemporalRaster(driver, "soil.hdf4");
// }



//   // if(GDALRasterDriver::driverIsAvailable("HDF5")) {
// void dal::GDALRasterDriverTest::testHdf5()
// {
//   GDALRasterDriver driver("HDF5Image");
//   detail::testUInt1Raster1(driver, "boolean.hdf5");
//   detail::testUInt1Raster2(driver, "ldd.hdf5");
//   detail::testInt4Raster1(driver, "nominal.hdf5");
//   detail::testInt4Raster2(driver, "ordinal.hdf5");
//   detail::testReal4Raster1(driver, "scalar.hdf5");
//   detail::testReal4Raster2(driver, "directional.hdf5");
//   detail::testAllMVRaster(driver, "allmv.hdf5");
//   detail::testTemporalRaster(driver, "soil.hdf5");
// }



BOOST_AUTO_TEST_CASE(geotiff)
{
  using namespace dal;

  if(GDALRasterDriver::driverIsAvailable("GTiff")) {
      // Check results from converting boolean PCRaster raster to GeoTiff.
      GDALRasterDriver driver("GTiff");
      detail::testUInt1Raster1(driver, "boolean.tiff");
      detail::testUInt1Raster2(driver, "ldd.tiff");
      detail::testInt4Raster1(driver, "nominal.tiff");
      detail::testInt4Raster2(driver, "ordinal.tiff");
      detail::testReal4Raster1(driver, "scalar.tiff");
      detail::testReal4Raster2(driver, "directional.tiff");
      detail::testAllMVRaster(driver, "allmv.tiff");
      detail::testTemporalRaster(driver, "soil.tiff");
  }
}


BOOST_AUTO_TEST_CASE(esri_ascii_grid1)
{
  using namespace dal;

  std::string filename = "esriasciigrid1.asc";
  GDALRasterDriver driver("AAIGrid");
  Raster *raster;

  {
    raster = dynamic_cast<Raster*>(
           dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(raster);

    // BOOST_CHECK_EQUAL(raster->name(), std::string("esriasciigrid1.asc"));
    BOOST_CHECK_EQUAL(raster->nrRows(), size_t(4));
    BOOST_CHECK_EQUAL(raster->nrCols(), size_t(3));
    BOOST_CHECK_EQUAL(raster->cellSize(), 10.0);
    BOOST_CHECK_EQUAL(raster->west(), 3.0);
    BOOST_CHECK_EQUAL(raster->south(), 4.0);
    // BOOST_CHECK_EQUAL(raster->projection(), YINCRS2N);
    // BOOST_CHECK_EQUAL(raster->angle(), 0.0);
    BOOST_REQUIRE_EQUAL(raster->typeId(), TI_INT4);

    delete raster;
  }

  {
    raster = dynamic_cast<RasterDriver&>(driver).read(filename);
    BOOST_CHECK(raster);
    INT4 const* cells = static_cast<INT4 const*>(raster->cells());
    BOOST_CHECK(cells);

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

    delete raster;
  }
}


BOOST_AUTO_TEST_CASE(write_)
{
  using namespace dal;

  GDALRasterDriver driver("EHdr");

  Raster raster(2, 2, 10, 0.0, 0.0, TI_REAL4);

  REAL4 cells[4] = { 3.4f, 1 /* MV */, 1, 0};
  pcr::setMV(cells[1]);

  raster.setCellsReference(cells);

  dynamic_cast<RasterDriver&>(driver).write(raster, "gdalWrite.bil");

  // bin/linux-develop/testdir/gdalWrite.hdr
  // bin/linux-develop/testdir/int2mv.hdr
}


BOOST_AUTO_TEST_CASE(default_extension)
{
  using namespace dal;

  std::string filename = "esriasciigrid1";
  GDALRasterDriver driver("AAIGrid");
  Raster* raster;

  BOOST_CHECK(dynamic_cast<Driver&>(driver).exists("esriasciigrid1.asc"));
  BOOST_CHECK(dynamic_cast<Driver&>(driver).exists("esriasciigrid1"));

  {
    raster = dynamic_cast<Raster*>(
           dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(raster);

    BOOST_CHECK_EQUAL(raster->nrRows(), size_t(4));
    BOOST_CHECK_EQUAL(raster->nrCols(), size_t(3));
    BOOST_CHECK_EQUAL(raster->cellSize(), 10.0);
    BOOST_CHECK_EQUAL(raster->west(), 3.0);
    BOOST_CHECK_EQUAL(raster->south(), 4.0);
    BOOST_CHECK_EQUAL(raster->typeId(), TI_INT4);

    delete raster;
  }
}


BOOST_AUTO_TEST_CASE(rectangular_cells)
{
  using namespace dal;

  // TODO create raster with rectangular cells. Use GDAL Python extension.
  // TODO Make sure the detailed error message gets out.
  BOOST_WARN(false);
}
