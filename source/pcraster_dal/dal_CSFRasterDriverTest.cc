#define BOOST_TEST_MODULE pcraster dal csf_raster_driver
#include <boost/test/unit_test.hpp>
#include "dal_CSFRasterDriver.h"
#include "dal_Exception.h"
#include "dal_FilesystemUtils.h"
#include "dal_Client.h"


class ClientWrapper : public dal::Client {
public:
  ClientWrapper(std::filesystem::path const& prefix,
                   bool  /*addAllDrivers*/=false,
                   bool  /*cacheDatasetInfo*/=true)
  : dal::Client(prefix) {
  }
};


struct Fixture
{

    Fixture()
    {
        static ClientWrapper const client("/my/path/csf_raster_driver_test", true);
    }

    ~Fixture()
    {
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(description)
{
  using namespace dal;

  CSFRasterDriver const driver;
  BOOST_TEST(driver.description() == "CSF-2.0 raster file format");
}


BOOST_AUTO_TEST_CASE(unexisting)
{
  using namespace dal;

  std::string const filename = "unexisting";
  CSFRasterDriver driver;

  auto* raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
  BOOST_TEST(!raster);
  BOOST_CHECK_THROW(dynamic_cast<Driver const&>(driver).dataSpace(filename),
         Exception);

  bool exceptionCaught = false;
  try {
    exceptionCaught = false;
    raster = dynamic_cast<RasterDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_TEST(exception.message() ==
       "Data source " + filename + "(raster):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_TEST(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(empty)
{
  using namespace dal;

  std::string const filename = "emptyfile";
  CSFRasterDriver driver;

  auto* raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
  BOOST_TEST(!raster);

  bool exceptionCaught = false;
  try {
    exceptionCaught = false;
    raster = dynamic_cast<RasterDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_TEST(exception.message() ==
       "Data source " + filename + "(raster):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_TEST(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(dtm_small)
{
  using namespace dal;

  std::string const filename = "dtmsmall.map";
  CSFRasterDriver driver;
  Raster* raster = nullptr;

  {
    raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_TEST(raster);

    // BOOST_TEST(raster->name() == std::string("dtmsmall.map"));
    BOOST_TEST(raster->nrRows() == size_t(4));
    BOOST_TEST(raster->nrCols() == size_t(4));
    BOOST_TEST(raster->cellSize() == 1.0);
    BOOST_TEST(raster->west() == 0.0);
    BOOST_TEST(raster->north() == 0.0);
    BOOST_TEST(raster->typeId() == TI_REAL4);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_TEST(raster->properties().value<REAL8>(DAL_CSF_ANGLE) == 0.0);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_TEST(raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE) ==
         VS_SCALAR);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_TEST(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION) ==
         PT_YINCT2B);

    delete raster;
  }

  {
    DataSpace const dataSpace =
         dynamic_cast<Driver const&>(driver).dataSpace(filename);
    BOOST_TEST(dataSpace.rank() == size_t(1));
    BOOST_TEST(dataSpace.isSpatial());
    BOOST_TEST(dataSpace.hasRaster());
    BOOST_TEST(!dataSpace.hasTime());
  }

  {
    raster = dynamic_cast<RasterDriver&>(driver).read(filename);
    auto const* cells = static_cast<REAL4 const*>(raster->cells());
    assert(cells);

    for(size_t row = 0; row < raster->nrRows(); ++row) {
      size_t const index = row * raster->nrCols();
      BOOST_TEST(pcr::isMV(cells[index + 0]));
      BOOST_TEST(cells[index + 1] == 1.0);
      BOOST_TEST(cells[index + 2] == 1.0);
      BOOST_TEST(cells[index + 3] == 1.0);
    }

    delete raster;
  }
}


BOOST_AUTO_TEST_CASE(accu_ldd_i_map)
{
  using namespace dal;

  std::string const filename = "accu.Ldd.imap";
  CSFRasterDriver driver;
  Raster* raster = nullptr;

  {
    raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_TEST(raster);
    BOOST_TEST(raster->nrRows() == size_t(5));
    BOOST_TEST(raster->nrCols() == size_t(5));
    BOOST_TEST(raster->cellSize() == 1.0);
    BOOST_TEST(raster->west()  == 0.0);
    BOOST_TEST(raster->north() == 0.0);
    BOOST_TEST(raster->south() == -5.0);
    BOOST_TEST(raster->east()  == 5.0);
    BOOST_TEST(raster->typeId() == TI_UINT1);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_TEST(raster->properties().value<REAL8>(DAL_CSF_ANGLE) == 0.0);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_TEST(raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE) == VS_LDD);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_TEST(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION) == PT_YINCT2B);

    delete raster;
  }

  {
    raster = dynamic_cast<RasterDriver&>(driver).read(filename);
    auto const* cells = static_cast<UINT1 const*>(raster->cells());

    assert(cells);

    BOOST_TEST(cells[0] == 2);
    BOOST_TEST(cells[1] == 2);
    BOOST_TEST(cells[2] == 2);
    BOOST_TEST(cells[3] == 1);
    BOOST_TEST(cells[4] == 1);

    BOOST_TEST(cells[5] == 2);
    BOOST_TEST(cells[6] == 2);
    BOOST_TEST(cells[7] == 1);
    BOOST_TEST(cells[8] == 1);
    BOOST_TEST(cells[9] == 1);

    BOOST_TEST(cells[10] == 3);
    BOOST_TEST(cells[11] == 2);
    BOOST_TEST(cells[12] == 1);
    BOOST_TEST(cells[13] == 4);
    BOOST_TEST(cells[14] == 1);

    BOOST_TEST(cells[15] == 3);
    BOOST_TEST(cells[16] == 2);
    BOOST_TEST(cells[17] == 1);
    BOOST_TEST(cells[18] == 4);
    BOOST_TEST(cells[19] == 4);

    BOOST_TEST(cells[20] == 6);
    BOOST_TEST(cells[21] == 5);
    BOOST_TEST(cells[22] == 4);
    BOOST_TEST(cells[23] == 4);
    BOOST_TEST(cells[24] == 4);

    delete raster;
  }
}


BOOST_AUTO_TEST_CASE(names)
{
  using namespace dal;

  std::string filename = "sillyname";
  CSFRasterDriver driver;
  Raster* raster = nullptr;

  {
    raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_TEST(raster);
    BOOST_TEST(raster->nrRows() == size_t(3));
    BOOST_TEST(raster->nrCols() == size_t(2));
    BOOST_TEST(raster->cellSize() == 50.0);
    BOOST_TEST(raster->west()  == 100000);
    BOOST_TEST(raster->north() == 200000);
    BOOST_TEST(raster->typeId() == TI_INT4);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_TEST(raster->properties().value<REAL8>(DAL_CSF_ANGLE) == 0.0);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_TEST(raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE) == VS_NOMINAL);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_TEST(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION) == PT_YDECT2B);

    delete raster;
  }

  filename = "sillyname.map";

  {
    raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_TEST(raster);
    BOOST_TEST(raster->nrRows() == size_t(3));
    BOOST_TEST(raster->nrCols() == size_t(2));
    BOOST_TEST(raster->cellSize() == 50.0);
    BOOST_TEST(raster->west()  == 100000);
    BOOST_TEST(raster->north() == 200000);
    BOOST_TEST(raster->typeId() == TI_INT4);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_TEST(raster->properties().value<REAL8>(DAL_CSF_ANGLE) == 0.0);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_TEST(raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE) == VS_ORDINAL);

    BOOST_TEST(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_TEST(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION) == PT_YDECT2B);

    delete raster;
  }
}


BOOST_AUTO_TEST_CASE(write_)
{
  using namespace dal;

  // Create a raster to write.
  size_t const nrRows = 2;
  size_t const nrCols = 3;
  double const cellSize = 10.0;
  double const west = 0.0;
  double const north = 0.0;
  TypeId const typeId = TI_REAL4;

  Raster raster(nrRows, nrCols, cellSize, west, north, typeId);
  raster.createCells();
  auto* cells = raster.cells<REAL4>();

  for(size_t i = 0; i < raster.nrCells(); ++i) {
    cells[i] = REAL4(i);
  }

  // Create a space and address.
  DataSpace space;
  std::vector<size_t> steps;
  steps.push_back(10);
  steps.push_back(20);
  steps.push_back(1);
  space.addDimension(Dimension(Time, steps));
  DataSpaceAddress address(space.address());
  address.setCoordinate<size_t>(0, 15);

  // Write raster at specified address.
  CSFRasterDriver const driver;
  driver.write(raster, space, address, "leavehomealonefailed.pcrmap");
  BOOST_TEST(exists("leavehomealonefailed_15.pcrmap"));
}


BOOST_AUTO_TEST_CASE(properties)
{
  using namespace dal;

  std::string name = "d83.map";

  CSFRasterDriver driver;

  {
    std::shared_ptr<Raster> const raster(dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(name)));
    assert(raster);

    // Raster properties.
    BOOST_TEST(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_TEST(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_TEST(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_TEST(raster->properties().hasValue(DAL_LEGEND));
    BOOST_TEST(!raster->properties().hasValue(DAL_DEFAULT_EXTENSION));
    // If not, add more tests.
    BOOST_TEST(raster->properties().size() == size_t(4));

    // This test is only useful in case a raster from a dynamic stack is read.
    // Driver properties.
    // BOOST_TEST(driver.properties().hasValue(name));
    // Properties const& properties(driver.properties().value<Properties>(name));
    // BOOST_TEST(properties.hasValue(DAL_FILENAME_CONVENTION));
    // If not, add more tests.
    // BOOST_TEST(properties.size() == 1);
  }

  {
    // No extension in name, should find d83.map.
    name = "d83";
    std::shared_ptr<Raster> const raster(dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(name)));
    BOOST_TEST_REQUIRE(raster);

    // Raster properties.
    BOOST_TEST(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_TEST(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_TEST(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_TEST(raster->properties().hasValue(DAL_LEGEND));
    BOOST_TEST(!raster->properties().hasValue(DAL_DEFAULT_EXTENSION));
    // If not, add more tests.
    BOOST_TEST(raster->properties().size() == size_t(4));
  }

  {
    name = "d83.map";
    std::shared_ptr<Raster> const raster(dynamic_cast<RasterDriver&>(driver).read(name));
    assert(raster);
    BOOST_TEST(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_TEST(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_TEST(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_TEST(!raster->properties().hasValue(DAL_DEFAULT_EXTENSION));
    BOOST_TEST(raster->properties().hasValue(DAL_LEGEND));
    // If not, add more tests.
    // BOOST_TEST(raster->properties().size(), 4);
  }
}


BOOST_AUTO_TEST_CASE(query)
{
  using namespace dal;

  // Test whether the space of the query result is correct.

  CSFRasterDriver const driver;

  DataSpace space;
  std::vector<size_t> steps;
  steps.push_back(1);
  steps.push_back(15);
  steps.push_back(1);
  space.addDimension(Dimension(Time, steps));

  DataSpaceQueryResult const result = driver.search("soil", space, SearchForAllItems);

  BOOST_TEST(result);

  {
    DataSpace const& space(result.space());
    BOOST_TEST(space.hasTime());

    Dimension const& dimension(space.dimension(Time));
    BOOST_TEST(dimension.discretisation() == RegularDiscretisation);
    BOOST_TEST(dimension.nrValues() == size_t(3));
    BOOST_TEST(dimension.nrCoordinates() == size_t(6));
    BOOST_TEST(dimension.coordinate<size_t>(0) == size_t(10));
    BOOST_TEST(dimension.coordinate<size_t>(1) == size_t(11));
    BOOST_TEST(dimension.coordinate<size_t>(2) == size_t(12));
    BOOST_TEST(dimension.coordinate<size_t>(3) == size_t(13));
    BOOST_TEST(dimension.coordinate<size_t>(4) == size_t(14));
    BOOST_TEST(dimension.coordinate<size_t>(5) == size_t(15));

    DataSpaceAddress const& address(result.address());
    BOOST_TEST(space.isValid(address));

    BOOST_TEST(address.coordinate<size_t>(0) == size_t(10));
  }
}
