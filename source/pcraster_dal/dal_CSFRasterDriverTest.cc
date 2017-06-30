#define BOOST_TEST_MODULE pcraster dal csf_raster_driver
#include <boost/test/unit_test.hpp>
#include "dal_CSFRasterDriver.h"
#include "dal_Exception.h"
#include "dal_FilesystemUtils.h"
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
        static ClientWrapper client("/my/path/csf_raster_driver_test", true);
    }

    ~Fixture()
    {
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(description)
{
  using namespace dal;

  CSFRasterDriver driver;
  BOOST_CHECK_EQUAL(driver.description(), "CSF-2.0 raster file format");
}


BOOST_AUTO_TEST_CASE(unexisting)
{
  using namespace dal;

  std::string filename = "unexisting";
  CSFRasterDriver driver;

  Raster* raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
  BOOST_CHECK(!raster);
  BOOST_CHECK_THROW(dynamic_cast<Driver const&>(driver).dataSpace(filename),
         Exception);

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
  CSFRasterDriver driver;

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


BOOST_AUTO_TEST_CASE(dtm_small)
{
  using namespace dal;

  std::string filename = "dtmsmall.map";
  CSFRasterDriver driver;
  Raster* raster;

  {
    raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(raster);

    // BOOST_CHECK_EQUAL(raster->name(), std::string("dtmsmall.map"));
    BOOST_CHECK_EQUAL(raster->nrRows(), size_t(4));
    BOOST_CHECK_EQUAL(raster->nrCols(), size_t(4));
    BOOST_CHECK_EQUAL(raster->cellSize(), 1.0);
    BOOST_CHECK_EQUAL(raster->west(), 0.0);
    BOOST_CHECK_EQUAL(raster->north(), 0.0);
    BOOST_CHECK_EQUAL(raster->typeId(), TI_REAL4);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_CHECK_EQUAL(raster->properties().value<REAL8>(DAL_CSF_ANGLE), 0.0);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_CHECK_EQUAL(raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE),
         VS_SCALAR);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_CHECK_EQUAL(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION),
         PT_YINCT2B);

    delete raster;
  }

  {
    DataSpace dataSpace =
         dynamic_cast<Driver const&>(driver).dataSpace(filename);
    BOOST_CHECK_EQUAL(dataSpace.rank(), size_t(1));
    BOOST_CHECK(dataSpace.isSpatial());
    BOOST_CHECK(dataSpace.hasRaster());
    BOOST_CHECK(!dataSpace.hasTime());
  }

  {
    raster = dynamic_cast<RasterDriver&>(driver).read(filename);
    REAL4 const* cells = static_cast<REAL4 const*>(raster->cells());
    assert(cells);

    for(size_t row = 0; row < raster->nrRows(); ++row) {
      size_t index = row * raster->nrCols();
      BOOST_CHECK(pcr::isMV(cells[index + 0]));
      BOOST_CHECK_EQUAL(cells[index + 1], 1.0);
      BOOST_CHECK_EQUAL(cells[index + 2], 1.0);
      BOOST_CHECK_EQUAL(cells[index + 3], 1.0);
    }

    delete raster;
  }
}


BOOST_AUTO_TEST_CASE(accu_ldd_i_map)
{
  using namespace dal;

  std::string filename = "accu.Ldd.imap";
  CSFRasterDriver driver;
  Raster* raster;

  {
    raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(raster);
    BOOST_CHECK_EQUAL(raster->nrRows(), size_t(5));
    BOOST_CHECK_EQUAL(raster->nrCols(), size_t(5));
    BOOST_CHECK_EQUAL(raster->cellSize(), 1.0);
    BOOST_CHECK_EQUAL(raster->west() , 0.0);
    BOOST_CHECK_EQUAL(raster->north(), 0.0);
    BOOST_CHECK_EQUAL(raster->south(), -5.0);
    BOOST_CHECK_EQUAL(raster->east() , 5.0);
    BOOST_CHECK_EQUAL(raster->typeId(), TI_UINT1);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_CHECK_EQUAL(raster->properties().value<REAL8>(DAL_CSF_ANGLE), 0.0);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_CHECK_EQUAL(raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE), VS_LDD);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_CHECK_EQUAL(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION), PT_YINCT2B);

    delete raster;
  }

  {
    raster = dynamic_cast<RasterDriver&>(driver).read(filename);
    UINT1 const* cells = static_cast<UINT1 const*>(raster->cells());

    assert(cells);

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

    delete raster;
  }
}


BOOST_AUTO_TEST_CASE(names)
{
  using namespace dal;

  std::string filename = "sillyname";
  CSFRasterDriver driver;
  Raster* raster;

  {
    raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(raster);
    BOOST_CHECK_EQUAL(raster->nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(raster->nrCols(), size_t(2));
    BOOST_CHECK_EQUAL(raster->cellSize(), 50.0);
    BOOST_CHECK_EQUAL(raster->west() , 100000);
    BOOST_CHECK_EQUAL(raster->north(), 200000);
    BOOST_CHECK_EQUAL(raster->typeId(), TI_INT4);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_CHECK_EQUAL(raster->properties().value<REAL8>(DAL_CSF_ANGLE), 0.0);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_CHECK_EQUAL(raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE), VS_NOMINAL);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_CHECK_EQUAL(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION), PT_YDECT2B);

    delete raster;
  }

  filename = "sillyname.map";

  {
    raster = dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(raster);
    BOOST_CHECK_EQUAL(raster->nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(raster->nrCols(), size_t(2));
    BOOST_CHECK_EQUAL(raster->cellSize(), 50.0);
    BOOST_CHECK_EQUAL(raster->west() , 100000);
    BOOST_CHECK_EQUAL(raster->north(), 200000);
    BOOST_CHECK_EQUAL(raster->typeId(), TI_INT4);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_CHECK_EQUAL(raster->properties().value<REAL8>(DAL_CSF_ANGLE), 0.0);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_CHECK_EQUAL(raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE), VS_ORDINAL);

    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_CHECK_EQUAL(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION), PT_YDECT2B);

    delete raster;
  }
}


BOOST_AUTO_TEST_CASE(write_)
{
  using namespace dal;

  // Create a raster to write.
  size_t nrRows = 2;
  size_t nrCols = 3;
  double cellSize = 10.0;
  double west = 0.0;
  double north = 0.0;
  TypeId typeId = TI_REAL4;

  Raster raster(nrRows, nrCols, cellSize, west, north, typeId);
  raster.createCells();
  REAL4* cells = raster.cells<REAL4>();

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
  CSFRasterDriver driver;
  driver.write(raster, space, address, "leavehomealonefailed.pcrmap");
  BOOST_CHECK(exists("leavehomealonefailed_15.pcrmap"));
}


BOOST_AUTO_TEST_CASE(properties)
{
  using namespace dal;

  std::string name = "d83.map";

  CSFRasterDriver driver;

  {
    boost::shared_ptr<Raster> raster(dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(name)));
    assert(raster);

    // Raster properties.
    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_CHECK(raster->properties().hasValue(DAL_LEGEND));
    BOOST_CHECK(!raster->properties().hasValue(DAL_DEFAULT_EXTENSION));
    // If not, add more tests.
    BOOST_CHECK_EQUAL(raster->properties().size(), size_t(4));

    // This test is only useful in case a raster from a dynamic stack is read.
    // Driver properties.
    // BOOST_CHECK(driver.properties().hasValue(name));
    // Properties const& properties(driver.properties().value<Properties>(name));
    // BOOST_CHECK(properties.hasValue(DAL_FILENAME_CONVENTION));
    // If not, add more tests.
    // BOOST_CHECK_EQUAL(properties.size(), 1);
  }

  {
    // No extension in name, should find d83.map.
    name = "d83";
    boost::shared_ptr<Raster> raster(dynamic_cast<Raster*>(
         dynamic_cast<Driver&>(driver).open(name)));
    BOOST_REQUIRE(raster);

    // Raster properties.
    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_CHECK(raster->properties().hasValue(DAL_LEGEND));
    BOOST_CHECK(!raster->properties().hasValue(DAL_DEFAULT_EXTENSION));
    // If not, add more tests.
    BOOST_CHECK_EQUAL(raster->properties().size(), size_t(4));
  }

  {
    name = "d83.map";
    boost::shared_ptr<Raster> raster(dynamic_cast<RasterDriver&>(driver).read(name));
    assert(raster);
    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_VALUESCALE));
    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_PROJECTION));
    BOOST_CHECK(raster->properties().hasValue(DAL_CSF_ANGLE));
    BOOST_CHECK(!raster->properties().hasValue(DAL_DEFAULT_EXTENSION));
    BOOST_CHECK(raster->properties().hasValue(DAL_LEGEND));
    // If not, add more tests.
    // BOOST_CHECK(raster->properties().size(), 4);
  }
}


BOOST_AUTO_TEST_CASE(query)
{
  using namespace dal;

  // Test whether the space of the query result is correct.

  CSFRasterDriver driver;

  DataSpace space;
  std::vector<size_t> steps;
  steps.push_back(1);
  steps.push_back(15);
  steps.push_back(1);
  space.addDimension(Dimension(Time, steps));

  DataSpaceQueryResult result = driver.search("soil", space, SearchForAllItems);

  BOOST_CHECK(result);

  {
    DataSpace const& space(result.space());
    BOOST_CHECK(space.hasTime());

    Dimension const& dimension(space.dimension(Time));
    BOOST_CHECK_EQUAL(dimension.discretisation(), RegularDiscretisation);
    BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(3));
    BOOST_CHECK_EQUAL(dimension.nrCoordinates(), size_t(6));
    BOOST_CHECK_EQUAL(dimension.coordinate<size_t>(0), size_t(10));
    BOOST_CHECK_EQUAL(dimension.coordinate<size_t>(1), size_t(11));
    BOOST_CHECK_EQUAL(dimension.coordinate<size_t>(2), size_t(12));
    BOOST_CHECK_EQUAL(dimension.coordinate<size_t>(3), size_t(13));
    BOOST_CHECK_EQUAL(dimension.coordinate<size_t>(4), size_t(14));
    BOOST_CHECK_EQUAL(dimension.coordinate<size_t>(5), size_t(15));

    DataSpaceAddress const& address(result.address());
    BOOST_CHECK(space.isValid(address));

    BOOST_CHECK_EQUAL(address.coordinate<size_t>(0), size_t(10));
  }
}
