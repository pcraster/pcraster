#define BOOST_TEST_MODULE pcraster dal data_source
#include <boost/test/unit_test.hpp>
#include "dal_DataSource.h"
#include "dal_Exception.h"
#include "dal_Raster.h"
#include "dal_SpatialCoordinate.h"
#include "dal_StackInfo.h"
#include "dal_Table.h"
#ifdef _MSC_VER
#ifdef _WIN64
// compiler hangs otherwise
#pragma optimize( "g", off )
#pragma warning(once: 4748)
#endif
#endif
#include "dal_Client.h"


class ClientWrapper : public dal::Client {
public:
  ClientWrapper(boost::filesystem::path const& prefix,
                   bool addAllDrivers=false,
                   bool cacheDatasetInfo=true)
  : dal::Client(prefix, addAllDrivers) {
  }
};



struct Fixture
{

    Fixture()
    {
        dal::Driver::datasetProperties().clear();
        static ClientWrapper client("/my/path/csf_raster_driver_test", true);
    }

    ~Fixture()
    {
    }

};


BOOST_FIXTURE_TEST_SUITE(data_source, Fixture)

BOOST_AUTO_TEST_CASE(unexisting)
{
  using namespace dal;

  std::string name = "doesnotexist";
  bool exceptionCaught = false;

  {
    try {
      exceptionCaught = false;
      DataSource source(name);
    }
    catch(Exception& exception) {
      BOOST_CHECK_EQUAL(exception.message(),
         "Data source " + name + ":\ncannot be opened");
      exceptionCaught = true;
    }
    BOOST_CHECK(exceptionCaught);
  }
}


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  std::string name = "dtmsmall.map";

  {
    DataSource source(name);
    BOOST_CHECK_EQUAL(source.name(), name);

    DataSpace dataSpace = source.dataSpace();
    BOOST_CHECK(dataSpace.hasRaster());
    BOOST_CHECK(!dataSpace.hasTime());

    boost::shared_ptr<Raster> raster(source.raster());
    BOOST_CHECK(raster);
  }
}


BOOST_AUTO_TEST_CASE(soil)
{
  using namespace dal;

  std::string name;

  {
    name = "soil";

    BOOST_CHECK_NO_THROW(
      DataSource dataSource(name);
      BOOST_CHECK(dataSource.enclosingDataSpace() == DataSpace());
    );
  }

  {
    name = "soil0000.001+110";
    StackInfo info(name, false);
    BOOST_CHECK(info.isDynamic());

    name = info.name();

    DataSpace space;
    std::vector<size_t> timeSteps;
    timeSteps.push_back(info.first());
    timeSteps.push_back(info.last());
    timeSteps.push_back(1);

    space.addDimension(Dimension(Time, timeSteps));

    DataSource source(name, space);
    BOOST_CHECK_EQUAL(source.name(), info.name());

    DataSpace dataSpace = source.dataSpace();
    BOOST_CHECK(dataSpace.hasTime());
    BOOST_CHECK(dataSpace.hasRaster());

    DataSpaceAddress address(1);

    address.setCoordinate<size_t>(0, 10);
    boost::shared_ptr<Raster> raster(source.raster(address));
    BOOST_CHECK(raster);

    address.setCoordinate<size_t>(0, 50);
    raster.reset(source.raster(address));
    BOOST_CHECK(raster);

    address.setCoordinate<size_t>(0, 100);
    raster.reset(source.raster(address));
    BOOST_CHECK(raster);

    // No, address lies outside of the data source's data space, which is
    // illegal.
    // bool exceptionCaught = false;

    // {
    //   try {
    //     exceptionCaught = false;
    //     address.setCoordinate<size_t>(0, 101);
    //     raster.reset(source.raster(address));
    //   }
    //   catch(Exception& exception) {
    //     BOOST_CHECK_EQUAL(exception.message(),
    //           "Data source soil0000(raster) at /time[1, 110, 1](101):\ncannot be opened");
    //     exceptionCaught = true;
    //   }
    //   BOOST_CHECK(exceptionCaught);
    // }
  }
}


BOOST_AUTO_TEST_CASE(unique_values)
{
  using namespace dal;

  std::string name;

  {
    name = "soil.map";
    DataSource source(name);
    std::set<INT4> values;
    source.uniqueValues(values);
    BOOST_CHECK_EQUAL(values.size(), size_t(5));
    std::set<INT4>::const_iterator it = values.begin();
    BOOST_CHECK_EQUAL(*it++, 1);
    BOOST_CHECK_EQUAL(*it++, 2);
    BOOST_CHECK_EQUAL(*it++, 4);
    BOOST_CHECK_EQUAL(*it++, 7);
    BOOST_CHECK_EQUAL(*it++, 8);
    BOOST_CHECK(it == values.end());
  }

  {
    name = "soil";
    DataSpace space;
    std::vector<size_t> timeSteps;
    timeSteps.push_back(10);
    timeSteps.push_back(100);
    timeSteps.push_back(1);

    space.addDimension(Dimension(Time, timeSteps));

    DataSource source(name, space);
    std::set<INT4> values;
    source.uniqueValues(values);
    BOOST_CHECK_EQUAL(values.size(), size_t(94));
    std::set<INT4>::const_iterator it = values.begin();
    BOOST_CHECK_EQUAL(*it++, 1);
    BOOST_CHECK_EQUAL(*it++, 2);
    BOOST_CHECK_EQUAL(*it++, 3);
    BOOST_CHECK_EQUAL(*it++, 10);
    BOOST_CHECK_EQUAL(*it++, 11);
    it = --values.end();
    BOOST_CHECK_EQUAL(*it++, 100);
  }
}


BOOST_AUTO_TEST_CASE(raster)
{
  using namespace dal;

  // Create data source.
  {
    std::string name = "values";
    DataSpace space;
    std::vector<size_t> timeSteps;
    timeSteps.push_back(10);
    timeSteps.push_back(20);
    timeSteps.push_back(1);
    space.addDimension(Dimension(Time, timeSteps));
    DataSource source(name, space);

    boost::shared_ptr<Raster> raster(source.open<Raster>());
    assert(raster);

    DataSpaceAddress address(1);
    address.setCoordinate<size_t>(0, 10);

    // Address is at the first address in the data space. This one happens
    // to exist.
    source.read(*raster, address);
    BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(1.0)));

    // Address is at the second address in the data space. This one does
    // not exist. Allong the time dimension data is searched for at previous
    // time steps. In this case, 10.
    address.setCoordinate<size_t>(0, 11);
    source.read(*raster, address);
    BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(1.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(1.0)));

    // Address is at the sixth address in the data space. This one exists.
    address.setCoordinate<size_t>(0, 15);
    source.read(*raster, address);
    BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(6.0)));

    // Address is at the seventh address in the data space. This one does
    // not exist.
    address.setCoordinate<size_t>(0, 16);
    source.read(*raster, address);
    BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(6.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(6.0)));

    // Address is at the 11th address in the data space. This one exists.
    address.setCoordinate<size_t>(0, 20);
    source.read(*raster, address);
    BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(11.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(11.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(11.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(11.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(11.0)));
    BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(11.0)));

    // // -------------------------------------------------------------------------
    // // Turn on interpolation and check returned values again.
    // source.setMissingDataStrategy(DataSource::Interpolate);

    // // Address is at the first address in the data space, exists.
    // address.setCoordinate<size_t>(0, 10);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(1.0)));

    // // Address is at the second address in the data space, interpolate.
    // address.setCoordinate<size_t>(0, 11);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(2.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(2.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(2.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(2.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(2.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(2.0)));

    // // Address is at the sixth address in the data space, exists.
    // address.setCoordinate<size_t>(0, 15);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(6.0)));

    // // Address is at the 7th address in the data space, interpolate.
    // address.setCoordinate<size_t>(0, 16);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(7.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(7.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(7.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(7.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(7.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(7.0)));

    // // Address is at the 11th address in the data space, exists.
    // address.setCoordinate<size_t>(0, 20);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(11.0)));

    // // -------------------------------------------------------------------------
    // // Turn on UsePrevious and check returned values again.
    // source.setMissingDataStrategy(DataSource::UsePrevious);

    // // Address is at the first address in the data space, exists.
    // address.setCoordinate<size_t>(0, 10);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(1.0)));

    // // Address is at the second address in the data space, use previous.
    // address.setCoordinate<size_t>(0, 11);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(1.0)));

    // // Address is at the sixth address in the data space, exists.
    // address.setCoordinate<size_t>(0, 15);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(6.0)));

    // // Address is at the 7th address in the data space, interpolate.
    // address.setCoordinate<size_t>(0, 16);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(6.0)));

    // // Address is at the 11th address in the data space, exists.
    // address.setCoordinate<size_t>(0, 20);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(11.0)));

    // // -------------------------------------------------------------------------
    // // Turn on UseNext and check returned values again.
    // source.setMissingDataStrategy(DataSource::UseNext);

    // // Address is at the first address in the data space, exists.
    // address.setCoordinate<size_t>(0, 10);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(1.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(1.0)));

    // // Address is at the second address in the data space, use previous.
    // address.setCoordinate<size_t>(0, 11);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(6.0)));

    // // Address is at the sixth address in the data space, exists.
    // address.setCoordinate<size_t>(0, 15);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(6.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(6.0)));

    // // Address is at the 7th address in the data space, interpolate.
    // address.setCoordinate<size_t>(0, 16);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(11.0)));

    // // Address is at the 11th address in the data space, exists.
    // address.setCoordinate<size_t>(0, 20);
    // source.read(*raster, address);
    // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(1), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(2), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(3), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(4), REAL4(11.0)));
    // BOOST_CHECK(comparable(raster->cell<REAL4>(5), REAL4(11.0)));
  }
}


BOOST_AUTO_TEST_CASE(dataset_1)
{
  using namespace dal;

  // Empty data space (apart from the raster cells).
  {
    // TODO
  }

  // Data space with time steps. Time steps extend beyond the available steps.
  {
    std::string datasetName = (boost::filesystem::path("dataset1")
         / "aap" / "scalar").string();

    DataSpace space;
    std::vector<size_t> timeSteps;
    timeSteps.push_back(size_t( 9));
    timeSteps.push_back(size_t(21));
    timeSteps.push_back(size_t(1));
    space.addDimension(Dimension(Time, timeSteps));

    DataSource source(datasetName, space);

    BOOST_CHECK_EQUAL(source.name(), datasetName);

    {
      DataSpace realSpace;
      std::vector<size_t> timeSteps;
      timeSteps.push_back(size_t(10));
      timeSteps.push_back(size_t(20));
      timeSteps.push_back(size_t(1));
      realSpace.addDimension(Dimension(Time, timeSteps));
      BOOST_CHECK(source.enclosingDataSpace() == realSpace);
    }

    BOOST_CHECK_EQUAL(source.unitDataSpace().rank(), size_t(1));
    BOOST_CHECK(source.unitDataSpace().hasRaster());

    // -------------------------------------------------------------------------
    // Read data in raster.
    {
      boost::shared_ptr<Raster> raster(source.open<Raster>());
      BOOST_CHECK(raster);
      raster->createCells();

      DataSpaceAddress address(1);

      // Outside data source's data space.
      address.setCoordinate<size_t>(0, 9);
      source.read(*raster, address);
      BOOST_CHECK(raster->allMV());

      // These time steps all exist.
      for(size_t i = 10; i <= 14; ++i) {
        address.setCoordinate<size_t>(0, i);
        source.read(*raster, address);
        BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
        BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(i)));
      }

      // 15 does not, fall back in time to previous time step.
      address.setCoordinate<size_t>(0, 15);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(14.0)));

      // These time steps all exist.
      for(size_t i = 16; i <= 20; ++i) {
        address.setCoordinate<size_t>(0, i);
        source.read(*raster, address);
        BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
        BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(i)));
      }

      // Outside data source's data space.
      address.setCoordinate<size_t>(0, 21);
      source.read(*raster, address);
      BOOST_CHECK(raster->allMV());
    }

    // -------------------------------------------------------------------------
    // Read data in table: time series.
    {
      Table table;
      table.appendCol("", TI_REAL4);

      DataSpaceAddress address(2);
      address.setCoordinate<SpatialCoordinate>(1,
         SpatialCoordinate(100025.0, 199975.0));
      DataSpace iterSpace(source.dataSpace(), address);
      iterSpace.dimension(0) = source.dataSpace().dimension(0);

      source.read(table, iterSpace, address);

      BOOST_REQUIRE_EQUAL(table.nrRecs(), size_t(11));

      for(size_t i = 10; i <= 14; ++i) {
        BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[i - 10]));
        BOOST_CHECK(comparable(table.col<REAL4>(0)[i - 10], REAL4(i)));
      }

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[15 - 10]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[15 - 10], REAL4(14.0)));

      for(size_t i = 16; i <= 20; ++i) {
        BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[i - 10]));
        BOOST_CHECK(comparable(table.col<REAL4>(0)[i - 10], REAL4(i)));
      }
    }
  }

  // Data space with time steps. Time steps narrower then the available steps.
  {
    std::string datasetName = (boost::filesystem::path("dataset1")
         / "aap" / "scalar").string();

    DataSpace space;
    std::vector<size_t> timeSteps;
    timeSteps.push_back(size_t(11));
    timeSteps.push_back(size_t(19));
    timeSteps.push_back(size_t(1));
    space.addDimension(Dimension(Time, timeSteps));
    DataSource source(datasetName, space);

    // -------------------------------------------------------------------------
    // Read data in raster.
    {
      boost::shared_ptr<Raster> raster(source.open<Raster>());
      BOOST_CHECK(raster);
      raster->createCells();

      DataSpaceAddress address(1);

      // These time steps all exist.
      for(size_t i = 11; i <= 14; ++i) {
        address.setCoordinate<size_t>(0, i);
        source.read(*raster, address);
        BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
        BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(i)));
      }

      // 15 does not, fall back in time to previous time step.
      address.setCoordinate<size_t>(0, 15);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(14.0)));

      // These time steps all exist.
      for(size_t i = 16; i <= 19; ++i) {
        address.setCoordinate<size_t>(0, i);
        source.read(*raster, address);
        BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
        BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(i)));
      }
    }

    // -------------------------------------------------------------------------
    // Read data in table: time series.
    {
      Table table;
      table.appendCol("", TI_REAL4);

      DataSpaceAddress address(2);
      address.setCoordinate<SpatialCoordinate>(1,
         SpatialCoordinate(100025.0, 199975.0));
      DataSpace iterSpace(source.dataSpace(), address);
      iterSpace.dimension(0) = source.dataSpace().dimension(0);

      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(9));

      for(size_t i = 11; i <= 14; ++i) {
        BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[i - 11]));
        BOOST_CHECK(comparable(table.col<REAL4>(0)[i - 11], REAL4(i)));
      }

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[15 - 11]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[15 - 11], REAL4(14)));

      for(size_t i = 16; i <= 19; ++i) {
        BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[i - 11]));
        BOOST_CHECK(comparable(table.col<REAL4>(0)[i - 11], REAL4(i)));
      }
    }
  }

  // Data space with time steps. Interval > 1.
  {
    std::string datasetName = (boost::filesystem::path("dataset1")
         / "aap" / "scalar").string();

    DataSpace space;
    std::vector<size_t> timeSteps;
    timeSteps.push_back(size_t( 9));
    timeSteps.push_back(size_t(21));
    timeSteps.push_back(size_t(2));
    space.addDimension(Dimension(Time, timeSteps));
    DataSource source(datasetName, space);

    // -------------------------------------------------------------------------
    // Read data in raster.
    {
      boost::shared_ptr<Raster> raster(source.open<Raster>());
      BOOST_CHECK(raster);
      raster->createCells();

      DataSpaceAddress address(1);

      // Outside of data source's data space.
      address.setCoordinate<size_t>(0, 9);
      source.read(*raster, address);
      BOOST_CHECK(raster->allMV());

      // These time steps all exist.
      for(size_t i = 11; i <= 13; i += 2) {
        address.setCoordinate<size_t>(0, i);
        source.read(*raster, address);
        BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
        BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(i)));
      }

      // Fall back in time to previous time step (which is 13 given the
      // interval).
      address.setCoordinate<size_t>(0, 15);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(13.0)));

      // These time steps all exist.
      for(size_t i = 17; i <= 19; i += 2) {
        address.setCoordinate<size_t>(0, i);
        source.read(*raster, address);
        BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
        BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(i)));
      }

      // Outside of data source's data space.
      address.setCoordinate<size_t>(0, 21);
      source.read(*raster, address);
      BOOST_CHECK(raster->allMV());
    }

    // -------------------------------------------------------------------------
    // Read data in table: time series.
    {
      Table table;
      table.appendCol("", TI_REAL4);

      DataSpaceAddress address(2);
      address.setCoordinate<SpatialCoordinate>(1,
         SpatialCoordinate(100025.0, 199975.0));
      DataSpace iterSpace(source.dataSpace(), address);
      iterSpace.dimension(0) = source.dataSpace().dimension(0);

      source.read(table, iterSpace, address);

      BOOST_REQUIRE_EQUAL(table.nrRecs(), size_t(5));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], REAL4(11.0)));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[1], REAL4(13.0)));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[2]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[2], REAL4(13.0)));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[3]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[3], REAL4(17.0)));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[4]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[4], REAL4(19.0)));
    }
  }

  bool emptyDataSpaceTestCreated = false;
  BOOST_WARN(emptyDataSpaceTestCreated);
}


BOOST_AUTO_TEST_CASE(dataset_1_quantiles)
{
  using namespace dal;

  // Data space with quantiles.
  {
    std::string datasetName = (boost::filesystem::path("dataset1")
         / "aap" / "quantile_10").string();

    DataSpace space;
    std::vector<float> quantiles;
    quantiles.push_back(float(0.01));
    quantiles.push_back(float(0.99));
    quantiles.push_back(float(0.01));
    space.addDimension(Dimension(CumulativeProbabilities, quantiles));

    DataSource source(datasetName, space);
    BOOST_CHECK_EQUAL(source.name(), datasetName);

    {
      DataSpace realSpace;
      std::vector<float> quantiles;
      quantiles.push_back(float(0.1));
      quantiles.push_back(float(0.9));
      quantiles.push_back(float(0.01));
      realSpace.addDimension(Dimension(CumulativeProbabilities, quantiles));
      BOOST_CHECK(source.enclosingDataSpace() == realSpace);
    }

    BOOST_CHECK_EQUAL(source.unitDataSpace().rank(), size_t(1));
    BOOST_CHECK(source.unitDataSpace().hasRaster());

    // -------------------------------------------------------------------------
    // Read data in raster.
    {
      boost::shared_ptr<Raster> raster(source.open<Raster>());
      BOOST_CHECK(raster);
      raster->createCells();

      DataSpaceAddress address(1);

      address.setCoordinate<float>(0, 0.1f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.0)));

      address.setCoordinate<float>(0, 0.25f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(4.0)));

      address.setCoordinate<float>(0, 0.5f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(5.0)));

      address.setCoordinate<float>(0, 0.75f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));

      address.setCoordinate<float>(0, 0.9f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(10.0)));

      // Quantile does not exist. Outside valid range.
      address.setCoordinate<float>(0, 0.09f);
      source.read(*raster, address);
      BOOST_CHECK(raster->allMV());

      // Quantile does not exist. Outside valid range.
      address.setCoordinate<float>(0, 0.91f);
      source.read(*raster, address);
      BOOST_CHECK(raster->allMV());

      // Quantile does not exist. Inside valid range.
      address.setCoordinate<float>(0, 0.2f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      REAL4 value;
      interpolate(value, REAL4(0.0), REAL4(0.1), REAL4(4.0), REAL4(0.05));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), value));

      // Test the retrieval of quantiles given a data value.
      address.unsetCoordinate(0);
      source.read(*raster, REAL4(5.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.5)));

      source.read(*raster, REAL4(4.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.25)));

      source.read(*raster, REAL4(0.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.1)));

      source.read(*raster, REAL4(10.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.9)));

      // Data value not present, within data range, must be interpolated.
      source.read(*raster, REAL4(3.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      interpolate(value, REAL4(0.25), REAL4(1), REAL4(0.1), REAL4(3.0));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), value));

      // Outside data range. Left tail: 0.0
      source.read(*raster, REAL4(-1.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.0)));

      // Outside data range. Right tail: 1.0
      source.read(*raster, REAL4(10.1), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    }

    // -------------------------------------------------------------------------
    // Read data in table: cumulative probability plot.
    {
      Table table;
      table.appendCol("", TI_REAL4);

      DataSpaceAddress address(2);
      address.setCoordinate<SpatialCoordinate>(1,
         SpatialCoordinate(100025.0, 199975.0));
      DataSpace iterSpace(source.dataSpace(), address);
      iterSpace.dimension(0) = source.dataSpace().dimension(0);

      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(81));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], REAL4(0.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[15]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[15], REAL4(4.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[40]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[40], REAL4(5.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[65]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[65], REAL4(6.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[80]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[80], REAL4(10.0)));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[10]));
      REAL4 value;
      interpolate(value, REAL4(0.0), REAL4(0.1), REAL4(4.0), REAL4(0.05));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[10], value));
    }
  }

  // Data space with quantiles and time.
  {
    std::string datasetName = (boost::filesystem::path("dataset1")
         / "aap" / "quantile").string();

    DataSpace space;
    std::vector<size_t> timeSteps;
    timeSteps.push_back(size_t(9));
    timeSteps.push_back(size_t(21));
    timeSteps.push_back(size_t(1));
    std::vector<float> quantiles;
    quantiles.push_back(float(0.01));
    quantiles.push_back(float(0.99));
    quantiles.push_back(float(0.01));
    space.addDimension(Dimension(Time, timeSteps));
    space.addDimension(Dimension(CumulativeProbabilities, quantiles));

    DataSource source(datasetName, space);
    BOOST_CHECK_EQUAL(source.name(), datasetName);

    {
      DataSpace realSpace;
      std::vector<size_t> timeSteps;
      timeSteps.push_back(size_t(10));
      timeSteps.push_back(size_t(20));
      timeSteps.push_back(size_t(1));
      std::vector<float> quantiles;
      quantiles.push_back(float(0.1));
      quantiles.push_back(float(0.9));
      quantiles.push_back(float(0.01));
      realSpace.addDimension(Dimension(Time, timeSteps));
      realSpace.addDimension(Dimension(CumulativeProbabilities, quantiles));
    }

    BOOST_CHECK_EQUAL(source.unitDataSpace().rank(), size_t(1));
    BOOST_CHECK(source.unitDataSpace().hasRaster());

    // -------------------------------------------------------------------------
    // Read data in raster.
    {
      boost::shared_ptr<Raster> raster(source.open<Raster>());
      BOOST_CHECK(raster);
      raster->createCells();

      DataSpaceAddress address(2);

      address.setCoordinate<size_t>(0, 10);
      address.setCoordinate<float>(1, 0.1f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.0)));

      address.setCoordinate<size_t>(0, 15);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));

      address.setCoordinate<size_t>(0, 20);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(2.0)));

      address.setCoordinate<float>(1, 0.25f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));

      address.setCoordinate<size_t>(0, 15);
      address.setCoordinate<float>(1, 0.5f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));

      address.setCoordinate<float>(1, 0.6f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      REAL4 value;
      interpolate(value, REAL4(6.0), REAL4(0.1), REAL4(7.0), REAL4(0.15));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), value));

      address.setCoordinate<size_t>(0, 16);
      address.setCoordinate<float>(1, 0.5f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(6.0)));

      address.setCoordinate<size_t>(0, 14);
      address.setCoordinate<float>(1, 0.5f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(5.0)));

      // Test the retrieval of quantiles given a data value.
      address.setCoordinate<size_t>(0, 15);
      address.unsetCoordinate(1);
      source.read(*raster, REAL4(5.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.25)));

      source.read(*raster, REAL4(1.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.1)));

      source.read(*raster, REAL4(11.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.9)));

      address.setCoordinate<size_t>(0, 14);
      source.read(*raster, REAL4(5.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.5)));

      address.setCoordinate<size_t>(0, 21);
      source.read(*raster, REAL4(6.0), address);
      BOOST_CHECK(pcr::isMV(raster->cell<REAL4>(0)));
      // BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      // BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.25)));

      address.setCoordinate<size_t>(0, 9);
      source.read(*raster, REAL4(6.0), address);
      BOOST_CHECK(pcr::isMV(raster->cell<REAL4>(0)));

      address.setCoordinate<size_t>(0, 15);
      source.read(*raster, REAL4(8.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      interpolate(value, REAL4(0.75), REAL4(1.0), REAL4(0.9), REAL4(3.0));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), value));

      // Outside data range. Left tail: 0.0
      source.read(*raster, REAL4(-1.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.0)));

      // Outside data range. Right tail: 1.0
      source.read(*raster, REAL4(11.1), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    }

    // -------------------------------------------------------------------------
    // Read data in table: cumulative probability plot.
    {
      Table table;
      table.appendCol("", TI_REAL4);

      // Wide cumulative probabilities.
      DataSpaceAddress address(3);
      address.setCoordinate<size_t>(0, 15);
      address.setCoordinate<SpatialCoordinate>(2,
         SpatialCoordinate(100025.0, 199975.0));
      DataSpace iterSpace(source.dataSpace(), address);
      iterSpace.dimension(1) = source.dataSpace().dimension(1);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(81));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], REAL4(1.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[15]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[15], REAL4(5.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[40]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[40], REAL4(6.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[65]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[65], REAL4(7.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[80]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[80], REAL4(11.0)));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[10]));
      REAL4 value;
      interpolate(value, REAL4(1.0), REAL4(0.1), REAL4(5.0), REAL4(0.05));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[10], value));

      // Data for this time step does not exist, fall back to previous one.
      address.setCoordinate<size_t>(0, 14);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(81));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], REAL4(0.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[15]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[15], REAL4(4.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[40]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[40], REAL4(5.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[65]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[65], REAL4(6.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[80]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[80], REAL4(10.0)));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[10]));
      interpolate(value, REAL4(0.0), REAL4(0.1), REAL4(4.0), REAL4(0.05));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[10], value));

      // Data for this time step does not exist, fall back to previous one.
      address.setCoordinate<size_t>(0, 21);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(81));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[15]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[40]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[65]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[80]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[10]));

      // Data for this time step does not exist.
      address.setCoordinate<size_t>(0, 9);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(81));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[15]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[40]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[65]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[80]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[10]));
    }

    // -------------------------------------------------------------------------
    // Read data in table: timeseries
    {
      Table table;
      table.appendCol("", TI_REAL4);

      // Wide time dimension.
      DataSpaceAddress address(3);
      address.setCoordinate<float>(1, 0.75f);
      address.setCoordinate<SpatialCoordinate>(2,
         SpatialCoordinate(100025.0, 199975.0));
      DataSpace iterSpace(source.dataSpace(), address);
      iterSpace.dimension(0) = source.dataSpace().dimension(0);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(11));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], REAL4(6.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[1], REAL4(6.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[4]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[4], REAL4(6.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[5]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[5], REAL4(7.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[6]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[6], REAL4(7.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[9]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[9], REAL4(7.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[10]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[10], REAL4(8.0)));

      address.setCoordinate<float>(1, 0.60f);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(11));
      REAL4 value;
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      interpolate(value, REAL4(5.0), REAL4(0.1), REAL4(6.0), REAL4(0.15));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], value));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[1], value));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[4]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[4], value));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[5]));
      interpolate(value, REAL4(6.0), REAL4(0.1), REAL4(7.0), REAL4(0.15));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[5], value));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[9]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[9], value));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[10]));
      interpolate(value, REAL4(7.0), REAL4(0.1), REAL4(8.0), REAL4(0.15));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[10], value));

      address.setCoordinate<float>(1, 0.09f);
      table.clear();
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(11));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[2]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[3]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[4]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[5]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[6]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[7]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[8]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[9]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[10]));

      address.setCoordinate<float>(1, 0.91f);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(11));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[2]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[3]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[4]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[5]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[6]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[7]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[8]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[9]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[10]));

      // Test the retrieval of quantiles given a data value.
      address.unsetCoordinate(1);
      source.read<float>(table, 5.0f, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(11));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], REAL4(0.5)));

      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[2]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[3]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[4]));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[5]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[5], REAL4(0.25)));

      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[6]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[7]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[8]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[9]));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[10]));
      interpolate(value, REAL4(0.1), REAL4(3.0), REAL4(0.25), REAL4(1.0));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[10], value));
    }
  }

  // Data space with scenarios, quantiles and time.
  {
    std::string datasetName = "quantile";

    DataSpace space;

    std::set<std::string> scenarios;
    std::string aap(
         (boost::filesystem::path("dataset1") / "aap").string());
    std::string noot(
         (boost::filesystem::path("dataset1") / "noot").string());
    std::string mies(
         (boost::filesystem::path("dataset1") / "mies").string());
    std::string teun(
         (boost::filesystem::path("dataset1") / "teun").string());
    scenarios.insert(aap);
    scenarios.insert(noot);
    scenarios.insert(mies);
    scenarios.insert(teun);
    std::vector<size_t> timeSteps;
    timeSteps.push_back(size_t(9));
    timeSteps.push_back(size_t(21));
    timeSteps.push_back(size_t(1));
    std::vector<float> quantiles;
    quantiles.push_back(float(0.01));
    quantiles.push_back(float(0.99));
    quantiles.push_back(float(0.01));
    space.addDimension(Dimension(Scenarios, scenarios));
    space.addDimension(Dimension(Time, timeSteps));
    space.addDimension(Dimension(CumulativeProbabilities, quantiles));

    DataSource source(datasetName, space);
    BOOST_CHECK_EQUAL(source.name(), datasetName);

    {
      std::set<std::string> scenarios;
      std::string aap(
           (boost::filesystem::path("dataset1") / "aap").string());
      std::string noot(
           (boost::filesystem::path("dataset1") / "noot").string());
      std::string mies(
           (boost::filesystem::path("dataset1") / "mies").string());
      scenarios.insert(aap);
      scenarios.insert(mies);
      scenarios.insert(noot);
      std::vector<size_t> timeSteps;
      timeSteps.push_back(size_t(10));
      timeSteps.push_back(size_t(20));
      timeSteps.push_back(size_t(1));
      std::vector<float> quantiles;
      quantiles.push_back(float(0.1));
      quantiles.push_back(float(0.9));
      quantiles.push_back(float(0.01));
      DataSpace realSpace;
      realSpace.addDimension(Dimension(Scenarios, scenarios));
      realSpace.addDimension(Dimension(Time, timeSteps));
      realSpace.addDimension(Dimension(CumulativeProbabilities, quantiles));
      BOOST_CHECK(source.enclosingDataSpace() == realSpace);
    }

    BOOST_CHECK_EQUAL(source.unitDataSpace().rank(), size_t(1));
    BOOST_CHECK(source.unitDataSpace().hasRaster());

    // -------------------------------------------------------------------------
    // Read data in raster.
    {
      boost::shared_ptr<Raster> raster(source.open<Raster>());
      BOOST_CHECK(raster);
      raster->createCells();

      DataSpaceAddress address(3);

      address.setCoordinate<std::string>(0, noot);
      address.setCoordinate<size_t>(1, 15);
      address.setCoordinate<float>(2, 0.5f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(16.0)));

      address.setCoordinate<std::string>(0, mies);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(26.0)));

      address.setCoordinate<size_t>(1, 19);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(26.0)));

      address.setCoordinate<size_t>(1, 20);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(27.0)));

      address.setCoordinate<size_t>(1, 21);
      source.read(*raster, address);
      BOOST_CHECK(pcr::isMV(raster->cell<REAL4>(0)));

      address.setCoordinate<size_t>(1, 9);
      source.read(*raster, address);
      BOOST_CHECK(pcr::isMV(raster->cell<REAL4>(0)));

      address.setCoordinate<size_t>(1, 10);
      address.setCoordinate<float>(2, 0.6f);
      source.read(*raster, address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      REAL4 value;
      interpolate(value, REAL4(25.0), REAL4(0.1), REAL4(26.0), REAL4(0.15));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), value));

      // Test the retrieval of quantiles given a data value.
      address.setCoordinate<std::string>(0, mies);
      address.setCoordinate<size_t>(1, 20);
      address.unsetCoordinate(2);
      source.read(*raster, REAL4(28.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.75)));

      source.read(*raster, REAL4(22.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.1)));

      source.read(*raster, REAL4(32.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.9)));

      source.read(*raster, REAL4(21.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(0.0)));

      source.read(*raster, REAL4(33.0), address);
      BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
      BOOST_CHECK(comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    }

    // -------------------------------------------------------------------------
    // Read data in table: cumulative probability plot.
    {
      Table table;
      table.appendCol("", TI_REAL4);

      // Wide cumulative probabilities.
      DataSpaceAddress address(4);
      address.setCoordinate<std::string>(0, aap);
      address.setCoordinate<size_t>(1, 15);
      address.setCoordinate<SpatialCoordinate>(3,
         SpatialCoordinate(100025.0, 199975.0));
      DataSpace iterSpace(source.dataSpace(), address);
      iterSpace.dimension(2) = source.dataSpace().dimension(2);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(81));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], REAL4(1.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[15]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[15], REAL4(5.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[40]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[40], REAL4(6.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[65]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[65], REAL4(7.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[80]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[80], REAL4(11.0)));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[10]));
      REAL4 value;
      interpolate(value, REAL4(1.0), REAL4(0.1), REAL4(5.0), REAL4(0.05));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[10], value));

      // Data for this time step does not exist, fall back to previous one.
      address.setCoordinate<size_t>(1, 14);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(81));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], REAL4(0.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[15]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[15], REAL4(4.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[40]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[40], REAL4(5.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[65]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[65], REAL4(6.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[80]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[80], REAL4(10.0)));

      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[10]));
      interpolate(value, REAL4(0.0), REAL4(0.1), REAL4(4.0), REAL4(0.05));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[10], value));

      // Time step outside of data space. All missing values.
      iterSpace.dimension(0).setValue<std::string>(noot);
      address.setCoordinate<std::string>(0, noot);
      address.setCoordinate<size_t>(1, 21);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(81));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[15]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[40]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[65]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[80]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[10]));

      // Data for this time step does not exist.
      address.setCoordinate<size_t>(1, 9);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(81));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[15]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[40]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[65]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[80]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[10]));
    }

    // -------------------------------------------------------------------------
    // Read data in table: timeseries
    {
      Table table;
      table.appendCol("", TI_REAL4);

      // Wide time dimension.
      DataSpaceAddress address(4);
      address.setCoordinate<std::string>(0, mies);
      address.setCoordinate<float>(2, 0.75f);
      address.setCoordinate<SpatialCoordinate>(3,
         SpatialCoordinate(100025.0, 199975.0));
      DataSpace iterSpace(source.dataSpace(), address);
      iterSpace.dimension(1) = source.dataSpace().dimension(1);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(11));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], REAL4(26.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[1], REAL4(26.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[4]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[4], REAL4(26.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[5]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[5], REAL4(27.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[6]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[6], REAL4(27.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[9]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[9], REAL4(27.0)));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[10]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[10], REAL4(28.0)));

      address.setCoordinate<float>(2, 0.60f);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(11));
      REAL4 value;
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[0]));
      interpolate(value, REAL4(25.0), REAL4(0.1), REAL4(26.0), REAL4(0.15));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[0], value));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[1], value));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[4]));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[4], value));
      BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[5]));
      interpolate(value, REAL4(26.0), REAL4(0.1), REAL4(27.0), REAL4(0.15));
      BOOST_CHECK(comparable(table.col<REAL4>(0)[5], value));

      // BOOST_CHECK(!pcr::isMV(table.col<REAL4>(0)[12]));
      // interpolate(value, REAL4(27.0), REAL4(0.1), REAL4(28.0), REAL4(0.15));
      // BOOST_CHECK(comparable(table.col<REAL4>(0)[12], value));

      iterSpace.dimension(0).setValue<std::string>(aap);
      address.setCoordinate<std::string>(0, aap);
      address.setCoordinate<float>(2, 0.09f);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(11));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[2]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[3]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[4]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[5]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[6]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[7]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[8]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[9]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[10]));

      address.setCoordinate<float>(2, 0.91f);
      source.read(table, iterSpace, address);

      BOOST_CHECK_EQUAL(table.nrRecs(), size_t(11));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[0]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[1]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[2]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[3]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[4]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[5]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[6]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[7]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[8]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[9]));
      BOOST_CHECK(pcr::isMV(table.col<REAL4>(0)[10]));
    }
  }
}


BOOST_AUTO_TEST_CASE(uncertain_temporal_feature_layer)
{
  using namespace dal;

  BOOST_WARN(false);
}

BOOST_AUTO_TEST_SUITE_END()
