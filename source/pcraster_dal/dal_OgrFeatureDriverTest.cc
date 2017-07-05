#define BOOST_TEST_MODULE pcraster dal ogr_feature_driver
#include <boost/test/unit_test.hpp>
#include <ogr_geometry.h>
#include "dal_Exception.h"
#include "dal_Library.h"
#include "dal_OgrFeatureDriver.h"
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
        static ClientWrapper client("/my/path/ogr_feature_driver_test", true);
    }

    ~Fixture()
    {
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(exists)
{
  using namespace dal;

  std::string name;
  OgrFeatureDriver driver("ESRI Shapefile");
  return;

  // Unexisting feature data set.
  {
    driver.datasetProperties().clear();
    name = "unexisting";
    BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists(name));
  }

  // Existing feature data set. No layer selected.
  {
    driver.datasetProperties().clear();
    name = "points.shp//id";
    BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists(name));
  }

  // Existing feature data set. Attribute selected.
  {
    driver.datasetProperties().clear();
    name = "points.shp/points/id";
    BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name));

    // No extension.
    driver.datasetProperties().clear();
    name = "points/points/id";
    BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name));
  }

  // Existing feature data set. Only geometry selected.
  {
    driver.datasetProperties().clear();
    name = "points.shp/points";
    BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name));

    driver.datasetProperties().clear();
    name = "points.shp/points/";
    BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name));
  }

  // Existing feature data set. Path includes directory specifier.
  // Directory is name of data source, shapefile name is name of layer, etc.
  {
    driver.datasetProperties().clear();
    // source   : .
    // layer    : points.shp
    // attribute: points
    // layer name is false.
    name = "./points.shp/concentrat";
    BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists(name));

    // source   : .
    // layer    : points
    // attribute: concentrat
    // layer name is fine.
    driver.datasetProperties().clear();
    name = "./points/concentrat";
    BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name));

    // source   : .
    // layer    : points
    // attribute: -
    // Selects the geometry.
    driver.datasetProperties().clear();
    name = "./points";
    BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name));

    driver.datasetProperties().clear();
    name = "./points/points";
    BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists(name));
  }
}


/// BOOST_AUTO_TEST_CASE(open)
/// {
///   using namespace dal;
///
///   std::string name;
///   boost::shared_ptr<FeatureLayer> layer;
///   OgrFeatureDriver driver("ESRI Shapefile");
///
///   // Unexisting feature data set.
///   {
///     driver.datasetProperties().clear();
///     name = "unexisting";
///     BOOST_CHECK(Client::library().geometriesCache().empty());
///     layer.reset(dynamic_cast<FeatureLayer*>(
///            dynamic_cast<Driver&>(driver).open(name)));
///     BOOST_CHECK(!layer);
///     BOOST_CHECK(Client::library().geometriesCache().empty());
///   }
///
///   {
///     driver.datasetProperties().clear();
///     name = "points.shp/points/blablabla";
///     layer.reset(dynamic_cast<FeatureLayer*>(
///            dynamic_cast<Driver&>(driver).open(name)));
///     BOOST_CHECK(!layer);
///     BOOST_CHECK(Client::library().geometriesCache().empty());
///   }
///
///   // Existing feature data set.
///   {
///     driver.datasetProperties().clear();
///     name = "points.shp/points";
///     layer.reset(dynamic_cast<FeatureLayer*>(
///            dynamic_cast<Driver&>(driver).open(name)));
///     BOOST_CHECK_EQUAL(Client::library().geometriesCache().size(), size_t(1));
///     BOOST_REQUIRE(layer);
///     BOOST_CHECK(!layer->hasAttribute());
///     BOOST_CHECK_EQUAL(layer->typeId(), TI_NR_TYPES);
///
///     BOOST_CHECK_CLOSE(layer->dimensions().west(), -0.900337, 0.001);
///     BOOST_CHECK_CLOSE(layer->dimensions().south(), 0.382997, 0.001);
///     BOOST_CHECK_CLOSE(layer->dimensions().east(), -0.322896, 0.001);
///     BOOST_CHECK_CLOSE(layer->dimensions().north(), 0.870875, 0.001);
///   }
///
///   {
///     driver.datasetProperties().clear();
///     name = "points.shp/points/id";
///     layer.reset(dynamic_cast<FeatureLayer*>(
///            dynamic_cast<Driver&>(driver).open(name)));
///     BOOST_REQUIRE(layer);
///     BOOST_CHECK(layer->hasAttribute());
///     BOOST_CHECK_EQUAL(layer->typeId(), TI_INT4);
///   }
///
///   {
///     driver.datasetProperties().clear();
///     name = "points.shp/points/name";
///     layer.reset(dynamic_cast<FeatureLayer*>(
///            dynamic_cast<Driver&>(driver).open(name)));
///     BOOST_REQUIRE(layer);
///     BOOST_CHECK(layer->hasAttribute());
///     BOOST_CHECK_EQUAL(layer->typeId(), TI_STRING);
///   }
///
///   {
///     driver.datasetProperties().clear();
///     name = "points.shp/points/concentrat";
///     layer.reset(dynamic_cast<FeatureLayer*>(
///            dynamic_cast<Driver&>(driver).open(name)));
///     BOOST_REQUIRE(layer);
///     BOOST_CHECK(layer->hasAttribute());
///     BOOST_CHECK_EQUAL(layer->typeId(), TI_REAL8);
///   }
/// }
///
///
/// BOOST_AUTO_TEST_CASE(data_space)
/// {
///   using namespace dal;
///
///   std::string name;
///   DataSpace space;
///   OgrFeatureDriver driver("ESRI Shapefile");
///
///   // Existing feature data set.
///   {
///     driver.datasetProperties().clear();
///     name = "points.shp/points";
///     BOOST_REQUIRE_NO_THROW(
///       space = dynamic_cast<Driver&>(driver).dataSpace(name);
///     )
///     BOOST_CHECK_EQUAL(space.size(), size_t(1));
///
///     Dimension dimension(space.dimension(0));
///     BOOST_CHECK_EQUAL(dimension.meaning(), Space);
///     BOOST_CHECK_EQUAL(dimension.nrValues(), size_t(1));
///     BOOST_CHECK_EQUAL(dimension.coordinateType(), NumericalCoordinates);
///     BOOST_CHECK_EQUAL(dimension.discretisation(), BorderedDiscretisation);
///
///     SpaceDimensions const& spaceDimensions(
///          space.dimension(0).value<SpaceDimensions>(0));
///     BOOST_CHECK_CLOSE(spaceDimensions.west(), -0.900337, 0.001);
///     BOOST_CHECK_CLOSE(spaceDimensions.north(), 0.870875, 0.001);
///     BOOST_CHECK_CLOSE(spaceDimensions.east(), -0.322896, 0.001);
///     BOOST_CHECK_CLOSE(spaceDimensions.south(), 0.382997, 0.001);
///   }
/// }
///
///
/// BOOST_AUTO_TEST_CASE(read_)
/// {
///   using namespace dal;
///
///   std::string name;
///   boost::shared_ptr<FeatureLayer> layer;
///   OgrFeatureDriver driver("ESRI Shapefile");
///
///   {
///     driver.datasetProperties().clear();
///     name = "unexisting";
///     bool exceptionCaught;
///
///     try {
///       exceptionCaught = false;
///       layer.reset(dynamic_cast<FeatureDriver&>(driver).read(name));
///     }
///     catch(Exception& exception) {
///       BOOST_CHECK_EQUAL(exception.message(),
///            "Data source " + name + "(feature):\ncannot be opened");
///       exceptionCaught = true;
///     }
///
///     BOOST_CHECK(exceptionCaught);
///   }
///
///   {
///     driver.datasetProperties().clear();
///     name = "points.shp/points";
///     BOOST_REQUIRE_NO_THROW(
///          layer.reset(dynamic_cast<FeatureDriver&>(driver).read(name))
///     );
///
///     BOOST_REQUIRE(layer);
///     BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(3));
///
///     std::vector<long int> featureIds;
///     layer->featureIds(std::back_inserter(featureIds));
///
///     BOOST_FOREACH(long int featureId, featureIds) {
///       OGRGeometry const& geometry(layer->geometry(featureId));
///       BOOST_CHECK_EQUAL(geometry.getGeometryType(), wkbPoint);
///     }
///   }
///
///   {
///     driver.datasetProperties().clear();
///     name = "points.shp/points/concentrat";
///     BOOST_REQUIRE_NO_THROW(
///          layer.reset(dynamic_cast<FeatureDriver&>(driver).read(name))
///     );
///
///     BOOST_REQUIRE(layer);
///     BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(3));
///
///     std::vector<long int> featureIds;
///     layer->featureIds(std::back_inserter(featureIds));
///
///     BOOST_FOREACH(long int featureId, featureIds) {
///       OGRGeometry const& geometry(layer->geometry(featureId));
///       BOOST_CHECK_EQUAL(geometry.getGeometryType(), wkbPoint);
///     }
///   }
/// }
///
///
/// BOOST_AUTO_TEST_CASE(query)
/// {
///   using namespace dal;
///
///   OgrFeatureDriver driver("ESRI Shapefile");
///   DataSpaceQueryResult result;
///
///   driver.datasetProperties().clear();
///
///   BOOST_REQUIRE_NO_THROW(
///          result = driver.search("points.shp/points", DataSpace(),
///               SearchForAllItems);
///          BOOST_CHECK(result);
///   );
///
///   driver.datasetProperties().clear();
///
///   BOOST_REQUIRE_NO_THROW(
///          result = driver.search("lines.shp/lines/distance", DataSpace(),
///               SearchForAllItems);
///          BOOST_CHECK(result);
///   );
///
///   driver.datasetProperties().clear();
///
///   BOOST_REQUIRE_NO_THROW(
///          result = driver.search("doesnotexist.shp/bla/bli", DataSpace(),
///               SearchForAllItems);
///          BOOST_CHECK(!result);
///   );
/// }
///
///
/// BOOST_AUTO_TEST_CASE(attributes)
/// {
///   using namespace dal;
///
///   OgrFeatureDriver driver("VRT");
///   boost::shared_ptr<FeatureLayer> layer(
///          dynamic_cast<FeatureDriver&>(driver).read("polygons.vrt/polygons/id"));
///
///   BOOST_REQUIRE(layer);
///   BOOST_REQUIRE(layer->hasAttribute());
///   BOOST_CHECK_EQUAL(layer->name(), "id");
///   BOOST_CHECK_EQUAL(layer->typeId(), TI_INT4);
///   BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(2));
///
///   INT4 value;
///
///   std::vector<long int> featureIds;
///   layer->featureIds(std::back_inserter(featureIds));
///
///   BOOST_FOREACH(long int featureId, featureIds) {
///     OGRGeometry const& geometry(layer->geometry(featureId));
///     BOOST_CHECK_EQUAL(geometry.getGeometryType(), wkbPolygon);
///
///     layer->value<INT4>(featureId, value);
///     BOOST_CHECK_EQUAL(value, featureId);
///   }
/// }
///
///
/// BOOST_AUTO_TEST_CASE(space)
/// {
///   using namespace dal;
///
///   // Test reading a spatial attribute.
///
///   std::string name = "polygons/polygons/attribute1";
///
///   OgrFeatureDriver driver("VRT");
///   boost::shared_ptr<FeatureLayer> layer;
///
///   BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name));
///   BOOST_REQUIRE_NO_THROW(
///     layer.reset(dynamic_cast<FeatureDriver&>(driver).read(name));
///   )
///
///   BOOST_REQUIRE(layer);
///   BOOST_REQUIRE(layer->hasAttribute());
///   BOOST_CHECK_EQUAL(layer->name(), "attribute1");
///   BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(2));
///
///   BOOST_REQUIRE_EQUAL(layer->typeId(), TI_REAL8);
///   REAL8 value;
///
///   layer->value<REAL8>(1, value);
///   BOOST_CHECK_CLOSE(value, REAL8(1.1), 0.001);
///
///   layer->value<REAL8>(2, value);
///   BOOST_CHECK_CLOSE(value, REAL8(2.1), 0.001);
/// }
///
///
/// BOOST_AUTO_TEST_CASE(time_)
/// {
///   using namespace dal;
///
///   // Test reading a temporal attribute.
///
///   std::string name = "polygons/polygons/attribute1";
///
///   DataSpace space;
///   space.addDimension(Dimension(Time, size_t(1), size_t(10), size_t(1)));
///
///   DataSpaceAddress address(space.address());
///   address.setCoordinate<size_t>(0, 1);
///
///   OgrFeatureDriver driver("VRT");
///   boost::shared_ptr<FeatureLayer> layer;
///
///   BOOST_WARN(dynamic_cast<Driver&>(driver).exists(name, space, address));
///   // BOOST_REQUIRE(dynamic_cast<Driver&>(driver).exists(name, space, address));
///
///   // BOOST_REQUIRE_NO_THROW(
///   //   layer.reset(dynamic_cast<FeatureDriver&>(driver).read(name, space, address,
///   //        TI_REAL8));
///   // )
///
///   // BOOST_REQUIRE(layer);
///   // BOOST_REQUIRE(layer->hasAttribute());
///   // BOOST_CHECK_EQUAL(layer->name(), "attribute1");
///   // BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(2));
///
///   // REAL8 value;
///
///   // layer->value<REAL8>(1, value);
///   // BOOST_CHECK_CLOSE(value, REAL8(11.1), 0.001);
///
///   // layer->value<REAL8>(2, value);
///   // BOOST_CHECK_CLOSE(value, REAL8(12.1), 0.001);
///
///   // // Read another time step in the existing layer.
///   // address.setCoordinate<size_t>(0, 2);
///   // BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name, space, address));
///   // BOOST_REQUIRE_NO_THROW(
///   //   dynamic_cast<FeatureDriver&>(driver).read(*layer, name, space, address);
///   // )
///
///   // BOOST_REQUIRE(layer->hasAttribute());
///   // BOOST_CHECK_EQUAL(layer->name(), "attribute1");
///   // BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(2));
///
///   // layer->value<REAL8>(1, value);
///   // BOOST_CHECK_CLOSE(value, REAL8(11.2), 0.001);
///
///   // layer->value<REAL8>(2, value);
///   // BOOST_CHECK_CLOSE(value, REAL8(12.2), 0.001);
///
///   // // Read an unexisting time step in the existing layer.
///   // address.setCoordinate<size_t>(0, 3);
///   // BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists(name, space, address));
///   // BOOST_CHECK_THROW(
///   //   dynamic_cast<FeatureDriver&>(driver).read(*layer, name, space, address),
///   //   Exception
///   // )
/// }
///
///
/// BOOST_AUTO_TEST_CASE(uncertainty)
/// {
///   using namespace dal;
///
///   // Test reading an uncertain attribute.
///
///   std::string name = "polygons/polygons/attribute1";
///
///   std::vector<float> quantiles;
///   quantiles.push_back(0.01f);
///   quantiles.push_back(0.99f);
///   quantiles.push_back(0.01f);
///
///   DataSpace space;
///   space.addDimension(Dimension(CumulativeProbabilities, quantiles));
///
///   DataSpaceAddress address(space.address());
///   address.setCoordinate<float>(0, 0.01f);
///
///   OgrFeatureDriver driver("VRT");
///   boost::shared_ptr<FeatureLayer> layer;
///
///   // KDJ 20090403 - Will be fixed soon.
///   BOOST_WARN(dynamic_cast<Driver&>(driver).exists(name, space, address));
///   /// BOOST_REQUIRE_NO_THROW(
///   ///   layer.reset(dynamic_cast<FeatureDriver&>(driver).read(name, space, address,
///   ///        TI_REAL8));
///   /// )
///
///   /// BOOST_REQUIRE(layer);
///   /// BOOST_REQUIRE(layer->hasAttribute());
///   /// BOOST_CHECK_EQUAL(layer->name(), "attribute1");
///   /// BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(2));
///
///   /// REAL8 value;
///
///   /// layer->value<REAL8>(1, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(3.3), 0.001);
///
///   /// layer->value<REAL8>(2, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(3.1), 0.001);
///
///   /// // Read another quantile in the existing layer.
///   /// address.setCoordinate<float>(0, 0.5f);
///   /// BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name, space, address));
///   /// BOOST_REQUIRE_NO_THROW(
///   ///   dynamic_cast<FeatureDriver&>(driver).read(*layer, name, space, address);
///   /// )
///
///   /// BOOST_REQUIRE(layer->hasAttribute());
///   /// BOOST_CHECK_EQUAL(layer->name(), "attribute1");
///   /// BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(2));
///
///   /// layer->value<REAL8>(1, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(10.3), 0.001);
///
///   /// layer->value<REAL8>(2, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(11.3), 0.001);
///
///   /// // Read an unexisting quantile in the existing layer.
///   /// address.setCoordinate<float>(0, 0.6f);
///   /// BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists(name, space, address));
///   /// BOOST_CHECK_THROW(
///   ///   dynamic_cast<FeatureDriver&>(driver).read(*layer, name, space, address),
///   ///   Exception
///   /// )
/// }
///
///
/// BOOST_AUTO_TEST_CASE(uncertain_temporal)
/// {
///   using namespace dal;
///
///   // Test reading an uncertain temporal attribute.
///
///   std::string name = "polygons/polygons/attribute1";
///
///   std::vector<size_t> steps;
///   steps.push_back(1);
///   steps.push_back(10);
///   steps.push_back(1);
///
///   std::vector<float> quantiles;
///   quantiles.push_back(0.01f);
///   quantiles.push_back(0.99f);
///   quantiles.push_back(0.01f);
///
///   DataSpace space;
///   space.addDimension(Dimension(Time, steps));
///   space.addDimension(Dimension(CumulativeProbabilities, quantiles));
///
///   DataSpaceAddress address(space.address());
///   address.setCoordinate<size_t>(0, 1);
///   address.setCoordinate<float>(1, 0.01f);
///
///   OgrFeatureDriver driver("VRT");
///   boost::shared_ptr<FeatureLayer> layer;
///
///   // KDJ 20090403 - Will be fixed soon.
///   BOOST_WARN(dynamic_cast<Driver&>(driver).exists(name, space, address));
///   /// BOOST_REQUIRE_NO_THROW(
///   ///   layer.reset(dynamic_cast<FeatureDriver&>(driver).read(name, space, address,
///   ///        TI_REAL8));
///   /// )
///
///   /// BOOST_REQUIRE(layer);
///   /// BOOST_REQUIRE(layer->hasAttribute());
///   /// BOOST_CHECK_EQUAL(layer->name(), "attribute1");
///   /// BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(2));
///
///   /// REAL8 value;
///
///   /// layer->value<REAL8>(1, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(13.3), 0.001);
///
///   /// layer->value<REAL8>(2, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(13.1), 0.001);
///
///   /// // Read another quantile in the existing layer.
///   /// address.setCoordinate<float>(1, 0.5f);
///   /// BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name, space, address));
///   /// BOOST_REQUIRE_NO_THROW(
///   ///   dynamic_cast<FeatureDriver&>(driver).read(*layer, name, space, address);
///   /// )
///
///   /// BOOST_REQUIRE(layer->hasAttribute());
///   /// BOOST_CHECK_EQUAL(layer->name(), "attribute1");
///   /// BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(2));
///
///   /// layer->value<REAL8>(1, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(110.3), 0.001);
///
///   /// layer->value<REAL8>(2, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(111.3), 0.001);
///
///   /// // Read an unexisting quantile in the existing layer.
///   /// address.setCoordinate<float>(1, 0.6f);
///   /// BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists(name, space, address));
///   /// BOOST_CHECK_THROW(
///   ///   dynamic_cast<FeatureDriver&>(driver).read(*layer, name, space, address),
///   ///   Exception
///   /// )
///
///   /// // Read another time step in the existing layer.
///   /// address.setCoordinate<size_t>(0, 2);
///   /// address.setCoordinate<float>(1, 0.5f);
///   /// BOOST_CHECK(dynamic_cast<Driver&>(driver).exists(name, space, address));
///   /// BOOST_REQUIRE_NO_THROW(
///   ///   dynamic_cast<FeatureDriver&>(driver).read(*layer, name, space, address);
///   /// )
///
///   /// BOOST_REQUIRE(layer->hasAttribute());
///   /// BOOST_CHECK_EQUAL(layer->name(), "attribute1");
///   /// BOOST_CHECK_EQUAL(layer->nrGeometries(), size_t(2));
///
///   /// layer->value<REAL8>(1, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(210.3), 0.001);
///
///   /// layer->value<REAL8>(2, value);
///   /// BOOST_CHECK_CLOSE(value, REAL8(211.3), 0.001);
///
///   /// // Read an unexisting time step in the existing layer.
///   /// address.setCoordinate<size_t>(0, 3);
///   /// BOOST_CHECK(!dynamic_cast<Driver&>(driver).exists(name, space, address));
///   /// BOOST_CHECK_THROW(
///   ///   dynamic_cast<FeatureDriver&>(driver).read(*layer, name, space, address),
///   ///   Exception
///   /// )
/// }
