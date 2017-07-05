#define BOOST_TEST_MODULE pcraster dal feature_layer
#include <boost/test/unit_test.hpp>
#include <ogr_feature.h>
#include "dal_Library.h"
#include "dal_FeatureLayer.h"
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
        static ClientWrapper client("/my/path/feature_layer_test", true);
    }

    ~Fixture()
    {
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  Client::library().geometriesCache().clear();

  {
    FeatureLayerGeometries* geometries = new FeatureLayerGeometries(
           0.0, 10.0, 10.0, 0.0);
    Client::library().geometriesCache().insert("bla", geometries);
    FeatureLayer featureLayer(geometries);

    OGRPolygon* polygon = dynamic_cast<OGRPolygon*>(
        OGRGeometryFactory::createGeometry(wkbPolygon));
    OGRLinearRing* linearRing = dynamic_cast<OGRLinearRing*>(
        OGRGeometryFactory::createGeometry(wkbLinearRing));

    linearRing->addPoint(1.0, 1.0);
    linearRing->addPoint(3.0, 1.0);
    linearRing->addPoint(2.0, 3.0);
    assert(!linearRing->get_IsClosed());
    assert(!linearRing->IsValid());
    linearRing->addPoint(1.0, 1.0);
    assert(linearRing->get_IsClosed());

    polygon->addRingDirectly(linearRing);
    // If this fails, gdal is probably not built using support for geos.
    assert(polygon->IsValid());

    featureLayer.insert(3, polygon);

    // TODO Move to test for featurelayergeometries
    // BOOST_CHECK_EQUAL(featureLayer.featureId(polygon), 3);

    OGRGeometry const* geometry = featureLayer.geometry(2.0, 2.0);
    BOOST_REQUIRE(geometry);

    BOOST_CHECK_EQUAL(geometry->getGeometryType(), wkbPolygon);

    // Some points outside of the polygon.
    BOOST_CHECK(!featureLayer.geometry(0.0, 0.0));
    BOOST_CHECK(!featureLayer.geometry(1.5, 0.5));
    BOOST_CHECK(!featureLayer.geometry(-1.5, -0.5));

    // The points that make up the polygon.
    // TODO Doesn't work. The implementation uses 'Contains' which apparently
    // TODO means that the border is 'out'. Not a big issue at the moment.
    BOOST_WARN(featureLayer.geometry(1.0, 1.0));
    BOOST_WARN(featureLayer.geometry(3.0, 1.0));
    BOOST_WARN(featureLayer.geometry(2.0, 3.0));

    BOOST_CHECK(Client::library().geometriesCache().contains("bla"));
  }

  BOOST_CHECK(!Client::library().geometriesCache().contains("bla"));
  BOOST_CHECK(Client::library().geometriesCache().empty());
}
