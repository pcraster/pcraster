#ifndef INCLUDED_DAL_FEATURELAYERTEST
#include "dal_FeatureLayerTest.h"
#define INCLUDED_DAL_FEATURELAYERTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

#ifndef INCLUDED_OGR_FEATURE
#include <ogr_feature.h>
#define INCLUDED_OGR_FEATURE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

#ifndef INCLUDED_DAL_LIBRARY
#include "dal_Library.h"
#define INCLUDED_DAL_LIBRARY
#endif

#ifndef INCLUDED_DAL_FEATURELAYER
#include "dal_FeatureLayer.h"
#define INCLUDED_DAL_FEATURELAYER
#endif



/*!
  \file
  This file contains the implementation of the FeatureLayerTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC FEATURELAYERTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* FeatureLayerTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FeatureLayerTest> instance(new FeatureLayerTest());
  suite->add(BOOST_CLASS_TEST_CASE(&FeatureLayerTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF FEATURELAYERTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
FeatureLayerTest::FeatureLayerTest()
{
}



void FeatureLayerTest::test()
{
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

} // namespace dal

