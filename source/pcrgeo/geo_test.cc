#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif


#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif
#ifndef INCLUDED_GEO_ADDFILTERTEST
#include "geo_addfiltertest.h"
#define INCLUDED_GEO_ADDFILTERTEST
#endif

#ifndef INCLUDED_GEO_ALGORITHMTEST
#include "geo_algorithmtest.h"
#define INCLUDED_GEO_ALGORITHMTEST
#endif

#ifndef INCLUDED_GEO_ASSIGNFILTERTEST
#include "geo_assignfiltertest.h"
#define INCLUDED_GEO_ASSIGNFILTERTEST
#endif

#ifndef INCLUDED_GEO_AVERAGEFILTERTEST
#include "geo_averagefiltertest.h"
#define INCLUDED_GEO_AVERAGEFILTERTEST
#endif

#ifndef INCLUDED_GEO_CELLLOCVISITORTEST
#include "geo_celllocvisitortest.h"
#define INCLUDED_GEO_CELLLOCVISITORTEST
#endif

#ifndef INCLUDED_GEO_CIRCULARNEIGHBOURHOODTEST
#include "geo_circularneighbourhoodtest.h"
#define INCLUDED_GEO_CIRCULARNEIGHBOURHOODTEST
#endif

#ifndef INCLUDED_GEO_COUNTFILTERTEST
#include "geo_countfiltertest.h"
#define INCLUDED_GEO_COUNTFILTERTEST
#endif

#ifndef INCLUDED_GEO_CSFMAPTEST
#include "geo_csfmaptest.h"
#define INCLUDED_GEO_CSFMAPTEST
#endif

#ifndef INCLUDED_GEO_CSFSTACKNAMETEST
#include "geo_csfstacknametest.h"
#define INCLUDED_GEO_CSFSTACKNAMETEST
#endif

#ifndef INCLUDED_GEO_FILECREATETESTERTEST
#include "geo_filecreatetestertest.h"
#define INCLUDED_GEO_FILECREATETESTERTEST
#endif

#ifndef INCLUDED_GEO_FRACTIONFILTERTEST
#include "geo_fractionfiltertest.h"
#define INCLUDED_GEO_FRACTIONFILTERTEST
#endif

#ifndef INCLUDED_GEO_GRIDDEDPOINTSTEST
#include "geo_griddedpointstest.h"
#define INCLUDED_GEO_GRIDDEDPOINTSTEST
#endif

#ifndef INCLUDED_GEO_MOORENEIGHBOURHOODTEST
#include "geo_mooreneighbourhoodtest.h"
#define INCLUDED_GEO_MOORENEIGHBOURHOODTEST
#endif

#ifndef INCLUDED_GEO_NEIGHBOURHOODTEST
#include "geo_neighbourhoodtest.h"
#define INCLUDED_GEO_NEIGHBOURHOODTEST
#endif

#ifndef INCLUDED_GEO_RASTERBOUNDARIESTEST
#include "geo_rasterboundariestest.h"
#define INCLUDED_GEO_RASTERBOUNDARIESTEST
#endif

#ifndef INCLUDED_GEO_RASTERSPACETEST
#include "geo_rasterspacetest.h"
#define INCLUDED_GEO_RASTERSPACETEST
#endif

#ifndef INCLUDED_GEO_RASTERTEST
#include "geo_rastertest.h"
#define INCLUDED_GEO_RASTERTEST
#endif

#ifndef INCLUDED_GEO_RIKSNEIGHBOURHOODTEST
#include "geo_riksneighbourhoodtest.h"
#define INCLUDED_GEO_RIKSNEIGHBOURHOODTEST
#endif

#ifndef INCLUDED_GEO_SCANCONVERSIONTEST
#include "geo_scanconversiontest.h"
#define INCLUDED_GEO_SCANCONVERSIONTEST
#endif

#ifndef INCLUDED_GEO_SIMPLERASTERTEST
#include "geo_simplerastertest.h"
#define INCLUDED_GEO_SIMPLERASTERTEST
#endif

#ifndef INCLUDED_GEO_UTILTEST
#include "geo_utiltest.h"
#define INCLUDED_GEO_UTILTEST
#endif

#ifndef INCLUDED_GEO_BANDMAPTEST
#include "geo_bandmaptest.h"
#define INCLUDED_GEO_BANDMAPTEST
#endif

#ifndef INCLUDED_GEO_RASTERFILECONVERTERTEST
#include "geo_rasterfileconvertertest.h"
#define INCLUDED_GEO_RASTERFILECONVERTERTEST
#endif

#ifndef INCLUDED_GEO_POINTTEST
#include "geo_pointtest.h"
#define INCLUDED_GEO_POINTTEST
#endif

#ifndef INCLUDED_GEO_POINTVALUETEST
#include "geo_pointvaluetest.h"
#define INCLUDED_GEO_POINTVALUETEST
#endif

#ifndef INCLUDED_GEO_SQUARETEST
#include "geo_squaretest.h"
#define INCLUDED_GEO_SQUARETEST
#endif

#ifndef INCLUDED_GEO_NEIGHBOURTEST
#include "geo_neighbourtest.h"
#define INCLUDED_GEO_NEIGHBOURTEST
#endif

#ifndef INCLUDED_GEO_RASTERDIMTEST
#include "geo_rasterdimtest.h"
#define INCLUDED_GEO_RASTERDIMTEST
#endif

/*!
 \namespace geo
 \brief related to geographic / spatial issues
*/


boost::unit_test::test_suite* init_unit_test_suite(int /* argc */, char ** const /*argv*/) {
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(geo::RasterDimTest().suite());
  test->add(geo::NeighbourTest().suite());
  test->add(geo::UtilTest().suite());
  test->add(geo::PointTest().suite());
  test->add(geo::PointValueTest().suite());
  test->add(geo::SquareTest().suite());
  test->add(geo::BandMapTest().suite());
  test->add(geo::SimpleRasterTest().suite());
  test->add(geo::RasterSpaceTest().suite());
  test->add(geo::RasterTest().suite());
  test->add(geo::CellLocVisitorTest().suite());
  test->add(geo::CSFStackNameTest().suite());
  test->add(geo::CSFMapTest().suite());
  test->add(geo::FileCreateTesterTest().suite());

  test->add(geo::RasterFileConverterTest().suite());

  test->add(geo::GriddedPointsTest().suite());
  test->add(geo::AssignFilterTest().suite());
  test->add(geo::AddFilterTest().suite());
  test->add(geo::CountFilterTest().suite());
  test->add(geo::AverageFilterTest().suite());
  test->add(geo::FractionFilterTest().suite());
  test->add(geo::RasterBoundariesTest().suite());
  test->add(geo::ScanConversionTest().suite());
  test->add(geo::NeighbourhoodTest().suite());
  test->add(geo::MooreNeighbourhoodTest().suite());
  test->add(geo::CircularNeighbourhoodTest().suite());
  test->add(geo::RiksNeighbourhoodTest().suite());
  test->add(geo::AlgorithmTest().suite());

  return test;
 }

