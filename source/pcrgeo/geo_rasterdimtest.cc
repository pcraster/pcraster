#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RASTERDIMTEST
#include "geo_rasterdimtest.h"
#define INCLUDED_GEO_RASTERDIMTEST
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

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif
#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif

/*!
  \file
  This file contains the implementation of the RasterDimTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERDIM MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::RasterDimTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RasterDimTest> instance(new RasterDimTest());

  suite->add(BOOST_CLASS_TEST_CASE(&RasterDimTest::testTarget, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF RASTERDIM MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::RasterDimTest::RasterDimTest()
{
}



//! setUp
void geo::RasterDimTest::setUp()
{
}

//! tearDown
void geo::RasterDimTest::tearDown()
{
}



void geo::RasterDimTest::testTarget()
{
  RasterDim rd(3,3);

  // all NBS are inside grid
  LinearLoc f=rd.convert(CellLoc(1,1));
  BOOST_CHECK(f==4);


  BOOST_CHECK(rd.target<LDD>(f,7)==0);
  BOOST_CHECK(rd.target<LDD>(f,3)==8);
  BOOST_CHECK(rd.target<LDD>(f,5)==4);

  BOOST_CHECK(rd.target<NB>(f,2)==8);
/*
  std::cout << "val " << rd.target<LDD>(f,7) << " \n";
  BOOST_CHECK(rd.target<NB>(f,1, false));
  BOOST_CHECK(rd.target<NB>(f,2, false));
  BOOST_CHECK(rd.target<NB>(f,3, false));
  BOOST_CHECK(rd.target<NB>(f,4, false));
  BOOST_CHECK(rd.target<NB>(f,5, false));
  BOOST_CHECK(rd.target<NB>(f,6, false));
*/
  //BOOST_CHECK(nbTarget(f,rd,7, false));
/*
  // left NBs are outside
  f=rd.convert(CellLoc(1,0));

  BOOST_CHECK(lddTarget(f,rd,1, true ));
  BOOST_CHECK(lddTarget(f,rd,2, false));
  BOOST_CHECK(lddTarget(f,rd,3, false));
  BOOST_CHECK(lddTarget(f,rd,4, true ));
  BOOST_CHECK(lddTarget(f,rd,5, false));
  BOOST_CHECK(lddTarget(f,rd,6, false));
  BOOST_CHECK(lddTarget(f,rd,7, true ));
  BOOST_CHECK(lddTarget(f,rd,8, false));
  BOOST_CHECK(lddTarget(f,rd,9, false));

  // top/left NBs are outside
  f=rd.convert(CellLoc(1,0));

  BOOST_CHECK(lddTarget(f,rd,1, true ));
  BOOST_CHECK(lddTarget(f,rd,2, false));
  BOOST_CHECK(lddTarget(f,rd,3, false));
  BOOST_CHECK(lddTarget(f,rd,4, true ));
  BOOST_CHECK(lddTarget(f,rd,5, false));
  BOOST_CHECK(lddTarget(f,rd,6, false));
  BOOST_CHECK(lddTarget(f,rd,7, true ));
  BOOST_CHECK(lddTarget(f,rd,8, true ));
  BOOST_CHECK(lddTarget(f,rd,9, true ));

  // bottom/right NBs are outside
  f=rd.convert(CellLoc(rd.nrRows()-1,rd.nrCols()-1));

  BOOST_CHECK(lddTarget(f,rd,1, true ));
  BOOST_CHECK(lddTarget(f,rd,2, true ));
  BOOST_CHECK(lddTarget(f,rd,3, true ));
  BOOST_CHECK(lddTarget(f,rd,4, false));
  BOOST_CHECK(lddTarget(f,rd,5, false));
  BOOST_CHECK(lddTarget(f,rd,6, true ));
  BOOST_CHECK(lddTarget(f,rd,7, false));
  BOOST_CHECK(lddTarget(f,rd,8, false));
  BOOST_CHECK(lddTarget(f,rd,9, true ));
*/
}
