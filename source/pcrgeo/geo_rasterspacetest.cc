#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RASTERSPACETEST
#include "geo_rasterspacetest.h"
#define INCLUDED_GEO_RASTERSPACETEST
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

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



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*geo::RasterSpaceTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<RasterSpaceTest> instance(new RasterSpaceTest());
  suite->add(BOOST_CLASS_TEST_CASE(&RasterSpaceTest::testEquality, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterSpaceTest::testNrRows, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterSpaceTest::testNrCols, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterSpaceTest::testNrCells, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterSpaceTest::testQuadrant, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&RasterSpaceTest::testIO, instance));
  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

geo::RasterSpaceTest::RasterSpaceTest()

  :    d_rs1(0), d_rs2(0)

{
}



void geo::RasterSpaceTest::setUp()
{
  d_rs1 = new RasterSpace(11, 12, 13.0, 14.0, 15.0);
}



void geo::RasterSpaceTest::tearDown()
{
  delete d_rs1;
  delete d_rs2;
}



void geo::RasterSpaceTest::testEquality()
{
  setUp();

  BOOST_CHECK(RasterSpace(1, 2, 3.0, 4.0, 5.0, geo::YIncrB2T, 6.0) ==
            RasterSpace(1, 2, 3.0, 4.0, 5.0, geo::YIncrB2T, 6.0));

  tearDown();
}



void geo::RasterSpaceTest::testNrRows()
{
  setUp();

  BOOST_CHECK(d_rs1->nrRows() == 11);

  tearDown();
}



void geo::RasterSpaceTest::testNrCols()
{
  setUp();

  BOOST_CHECK(d_rs1->nrCols() == 12);

  tearDown();
}



void geo::RasterSpaceTest::testNrCells()
{
  setUp();

  BOOST_CHECK(d_rs1->nrCells() == 11 * 12);

  tearDown();
}



void geo::RasterSpaceTest::testQuadrant()
{
  setUp();

  {
    RasterSpace space(3, 3, 1.0, 0.0, 0.0, geo::YIncrT2B);
    BOOST_CHECK(space.quadrant(0.0, 0.0) == NorthWest);
    BOOST_CHECK(space.quadrant(3.0, 0.0) == NorthWest);
    BOOST_CHECK(space.quadrant(3.0, 3.0) == NorthWest);
    BOOST_CHECK(space.quadrant(0.0, 3.0) == NorthWest);

    BOOST_CHECK(space.quadrant(1.1, 1.1) == NorthWest);
    BOOST_CHECK(space.quadrant(1.9, 1.1) == NorthEast);
    BOOST_CHECK(space.quadrant(1.9, 1.9) == SouthEast);
    BOOST_CHECK(space.quadrant(1.1, 1.9) == SouthWest);

    BOOST_CHECK(space.quadrant(1.5, 1.5) == SouthEast);
  }

  {
    RasterSpace space(3, 3, 1.0, 0.0, 0.0, geo::YIncrB2T);
    BOOST_CHECK(space.quadrant(0.0,  0.0) == NorthWest);
    BOOST_CHECK(space.quadrant(3.0,  0.0) == NorthWest);
    BOOST_CHECK(space.quadrant(3.0, -3.0) == NorthWest);
    BOOST_CHECK(space.quadrant(0.0, -3.0) == NorthWest);

    BOOST_CHECK(space.quadrant(1.1, -1.1) == NorthWest);
    BOOST_CHECK(space.quadrant(1.9, -1.1) == NorthEast);
    BOOST_CHECK(space.quadrant(1.9, -1.9) == SouthEast);
    BOOST_CHECK(space.quadrant(1.1, -1.9) == SouthWest);

    BOOST_CHECK(space.quadrant(1.5, -1.5) == SouthEast);
  }

  tearDown();
}



void geo::RasterSpaceTest::testIO()
{
  setUp();

  bool stuffReadDoesntEqualStuffWritten;

  try {

    stuffReadDoesntEqualStuffWritten = false;

    std::stringstream s;

    RasterSpace rs1(11, 12, 13.0, 14.0, 15.0, geo::YIncrB2T);
    s << rs1;

    RasterSpace rs2;
    s >> rs2;

    BOOST_CHECK(rs1 == rs2);

  }
  catch(const com::BadStreamFormat& ) {
    stuffReadDoesntEqualStuffWritten = true;
  }

  BOOST_CHECK(!stuffReadDoesntEqualStuffWritten);



  try {

    stuffReadDoesntEqualStuffWritten = false;

    std::stringstream s;

    RasterSpace rs1(11, 12, 13.0, 14.0, 15.0, geo::YIncrB2T);
    s << "blabla" << rs1;

    RasterSpace rs2;
    s >> rs2;

  }
  catch(const com::BadStreamFormat& ) {
    stuffReadDoesntEqualStuffWritten = true;
  }

  BOOST_CHECK(stuffReadDoesntEqualStuffWritten);

  tearDown();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


