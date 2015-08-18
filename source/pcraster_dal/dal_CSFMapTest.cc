#ifndef INCLUDED_DAL_CSFMAPTEST
#include "dal_CSFMapTest.h"
#define INCLUDED_DAL_CSFMAPTEST
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
#ifndef INCLUDED_DAL_CSFMAP
#include "dal_CSFMap.h"
#define INCLUDED_DAL_CSFMAP
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif



/*!
  \file
  This file contains the implementation of the CSFMapTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CSFMAP MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::CSFMapTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CSFMapTest> instance(new CSFMapTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CSFMapTest::testRead, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFMapTest::testCreate, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFMapTest::testError, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFMapTest::testLegend, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CSFMAP MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::CSFMapTest::CSFMapTest()
{
}



//! setUp
void dal::CSFMapTest::setUp()
{
}



//! tearDown
void dal::CSFMapTest::tearDown()
{
}

void dal::CSFMapTest::testRead()
{
  CSFMap map("inp1b.map");
  BOOST_CHECK_EQUAL(map.nrRows(), size_t(5));
  BOOST_CHECK_EQUAL(map.nrCols(), size_t(5));
  BOOST_CHECK_EQUAL(map.cellSize(), 50);
  {
    UINT1 data[25];
    map.getCells(0, 25, data);
    BOOST_CHECK(pcr::isMV(data[0]));
    BOOST_CHECK_EQUAL(data[1], 1);
    BOOST_CHECK_EQUAL(map.fileTypeId(), TI_UINT1);
    BOOST_CHECK_EQUAL(map.useTypeId(), TI_UINT1);

    UINT1 min = boost::any_cast<UINT1>(map.min());
    BOOST_CHECK_EQUAL(min, 1);
  }
  {
    map.useAs(TI_REAL4);
    BOOST_CHECK_EQUAL(map.fileTypeId(), TI_UINT1);
    BOOST_CHECK_EQUAL(map.useTypeId(), TI_REAL4);
    map.useAs(TI_REAL4);
    REAL4 data[25];
    map.getCells(0, 25, data);
    BOOST_CHECK(pcr::isMV(data[0]));
    BOOST_CHECK_EQUAL(data[1], 1);

    BOOST_CHECK_THROW(boost::any_cast<UINT1>(map.min()), boost::bad_any_cast);

    REAL4 min = boost::any_cast<REAL4>(map.min());
    BOOST_CHECK_EQUAL(min, 1);
  }
}

void dal::CSFMapTest::testCreate()
{
  // empty NO DATA WRITTEN!
  {
   CSFMap m("csfMapTestCreate.map", 5,5, 0,0,0,30,TI_UINT1,VS_BOOLEAN);
  }
  {
   CSFMap map("csfMapTestCreate.map");
   BOOST_CHECK_EQUAL(map.nrRows(), size_t(5));
   BOOST_CHECK_EQUAL(map.nrCols(), size_t(5));
   BOOST_CHECK_EQUAL(map.cellSize(), 30);
  }
}

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif
#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif
void dal::CSFMapTest::testError()
{
  // should throw! bad cellsize (< 0)
  BOOST_CHECK_THROW(
         CSFMap("csfMapTestErrorCreate.map", 5, 5, 0, 0, 0, -30,
         TI_UINT1, VS_BOOLEAN), dal::Exception);
  BOOST_CHECK(MstrError());
  /* TODO
   * - let CSFMap and also gdal add  "driver" specific message to error
   *    such as MstrError()
   * - refactor all throw functions from dal_utils to something "smaller"
   *    function objects? make DataSourceError an object?
   */
}



namespace dal {

void CSFMapTest::testLegend()
{
  CSFMap map("d83.map");
  BOOST_CHECK(map.hasLegend());
  Table legend(map.legend());
  BOOST_CHECK_EQUAL(legend.title(), "");
  BOOST_CHECK_EQUAL(legend.nrRecs(), size_t(2));
  BOOST_CHECK_EQUAL(legend.nrCols(), size_t(2));
  BOOST_CHECK_EQUAL(legend.typeId(0), TI_INT4);
  BOOST_CHECK_EQUAL(legend.typeId(1), TI_STRING);
  BOOST_CHECK_EQUAL(legend.col<INT4>(0)[0], 0);
  BOOST_CHECK_EQUAL(legend.col<std::string>(1)[0], "no dump");
  BOOST_CHECK_EQUAL(legend.col<INT4>(0)[1], 1);
  BOOST_CHECK_EQUAL(legend.col<std::string>(1)[1], "dump");
}

}

