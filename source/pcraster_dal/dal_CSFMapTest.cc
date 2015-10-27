#define BOOST_TEST_MODULE pcraster dal csf_map
#include <boost/test/unit_test.hpp>
#include "csf.h"
#include "dal_CSFMap.h"
#include "dal_Exception.h"
#include "dal_Table.h"


BOOST_AUTO_TEST_CASE(read_)
{
  using namespace dal;

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


BOOST_AUTO_TEST_CASE(create)
{
  using namespace dal;

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


BOOST_AUTO_TEST_CASE(error)
{
  using namespace dal;

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


// BOOST_AUTO_TEST_CASE(legend)
// {
//   using namespace dal;
// 
//   // TODO Bug? Legend detection seems wrong.
//   CSFMap map("d83.map");
//   BOOST_CHECK(map.hasLegend());
//   Table legend(map.legend());
//   BOOST_CHECK_EQUAL(legend.title(), "");
//   BOOST_CHECK_EQUAL(legend.nrRecs(), size_t(2));
//   BOOST_CHECK_EQUAL(legend.nrCols(), size_t(2));
//   BOOST_CHECK_EQUAL(legend.typeId(0), TI_INT4);
//   BOOST_CHECK_EQUAL(legend.typeId(1), TI_STRING);
//   BOOST_CHECK_EQUAL(legend.col<INT4>(0)[0], 0);
//   BOOST_CHECK_EQUAL(legend.col<std::string>(1)[0], "no dump");
//   BOOST_CHECK_EQUAL(legend.col<INT4>(0)[1], 1);
//   BOOST_CHECK_EQUAL(legend.col<std::string>(1)[1], "dump");
// }
