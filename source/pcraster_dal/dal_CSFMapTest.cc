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
  BOOST_TEST(map.nrRows() == size_t(5));
  BOOST_TEST(map.nrCols() == size_t(5));
  BOOST_TEST(map.cellSize() == 50);
  {
    UINT1 data[25];
    map.getCells(0, 25, data);
    BOOST_TEST(pcr::isMV(data[0]));
    BOOST_TEST(data[1] == 1);
    BOOST_TEST(map.fileTypeId() == TI_UINT1);
    BOOST_TEST(map.useTypeId() == TI_UINT1);

    auto min = std::any_cast<UINT1>(map.min());
    BOOST_TEST(min == 1);
  }
  {
    map.useAs(TI_REAL4);
    BOOST_TEST(map.fileTypeId() == TI_UINT1);
    BOOST_TEST(map.useTypeId() == TI_REAL4);
    map.useAs(TI_REAL4);
    REAL4 data[25];
    map.getCells(0, 25, data);
    BOOST_TEST(pcr::isMV(data[0]));
    BOOST_TEST(data[1] == 1);

    BOOST_CHECK_THROW(std::any_cast<UINT1>(map.min()), std::bad_any_cast);

    auto min = std::any_cast<REAL4>(map.min());
    BOOST_TEST(min == 1);
  }
}


BOOST_AUTO_TEST_CASE(create)
{
  using namespace dal;

  // empty NO DATA WRITTEN!
  {
   CSFMap const m("csfMapTestCreate.map", 5,5, 0,0,0,30,TI_UINT1,VS_BOOLEAN);
  }
  {
   CSFMap const map("csfMapTestCreate.map");
   BOOST_TEST(map.nrRows() == size_t(5));
   BOOST_TEST(map.nrCols() == size_t(5));
   BOOST_TEST(map.cellSize() == 30);
  }
}


BOOST_AUTO_TEST_CASE(error)
{
  using namespace dal;

  // should throw! bad cellsize (< 0)
  BOOST_CHECK_THROW(
         CSFMap("csfMapTestErrorCreate.map", 5, 5, 0, 0, 0, -30,
         TI_UINT1, VS_BOOLEAN), dal::Exception);
  BOOST_TEST(MstrError());
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
//   BOOST_TEST(map.hasLegend());
//   Table legend(map.legend());
//   BOOST_TEST(legend.title() == "");
//   BOOST_TEST(legend.nrRecs() == size_t(2));
//   BOOST_TEST(legend.nrCols() == size_t(2));
//   BOOST_TEST(legend.typeId(0) == TI_INT4);
//   BOOST_TEST(legend.typeId(1) == TI_STRING);
//   BOOST_TEST(legend.col<INT4>(0)[0] == 0);
//   BOOST_TEST(legend.col<std::string>(1)[0] == "no dump");
//   BOOST_TEST(legend.col<INT4>(0)[1] == 1);
//   BOOST_TEST(legend.col<std::string>(1)[1] == "dump");
// }
