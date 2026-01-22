#define BOOST_TEST_MODULE pcraster dal matrix
#include <boost/test/unit_test.hpp>
#include "dal_Matrix.h"


BOOST_AUTO_TEST_CASE(extremes)
{
  using namespace dal;

  { // setExtremes on no cells does not nothing
    Matrix matrix(2,2,TI_UINT1);
    BOOST_TEST(!matrix.cellsAreCreated());
    BOOST_TEST(!matrix.hasExtremes());
    matrix.setExtremes();
    BOOST_TEST(!matrix.hasExtremes());
  }

  { // setExtremes on with cells
    Matrix matrix(2,2,TI_UINT1);
    UINT1 data[4] = { MV_UINT1, 2, 4, 3 };
    matrix.setCellsReference(data);
    BOOST_TEST( matrix.cellsAreCreated());
    BOOST_TEST(!matrix.hasExtremes());
    matrix.setExtremes();
    BOOST_TEST(matrix.hasExtremes());
    BOOST_TEST(!matrix.allMV());
    BOOST_TEST(std::any_cast<UINT1>(matrix.min()) == 2);
    BOOST_TEST(std::any_cast<UINT1>(matrix.max()) == 4);
  }

  { // setExtremes on with All MV's
    Matrix matrix(1,1,TI_UINT1);
    UINT1 data[1] = { MV_UINT1 };
    matrix.setCellsReference(data);
    BOOST_TEST( matrix.cellsAreCreated());
    BOOST_TEST(!matrix.hasExtremes());
    matrix.setExtremes();
    BOOST_TEST(matrix.hasExtremes());
    BOOST_TEST(matrix.allMV());
    BOOST_TEST(!matrix.min().has_value());
    BOOST_TEST(!matrix.max().has_value());
  }

  {
    // setExtremes explicitly to certain values.
    Matrix matrix(2,2,TI_UINT1);
    UINT1 data[4] = { MV_UINT1, 2, 4, 3 };
    matrix.setCellsReference(data);
    BOOST_TEST( matrix.cellsAreCreated());
    BOOST_TEST(!matrix.hasExtremes());
    matrix.setExtremes(std::any(UINT1(0)), std::any(UINT1(5)));
    BOOST_TEST(matrix.hasExtremes());
    BOOST_TEST(!matrix.allMV());
    BOOST_TEST(std::any_cast<UINT1>(matrix.min()) == 0);
    BOOST_TEST(std::any_cast<UINT1>(matrix.max()) == 5);
  }
}
