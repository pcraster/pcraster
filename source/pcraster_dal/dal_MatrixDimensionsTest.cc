#define BOOST_TEST_MODULE pcraster dal matrix_dimensions
#include <boost/test/unit_test.hpp>
#include "dal_MatrixDimensions.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  // Default.
  {
    MatrixDimensions matrix;

    BOOST_CHECK_EQUAL(matrix.nrRows(), size_t(1));
    BOOST_CHECK_EQUAL(matrix.nrCols(), size_t(1));
    BOOST_CHECK_EQUAL(matrix.nrCells(), size_t(1));
  }

  // Non-default.
  {
    MatrixDimensions matrix(3, 4);

    BOOST_CHECK_EQUAL(matrix.nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(matrix.nrCols(), size_t(4));
    BOOST_CHECK_EQUAL(matrix.nrCells(), size_t(12));
  }

  // Copy.
  {
    MatrixDimensions matrix1(3, 4);

    MatrixDimensions matrix2(matrix1);
    BOOST_CHECK_EQUAL(matrix2.nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(matrix2.nrCols(), size_t(4));

    MatrixDimensions matrix3 = matrix1;
    BOOST_CHECK_EQUAL(matrix3.nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(matrix3.nrCols(), size_t(4));
  }

  // Equality.
  {
    MatrixDimensions matrix1(3, 4), matrix2(4, 3), matrix3(3, 4);

    BOOST_CHECK(matrix1 == matrix1);
    BOOST_CHECK(matrix1 != matrix2);
    BOOST_CHECK(matrix1 == matrix3);
  }
}
