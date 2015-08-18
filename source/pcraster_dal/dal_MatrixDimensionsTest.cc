#ifndef INCLUDED_DAL_MATRIXDIMENSIONSTEST
#include "dal_MatrixDimensionsTest.h"
#define INCLUDED_DAL_MATRIXDIMENSIONSTEST
#endif

// External headers.
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

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_MATRIXDIMENSIONS
#include "dal_MatrixDimensions.h"
#define INCLUDED_DAL_MATRIXDIMENSIONS
#endif



/*!
  \file
  This file contains the implementation of the MatrixDimensionsTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATRIXDIMENSIONSTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* MatrixDimensionsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MatrixDimensionsTest> instance(new MatrixDimensionsTest());
  suite->add(BOOST_CLASS_TEST_CASE(&MatrixDimensionsTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MATRIXDIMENSIONSTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
MatrixDimensionsTest::MatrixDimensionsTest()
{
}



void MatrixDimensionsTest::test()
{
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

} // namespace dal

