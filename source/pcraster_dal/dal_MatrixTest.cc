#ifndef INCLUDED_DAL_MATRIXTEST
#include "dal_MatrixTest.h"
#define INCLUDED_DAL_MATRIXTEST
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
#ifndef INCLUDED_DAL_MATRIX
#include "dal_Matrix.h"
#define INCLUDED_DAL_MATRIX
#endif


/*!
  \file
  This file contains the implementation of the MatrixTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATRIX MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* MatrixTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MatrixTest> instance(new MatrixTest());

  suite->add(BOOST_CLASS_TEST_CASE(&MatrixTest::testExtremes, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MATRIX MEMBERS
//------------------------------------------------------------------------------

//! ctor
MatrixTest::MatrixTest(
         )
{
}



//! setUp
void MatrixTest::setUp()
{
}



//! tearDown
void MatrixTest::tearDown()
{
}



void MatrixTest::testExtremes()
{
  { // setExtremes on no cells does not nothing
    Matrix matrix(2,2,TI_UINT1);
    BOOST_CHECK(!matrix.cellsAreCreated());
    BOOST_CHECK(!matrix.hasExtremes());
    matrix.setExtremes();
    BOOST_CHECK(!matrix.hasExtremes());
  }

  { // setExtremes on with cells
    Matrix matrix(2,2,TI_UINT1);
    UINT1 data[4] = { MV_UINT1, 2, 4, 3 };
    matrix.setCellsReference(data);
    BOOST_CHECK( matrix.cellsAreCreated());
    BOOST_CHECK(!matrix.hasExtremes());
    matrix.setExtremes();
    BOOST_CHECK(matrix.hasExtremes());
    BOOST_CHECK(!matrix.allMV());
    BOOST_CHECK_EQUAL(boost::any_cast<UINT1>(matrix.min()), 2);
    BOOST_CHECK_EQUAL(boost::any_cast<UINT1>(matrix.max()), 4);
  }

  { // setExtremes on with All MV's
    Matrix matrix(1,1,TI_UINT1);
    UINT1 data[1] = { MV_UINT1 };
    matrix.setCellsReference(data);
    BOOST_CHECK( matrix.cellsAreCreated());
    BOOST_CHECK(!matrix.hasExtremes());
    matrix.setExtremes();
    BOOST_CHECK(matrix.hasExtremes());
    BOOST_CHECK(matrix.allMV());
    BOOST_CHECK(matrix.min().empty());
    BOOST_CHECK(matrix.max().empty());
  }

  {
    // setExtremes explicitly to certain values.
    Matrix matrix(2,2,TI_UINT1);
    UINT1 data[4] = { MV_UINT1, 2, 4, 3 };
    matrix.setCellsReference(data);
    BOOST_CHECK( matrix.cellsAreCreated());
    BOOST_CHECK(!matrix.hasExtremes());
    matrix.setExtremes(boost::any(UINT1(0)), boost::any(UINT1(5)));
    BOOST_CHECK(matrix.hasExtremes());
    BOOST_CHECK(!matrix.allMV());
    BOOST_CHECK_EQUAL(boost::any_cast<UINT1>(matrix.min()), 0);
    BOOST_CHECK_EQUAL(boost::any_cast<UINT1>(matrix.max()), 5);
  }
}



} // namespace dal

