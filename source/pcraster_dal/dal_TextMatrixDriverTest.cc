#define BOOST_TEST_MODULE pcraster dal text_matrix_driver
#include <boost/test/unit_test.hpp>
#include "dal_Exception.h"
#include "dal_TextMatrixDriver.h"


BOOST_AUTO_TEST_CASE(description)
{
  using namespace dal;

  TextMatrixDriver driver;
  BOOST_CHECK_EQUAL(driver.description(), "Text matrix file format");
}


BOOST_AUTO_TEST_CASE(unexisting)
{
  using namespace dal;

  std::string filename = "unexisting";
  TextMatrixDriver driver;
  bool exceptionCaught;

  Matrix* matrix = dynamic_cast<Matrix*>(
         dynamic_cast<Driver&>(driver).open(filename));
  BOOST_CHECK(!matrix);

  try {
    exceptionCaught = false;
    matrix = dynamic_cast<MatrixDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_CHECK_EQUAL(exception.message(),
       "Data source " + filename + "(matrix):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(empty)
{
  using namespace dal;

  std::string filename = "emptyfile";
  TextMatrixDriver driver;
  bool exceptionCaught;

  Matrix* matrix = dynamic_cast<Matrix*>(
         dynamic_cast<Driver&>(driver).open(filename));
  BOOST_CHECK(!matrix);

  try {
    exceptionCaught = false;
    matrix = dynamic_cast<MatrixDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_CHECK_EQUAL(exception.message(),
       "Data source " + filename + "(matrix):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(invalid_grammar)
{
  using namespace dal;

  std::string filename = ":/:/:/:/:";
  TextMatrixDriver driver;
  bool exceptionCaught;

  Matrix* matrix = dynamic_cast<Matrix*>(
         dynamic_cast<Driver&>(driver).open(filename));
  BOOST_CHECK(!matrix);

  try {
    exceptionCaught = false;
    matrix = dynamic_cast<MatrixDriver&>(driver).read(filename);
  }
  catch(Exception& exception) {
    BOOST_CHECK_EQUAL(exception.message(),
         "Data source " + filename + "(matrix):\ncannot be opened");
    exceptionCaught = true;
  }
  BOOST_CHECK(exceptionCaught);
}


BOOST_AUTO_TEST_CASE(matrix1)
{
  using namespace dal;

  std::string filename;
  TextMatrixDriver driver;
  Matrix* matrix;

  {
    filename = "matrix1.txt";
    matrix = dynamic_cast<Matrix*>(
           dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(matrix);
    BOOST_CHECK_EQUAL(matrix->nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(matrix->nrCols(), size_t(3));
    BOOST_CHECK_EQUAL(matrix->typeId(), TI_REAL4);

    driver.read(filename, *matrix);
    BOOST_CHECK_EQUAL(matrix->nrRows(), size_t(6));

    REAL4 const* cells = matrix->cells<REAL4>();
    BOOST_CHECK_EQUAL(cells[0],  1.0);
    BOOST_CHECK_EQUAL(cells[1],  2.0);
    BOOST_CHECK_EQUAL(cells[2],  3.0);
    BOOST_CHECK_EQUAL(cells[3],  4.0);
    BOOST_CHECK_EQUAL(cells[4], -5.0);
    BOOST_CHECK_EQUAL(cells[5],  6.0);
    BOOST_CHECK_EQUAL(cells[6],  7.0);
    BOOST_CHECK_EQUAL(cells[7],  8.0);
    BOOST_CHECK_EQUAL(cells[8],  9.0);
    BOOST_CHECK_EQUAL(cells[9], 10.0);
    BOOST_CHECK_EQUAL(cells[10], 11.0);
    BOOST_CHECK_EQUAL(cells[11], 12.0);
    BOOST_CHECK_EQUAL(cells[12], 13.0);
    BOOST_CHECK_EQUAL(cells[13], 14.0);
    BOOST_CHECK_EQUAL(cells[14], 15.5);
    BOOST_CHECK_EQUAL(cells[15], 16.0);
    BOOST_CHECK_EQUAL(cells[16], 17.0);
    BOOST_CHECK_EQUAL(cells[17], 18.0);

    delete matrix;
  }
}


BOOST_AUTO_TEST_CASE(matrix2)
{
  using namespace dal;

  std::string filename;
  TextMatrixDriver driver;
  Matrix* matrix;

  {
    filename = "matrix2.txt";
    matrix = dynamic_cast<Matrix*>(
           dynamic_cast<Driver&>(driver).open(filename));
    BOOST_CHECK(matrix);
    BOOST_CHECK_EQUAL(matrix->nrRows(), size_t(0));
    BOOST_CHECK_EQUAL(matrix->nrCols(), size_t(3));
    BOOST_CHECK_EQUAL(matrix->typeId(), TI_UINT1);

    driver.read(filename, *matrix);
    BOOST_CHECK_EQUAL(matrix->nrRows(), size_t(3));

    UINT1 const* cells = matrix->cells<UINT1>();
    BOOST_CHECK_EQUAL(cells[0],  1.0);
    BOOST_CHECK_EQUAL(cells[1],  1.0);
    BOOST_CHECK_EQUAL(cells[2],  1.0);
    BOOST_CHECK_EQUAL(cells[3],  1.0);
    BOOST_CHECK_EQUAL(cells[4],  0.0);
    BOOST_CHECK_EQUAL(cells[5],  1.0);
    BOOST_CHECK_EQUAL(cells[6],  1.0);
    BOOST_CHECK_EQUAL(cells[7],  1.0);
    BOOST_CHECK_EQUAL(cells[8],  1.0);

    delete matrix;
  }
}


/// BOOST_AUTO_TEST_CASE(matrix3)
/// {
///   using namespace dal;
/// 
///   /*
///   std::string filename;
///   TextMatrixDriver driver;
///   Matrix* matrix;
/// 
///   {
///     filename = "matrix3.txt";
///     matrix = driver.open(filename);
///     BOOST_CHECK(matrix);
///     BOOST_CHECK_EQUAL(matrix->nrRows(), size_t(0));
///     BOOST_CHECK_EQUAL(matrix->nrCols(), size_t(3));
///     BOOST_CHECK_EQUAL(matrix->typeId(), TI_STRING);
/// 
///     driver.read(filename, *matrix);
///     BOOST_CHECK_EQUAL(matrix->nrRows(), size_t(3));
/// 
///     Array<std::string> const& cells = matrix->cells<std::string>();
///     BOOST_CHECK_EQUAL(cells[0],  "1");
///     BOOST_CHECK_EQUAL(cells[1],  "1.0");
///     BOOST_CHECK_EQUAL(cells[2],  "1");
///     BOOST_CHECK_EQUAL(cells[3],  "1");
///     BOOST_CHECK_EQUAL(cells[4],  "0");
///     BOOST_CHECK_EQUAL(cells[5],  "1");
///     BOOST_CHECK_EQUAL(cells[6],  "1");
///     BOOST_CHECK_EQUAL(cells[7],  "1");
///     BOOST_CHECK_EQUAL(cells[8],  "a");
/// 
///     delete matrix;
///   }
///   */
/// }


/// BOOST_AUTO_TEST_CASE(matrix4)
/// {
///   using namespace dal;
/// 
///   std::string filename;
///   TextMatrixDriver driver;
///   Matrix* matrix;
/// 
///   {
///     filename = "matrix4.txt";
///     matrix = dynamic_cast<Matrix*>(
///            dynamic_cast<Driver&>(driver).open(filename));
///     /*
///     BOOST_CHECK(matrix);
///     BOOST_CHECK_EQUAL(matrix->nrRows(), size_t(0));
///     BOOST_CHECK_EQUAL(matrix->nrCols(), size_t(3));
///     BOOST_CHECK_EQUAL(matrix->typeId(), TI_REAL4);
/// 
///     driver.read(filename, *matrix);
///     */
/// 
///     delete matrix;
///   }
/// }
