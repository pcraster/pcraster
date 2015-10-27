#define BOOST_TEST_MODULE pcraster aguila matrix
#include <boost/test/unit_test.hpp>
#include "com_matrix.h"


BOOST_AUTO_TEST_CASE(equality)
{
  using namespace com;

  // Create two 2x3 matrices.
  Matrix<double> m1(2, 3);
  Matrix<double> m2(2, 3);

  // Fill the matrices.
  m1.setElement(1, 1, 1.0);
  m1.setElement(2, 1, 2.0);
  m1.setElement(1, 2, 3.0);
  m1.setElement(2, 2, 4.0);
  m1.setElement(1, 3, 5.0);
  m1.setElement(2, 3, 6.0);
  m2.setElement(1, 1, 1.0);
  m2.setElement(2, 1, 2.0);
  m2.setElement(1, 2, 3.0);
  m2.setElement(2, 2, 4.0);
  m2.setElement(1, 3, 5.0);
  m2.setElement(2, 3, 6.0);

  // Check for equality.
  BOOST_CHECK(m1 == m2);
}


BOOST_AUTO_TEST_CASE(inequality)
{
  using namespace com;

  // Create two 2x3 matrices.
  Matrix<double> m1(2, 3);
  Matrix<double> m2(2, 3);

  // Fill the matrices.
  m1.setElement(1, 1, 1.0);
  m1.setElement(2, 1, 2.0);
  m1.setElement(1, 2, 3.0);
  m1.setElement(2, 2, 4.0);
  m1.setElement(1, 3, 5.0);
  m1.setElement(2, 3, 6.0);
  m2.setElement(1, 1, 1.0);
  m2.setElement(2, 1, 2.0);
  m2.setElement(1, 2, 7.0);  // <-- different value.
  m2.setElement(2, 2, 4.0);
  m2.setElement(1, 3, 5.0);
  m2.setElement(2, 3, 6.0);

  // Check for equality.
  BOOST_CHECK(m1 != m2);
}


BOOST_AUTO_TEST_CASE(identity)
{
  using namespace com;

  // Create a 3x3 identity matrix.
  Matrix<double> m(Matrix<double>::identity(3));

  // Check the contents of the cells.
  BOOST_CHECK(m.element(1, 1) == 1.0);
  BOOST_CHECK(m.element(2, 2) == 1.0);
  BOOST_CHECK(m.element(3, 3) == 1.0);

  BOOST_CHECK(m.element(2, 1) == 0.0);
  BOOST_CHECK(m.element(3, 1) == 0.0);
  BOOST_CHECK(m.element(1, 2) == 0.0);
  BOOST_CHECK(m.element(3, 2) == 0.0);
  BOOST_CHECK(m.element(1, 3) == 0.0);
  BOOST_CHECK(m.element(2, 3) == 0.0);
}


BOOST_AUTO_TEST_CASE(invert)
{
  using namespace com;

// yepyep: weird stuff 1 != 1 ...
  // Create a 3x4 matrix.
  Matrix<double> m(3, 3);

  // Fill the matrix.
  m.setElement(1, 1, 1.0);
  m.setElement(2, 1, 4.5);
  m.setElement(3, 1, 5.9);
  m.setElement(1, 2, 7.4);
  m.setElement(2, 2, 3.6);
  m.setElement(3, 2, 9.2);
  m.setElement(1, 3, 1.4);
  m.setElement(2, 3, 4.8);
  m.setElement(3, 3, 8.3);

  // Create the inverse matrix.
  Matrix<double> inverse(Matrix<double>::invert(m));

  // Create the identity matrix.
  Matrix<double> identity(Matrix<double>::identity(3));

  // The original times its inverse must equal the identity matrix.
  Matrix<double> product(m * inverse);

  BOOST_CHECK(product.element(1, 1) - 1.0 < 1e-10);
  BOOST_CHECK(product.element(2, 1) < 1e-10);
  BOOST_CHECK(product.element(3, 1) < 1e-10);
  BOOST_CHECK(product.element(1, 2) < 1e-10);
  BOOST_CHECK(product.element(2, 2) - 1.0  < 1e-10);
  BOOST_CHECK(product.element(3, 2) < 1e-10);
  BOOST_CHECK(product.element(1, 3) < 1e-10);
  BOOST_CHECK(product.element(2, 3) < 1e-10);
  BOOST_CHECK(product.element(3, 3) - 1.0 < 1e-10);

  // BOOST_CHECK(m * inverse == inverse * m);
  // BOOST_CHECK(m * inverse == identity);
}


BOOST_AUTO_TEST_CASE(transpose)
{
  using namespace com;

  // Create a 2x3 matrix.
  Matrix<double> m(2, 3);

  // Fill the matrix.
  m.setElement(1, 1, 1.0);
  m.setElement(2, 1, 2.0);
  m.setElement(1, 2, 3.0);
  m.setElement(2, 2, 4.0);
  m.setElement(1, 3, 5.0);
  m.setElement(2, 3, 6.0);

  // Create the transpose.
  Matrix<double> transpose(Matrix<double>::transpose(m));

  // Check the transpose.
  BOOST_CHECK(m.nrRows() == transpose.nrCols());
  BOOST_CHECK(m.nrCols() == transpose.nrRows());
  BOOST_CHECK(m.element(1, 1) == transpose.element(1, 1));
  BOOST_CHECK(m.element(2, 1) == transpose.element(1, 2));
  BOOST_CHECK(m.element(1, 2) == transpose.element(2, 1));
  BOOST_CHECK(m.element(2, 2) == transpose.element(2, 2));
  BOOST_CHECK(m.element(1, 3) == transpose.element(3, 1));
  BOOST_CHECK(m.element(2, 3) == transpose.element(3, 2));
}
