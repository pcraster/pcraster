#define BOOST_TEST_MODULE pcraster aguila vector
#include <boost/test/unit_test.hpp>
#include "com_vector.h"


BOOST_AUTO_TEST_CASE(size)
{
  using namespace com;

  Vector<double> v(5);
  BOOST_CHECK(v.size() == 5);
}


BOOST_AUTO_TEST_CASE(set_element)
{
  using namespace com;

  Vector<double> v(5);
  v.setElement(1, 12345.6789);
  BOOST_CHECK(v.element(1) == 12345.6789);
}


BOOST_AUTO_TEST_CASE(scale)
{
  using namespace com;

  Vector<double> v(5);
  v.setElement(1, 1.2345);
  v.scale(2.0);
  BOOST_CHECK(v.element(1) == 2.469);
}


BOOST_AUTO_TEST_CASE(add)
{
  using namespace com;

  Vector<int> v1(3);
  Vector<int> v2(3);

  v1.setElement(1, 1);
  v1.setElement(2, 2);
  v1.setElement(3, -2);

  v2.setElement(1, 3);
  v2.setElement(2, 0);
  v2.setElement(3, 1);

  v1.add(v2);
  BOOST_CHECK(v1.element(1) == 4);
  BOOST_CHECK(v1.element(2) == 2);
  BOOST_CHECK(v1.element(3) == -1);
}


BOOST_AUTO_TEST_CASE(magnitude)
{
  using namespace com;

  Vector<double> v(5);
  v.setElement(1, 1.0);
  v.setElement(1, 2.0);
  v.setElement(1, 3.0);
  v.setElement(1, 4.0);
  v.setElement(1, 5.0);
  BOOST_CHECK(v.magnitude() == 5.0);
}


BOOST_AUTO_TEST_CASE(dot)
{
  using namespace com;

  Vector<int> v1(5);
  Vector<int> v2(5);

  v1.setElement(1, 1);
  v1.setElement(2, 2);
  v1.setElement(3, 3);
  v1.setElement(4, 4);
  v1.setElement(5, 5);

  v2.setElement(1, 6);
  v2.setElement(2, 7);
  v2.setElement(3, 8);
  v2.setElement(4, 9);
  v2.setElement(5, 10);

  BOOST_CHECK(Vector<int>::dot(v1, v2) == 130);
}


BOOST_AUTO_TEST_CASE(cross)
{
  using namespace com;

  Vector<int> v1(3);
  Vector<int> v2(3);

  v1.setElement(1, 1);
  v1.setElement(2, 2);
  v1.setElement(3, -2);

  v2.setElement(1, 3);
  v2.setElement(2, 0);
  v2.setElement(3, 1);

  Vector<int> v3 = Vector<int>::cross(v1, v2);
  BOOST_CHECK(v3.element(1) == 2);
  BOOST_CHECK(v3.element(2) == -7);
  BOOST_CHECK(v3.element(3) == -6);
}
