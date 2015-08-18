#include "com_vectortest.h"
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>
#include "com_vector.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*com::VectorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<VectorTest> instance(new VectorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&VectorTest::testSetElement, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VectorTest::testSize, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VectorTest::testScale, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VectorTest::testAdd, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VectorTest::testMagnitude, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VectorTest::testDot, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VectorTest::testCross, instance));
  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

com::VectorTest::VectorTest()

{
}



void com::VectorTest::setUp()
{
}



void com::VectorTest::tearDown()
{
}



void com::VectorTest::testSize()
{
  Vector<double> v(5);
  BOOST_CHECK(v.size() == 5);
}



void com::VectorTest::testSetElement()
{
  Vector<double> v(5);
  v.setElement(1, 12345.6789);
  BOOST_CHECK(v.element(1) == 12345.6789);
}



void com::VectorTest::testScale()
{
  Vector<double> v(5);
  v.setElement(1, 1.2345);
  v.scale(2.0);
  BOOST_CHECK(v.element(1) == 2.469);
}



void com::VectorTest::testAdd()
{
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



void com::VectorTest::testMagnitude()
{
  Vector<double> v(5);
  v.setElement(1, 1.0);
  v.setElement(1, 2.0);
  v.setElement(1, 3.0);
  v.setElement(1, 4.0);
  v.setElement(1, 5.0);
  BOOST_CHECK(v.magnitude() == 5.0);
}



void com::VectorTest::testDot()
{
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



void com::VectorTest::testCross()
{
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



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


