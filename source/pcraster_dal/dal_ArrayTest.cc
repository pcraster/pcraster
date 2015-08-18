#ifndef INCLUDED_DAL_ARRAYTEST
#include "dal_ArrayTest.h"
#define INCLUDED_DAL_ARRAYTEST
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
#ifndef INCLUDED_DAL_ARRAY
#include "dal_Array.h"
#define INCLUDED_DAL_ARRAY
#endif



/*!
  \file
  This file contains the implementation of the ArrayTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ARRAY MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::ArrayTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ArrayTest> instance(new ArrayTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ArrayTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ArrayTest::testIndexOf, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ARRAY MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::ArrayTest::ArrayTest()
{
}



//! setUp
void dal::ArrayTest::setUp()
{
}



//! tearDown
void dal::ArrayTest::tearDown()
{
}



void dal::ArrayTest::testConstructor()
{
  {
    Array<int> array;
    BOOST_CHECK(array.empty());
    BOOST_CHECK_EQUAL(array.size(), size_t(0));
  }

  {
    Array<int> array(5);
    BOOST_CHECK(!array.empty());
    BOOST_CHECK_EQUAL(array.size(), size_t(5));
  }

  {
    Array<int> array(5, 3);
    BOOST_CHECK(!array.empty());
    BOOST_CHECK_EQUAL(array.size(), size_t(5));
    for(size_t i = 0; i < 5; ++i) {
      BOOST_CHECK_EQUAL(array[i], 3);
    }
  }

  {
    Array<int> rhs(5, 3);
    Array<int> lhs(rhs);
    /*
    for(size_t i = 0; i < 5; ++i) {
      lhs[i] == 3;
    }
  */
  }
}


void dal::ArrayTest::testIndexOf()
{
  {
    Array<int> array;
    BOOST_CHECK_EQUAL(indexOf(array, 5), 0);
  }

  {
    Array<int> array(5);
    BOOST_CHECK_EQUAL(indexOf(array, 5), 5);

    array[0] = 1; array[1] = 2; array[2] = 3; array[3] = 4; array[4] = 5;
    BOOST_CHECK_EQUAL(indexOf(array, 1), 0);
    BOOST_CHECK_EQUAL(indexOf(array, 3), 2);
    BOOST_CHECK_EQUAL(indexOf(array, 5), 4);
    BOOST_CHECK_EQUAL(indexOf(array, 0), 5);
    BOOST_CHECK_EQUAL(indexOf(array, 6), 5);

    array[0] = 5; array[1] = 4; array[2] = 3; array[3] = 2; array[4] = 1;
    BOOST_CHECK_EQUAL(indexOf(array, 1), 4);
    BOOST_CHECK_EQUAL(indexOf(array, 3), 2);
    BOOST_CHECK_EQUAL(indexOf(array, 5), 0);
    BOOST_CHECK_EQUAL(indexOf(array, 0), 5);
    BOOST_CHECK_EQUAL(indexOf(array, 6), 5);
  }
}

