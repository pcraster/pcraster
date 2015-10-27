#define BOOST_TEST_MODULE pcraster dal array
#include <boost/test/unit_test.hpp>
#include "dal_Array.h"


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace dal;

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


BOOST_AUTO_TEST_CASE(index_of)
{
  using namespace dal;

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
