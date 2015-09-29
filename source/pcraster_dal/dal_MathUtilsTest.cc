#define BOOST_TEST_MODULE pcraster dal array
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"


BOOST_AUTO_TEST_CASE(clamp_)
{
  using namespace dal;

  size_t first, last, interval;

  {
    first = 5;
    last = 10;
    interval = 1;
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 0), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 1), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 4), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 5), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 6), size_t(6));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 9), size_t(9));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 10), size_t(10));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 11), size_t(10));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 12), size_t(10));
  }

  {
    first = 5;
    last = 5;
    interval = 1;
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 3), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 4), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 5), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 6), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 7), size_t(5));
  }

  {
    first = 5;
    last = 6;
    interval = 1;
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 3), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 4), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 5), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 6), size_t(6));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 7), size_t(6));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 8), size_t(6));
  }

  {
    first = 5;
    last = 10;
    interval = 2;
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 0), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 1), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 4), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 5), size_t(5));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 6), size_t(7));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 7), size_t(7));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 8), size_t(9));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 9), size_t(9));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 10), size_t(9));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 11), size_t(9));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 12), size_t(9));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 13), size_t(9));
  }

  {
    first = 1;
    last = 100;
    interval = 2;
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 99), size_t(99));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 100), size_t(99));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 101), size_t(99));
    BOOST_CHECK_EQUAL(clamp<size_t>(first, last, interval, 102), size_t(99));
  }

  {
    float first, last, interval;

    {
      first    = 0.01f;
      last     = 0.99f;
      interval = 0.01f;

      // Below interval.
      BOOST_CHECK_CLOSE(clamp<float>(first, last, interval, 0.0f),
         float(0.01), 0.001f);

      // Between two values in the interval.
      BOOST_CHECK_CLOSE(clamp<float>(first, last, interval, 0.49f),
         float(0.49), 0.001f);
      BOOST_CHECK_CLOSE(clamp<float>(first, last, interval, 0.494f),
         float(0.49), 0.001f);
      BOOST_CHECK_CLOSE(clamp<float>(first, last, interval, 0.495f),
         float(0.5), 0.001f);
      BOOST_CHECK_CLOSE(clamp<float>(first, last, interval, 0.5f),
         float(0.5), 0.001f);

      // Above interval.
      BOOST_CHECK_CLOSE(clamp<float>(first, last, interval, 1.0f),
         float(0.99), 0.001f);
    }
  }
}


BOOST_AUTO_TEST_CASE(is_regular_increasing_range)
{
  using namespace dal;

  {
    int range[] = { 1, 2, 3, 4, 5 };
    int first, last, interval;
    BOOST_CHECK(isRegularIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first, 1);
    BOOST_CHECK_EQUAL(last, 5);
    BOOST_CHECK_EQUAL(interval, 1);
  }

  {
    std::vector<size_t> steps;
    steps.push_back(1);
    steps.push_back(2);
    steps.push_back(3);
    size_t first, last, interval;
    BOOST_CHECK(isRegularIncreasingRange(first, last, interval,
         steps.begin(), steps.end()));
    BOOST_CHECK_EQUAL(first, size_t(1));
    BOOST_CHECK_EQUAL(last, size_t(3));
    BOOST_CHECK_EQUAL(interval, size_t(1));
  }

  {
    int range[] = { 0 }; // empty array illegal
    int first, last, interval;
    BOOST_CHECK(!isRegularIncreasingRange(first, last, interval,
         range, range + 0 ));
  }

  {
    int range[] = { 5 };
    int first, last, interval;
    BOOST_CHECK(!isRegularIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
  }

  {
    int range[] = { 5, 6 };
    int first, last, interval;
    BOOST_CHECK(isRegularIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first, 5);
    BOOST_CHECK_EQUAL(last, 6);
    BOOST_CHECK_EQUAL(interval, 1);
  }

  {
    int range[] = { 1, 3, 5 };
    int first, last, interval;
    BOOST_CHECK(isRegularIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first, 1);
    BOOST_CHECK_EQUAL(last, 5);
    BOOST_CHECK_EQUAL(interval, 2);
  }

  {
    int range[] = { 1, 2, 3, 5 };
    int first, last, interval;
    BOOST_CHECK(!isRegularIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
  }

  {
    int range[] = { 1, 3, 4, 5 };
    int first, last, interval;
    BOOST_CHECK(!isRegularIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
  }

  {
    int range[] = { 46, 47, 48, 49, 50 };
    int first, last, interval;
    BOOST_CHECK(isRegularIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first, 46);
    BOOST_CHECK_EQUAL(last, 50);
    BOOST_CHECK_EQUAL(interval, 1);
  }
}


BOOST_AUTO_TEST_CASE(is_increasing_range)
{
  using namespace dal;

  {
    int range[] = { 1, 2, 3, 4, 5 };
    int first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first   , 1);
    BOOST_CHECK_EQUAL(last    , 5);
    BOOST_CHECK_EQUAL(interval, 1);
  }

  {
    int range[] = { 2, 4, 6 };
    int first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first   , 2);
    BOOST_CHECK_EQUAL(last    , 6);
    BOOST_CHECK_EQUAL(interval, 2);
  }

  {
    int range[] = { 2, 4, 7 };
    int first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first   , 2);
    BOOST_CHECK_EQUAL(last    , 7);
    BOOST_CHECK_EQUAL(interval, 1);
  }

  {
    std::vector<size_t> steps;
    steps.push_back(1);
    steps.push_back(2);
    steps.push_back(3);
    size_t first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         steps.begin(), steps.end()));
    BOOST_CHECK_EQUAL(first   , size_t(1));
    BOOST_CHECK_EQUAL(last    , size_t(3));
    BOOST_CHECK_EQUAL(interval, size_t(1));
  }

  {
    int range[] = { 0 }; // empty array illegal
    int first, last, interval;
    BOOST_CHECK(!isIncreasingRange(first, last, interval,
         range, range + 0 ));
  }

  {
    int range[] = { 5 };
    int first, last, interval;
    BOOST_CHECK(!isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
  }

  {
    int range[] = { 5, 6 };
    int first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first   , 5);
    BOOST_CHECK_EQUAL(last    , 6);
    BOOST_CHECK_EQUAL(interval, 1);
  }

  {
    int range[] = { 1, 3, 5 };
    int first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first   , 1);
    BOOST_CHECK_EQUAL(last    , 5);
    BOOST_CHECK_EQUAL(interval, 2);
  }

  {
    int range[] = { 1, 2, 3, 5 };
    int first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first   , 1);
    BOOST_CHECK_EQUAL(last    , 5);
    BOOST_CHECK_EQUAL(interval, 1);
  }

  {
    int range[] = { 1, 3, 4, 5 };
    int first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first   , 1);
    BOOST_CHECK_EQUAL(last    , 5);
    BOOST_CHECK_EQUAL(interval, 1);
  }

  {
    int range[] = { 46, 47, 48, 49, 50 };
    int first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(int)));
    BOOST_CHECK_EQUAL(first   , 46);
    BOOST_CHECK_EQUAL(last    , 50);
    BOOST_CHECK_EQUAL(interval, 1);
  }

  {
    double range[] = { 0.10, 0.25, 0.50, 0.75, 0.90 };
    double first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(double)));
    BOOST_CHECK_CLOSE(first   , 0.10, 0.001);
    BOOST_CHECK_CLOSE(last    , 0.90, 0.001);
    BOOST_CHECK_CLOSE(interval, 0.05, 0.001);
  }

  {
    double range[] = { 0.10, 0.25, 0.50, 0.75, 0.90, 0.99 };
    double first, last, interval;
    BOOST_CHECK(isIncreasingRange(first, last, interval,
         range, range + sizeof(range) / sizeof(double)));
    BOOST_CHECK_CLOSE(first   , 0.10, 0.001);
    BOOST_CHECK_CLOSE(last    , 0.99, 0.001);
    BOOST_CHECK_CLOSE(interval, 0.01, 0.001);
  }
}


BOOST_AUTO_TEST_CASE(comparable_)
{
  using namespace dal;

  BOOST_CHECK(comparable<float>(0.0f, 0.0f));
}


BOOST_AUTO_TEST_CASE(interpolate_)
{
  using namespace dal;

  {
    Array<double> array;
    BOOST_CHECK(array.empty());
    interpolate(array);
    BOOST_CHECK(array.empty());
  }

  {
    Array<double> array;
    array.resize(1);
    array[0] = 1.0;
    interpolate(array);
    BOOST_CHECK(comparable<double>(array[0], 1.0));
  }

  {
    Array<double> array;
    array.resize(1);
    pcr::setMV(array[0]);
    interpolate(array);
    pcr::isMV(array[0]);
  }

  {
    Array<double> array;
    array.resize(3);
    array[0] = 1.0;
    pcr::setMV(array[1]);
    array[2] = 3.0;
    interpolate(array);
    BOOST_CHECK(comparable<double>(array[0], 1.0));
    BOOST_CHECK(comparable<double>(array[1], 2.0));
    BOOST_CHECK(comparable<double>(array[2], 3.0));
  }

  {
    Array<double> array;
    array.resize(5);
    array[0] = 1.0;
    pcr::setMV(array[1]);
    array[2] = 3.0;
    pcr::setMV(array[3]);
    array[4] = 5.0;
    interpolate(array);
    BOOST_CHECK(comparable<double>(array[0], 1.0));
    BOOST_CHECK(comparable<double>(array[1], 2.0));
    BOOST_CHECK(comparable<double>(array[2], 3.0));
    BOOST_CHECK(comparable<double>(array[3], 4.0));
    BOOST_CHECK(comparable<double>(array[4], 5.0));
  }
}


BOOST_AUTO_TEST_CASE(fill_using_previous_value)
{
  using namespace dal;

  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(merge_ranges)
{
  using namespace dal;

  {
    // Integral tests.
    size_t first, last, step;

    {
      first = 1;
      last = 10;
      step = 2;
      mergeRanges<size_t>(first, last, step, 1, 10, 2);

      BOOST_CHECK_EQUAL(first, size_t(1));
      BOOST_CHECK_EQUAL(last, size_t(10));
      BOOST_CHECK_EQUAL(step, size_t(2));
    }

    {
      first = 1;
      last = 10;
      step = 2;
      mergeRanges<size_t>(first, last, step, 2, 10, 2);

      BOOST_CHECK_EQUAL(first, size_t(1));
      BOOST_CHECK_EQUAL(last, size_t(10));
      BOOST_CHECK_EQUAL(step, size_t(1));
    }

    {
      first = 1;
      last = 10;
      step = 2;
      mergeRanges<size_t>(first, last, step, 4, 10, 2);

      BOOST_CHECK_EQUAL(first, size_t(1));
      BOOST_CHECK_EQUAL(last, size_t(10));
      BOOST_CHECK_EQUAL(step, size_t(1));
    }

    {
      first = 1;
      last = 10;
      step = 2;
      mergeRanges<size_t>(first, last, step, 1, 10, 2);

      BOOST_CHECK_EQUAL(first, size_t(1));
      BOOST_CHECK_EQUAL(last, size_t(10));
      BOOST_CHECK_EQUAL(step, size_t(2));
    }

    {
      first = 1;
      last = 10;
      step = 2;
      mergeRanges<size_t>(first, last, step, 1, 1, 1);

      BOOST_CHECK_EQUAL(first, size_t(1));
      BOOST_CHECK_EQUAL(last, size_t(10));
      BOOST_CHECK_EQUAL(step, size_t(2));
    }

    {
      first = 1;
      last = 10;
      step = 2;
      mergeRanges<size_t>(first, last, step, 2, 2, 2);

      BOOST_CHECK_EQUAL(first, size_t(1));
      BOOST_CHECK_EQUAL(last, size_t(10));
      BOOST_CHECK_EQUAL(step, size_t(1));
    }

    {
      first = 1;
      last = 10;
      step = 2;
      mergeRanges<size_t>(first, last, step, 15, 16, 2);

      BOOST_CHECK_EQUAL(first, size_t(1));
      BOOST_CHECK_EQUAL(last, size_t(16));
      BOOST_CHECK_EQUAL(step, size_t(2));
    }

    {
      first = 1;
      last = 10;
      step = 2;
      mergeRanges<size_t>(first, last, step, 16, 17, 1);

      BOOST_CHECK_EQUAL(first, size_t(1));
      BOOST_CHECK_EQUAL(last, size_t(17));
      BOOST_CHECK_EQUAL(step, size_t(1));
    }
    {
      first = 1;
      last =  9;
      step =  2;
      mergeRanges<size_t>(first, last, step, 1, 9, 2);

      BOOST_CHECK(comparable<size_t>(first,1));
      BOOST_CHECK(comparable<size_t>(last, 9));
      BOOST_CHECK(comparable<size_t>(step, 2));
    }
  }

  // test made to discover gcdFloat bugs in _MSC_VER release mode
  BOOST_CHECK_EQUAL(gcd<size_t>(2, 2), size_t(2));
  BOOST_CHECK_EQUAL(gcd<size_t>(18, 12), size_t(6));
  BOOST_CHECK(comparable<double>(gcd<double>(0.2, 0.2), 0.2));
  BOOST_CHECK(comparable<float>(gcd<float>(0.2f, 0.2f), 0.2f));
  BOOST_CHECK(comparable<float>(gcd<float>(0.18f, 0.12f), 0.06f));
  BOOST_CHECK(comparable<float>(gcd<float>(0.01f, 0.01f), 0.01f));

  {
    // Floating points tests.
    float first, last, step;

    {
      // Because of bug, only in release mode.
      // result:     /cumulative probabilities[0.01, 0.99, -2.14748e-07]
      first = 0.01f;
      last = 0.99f;
      step = 0.01f;

      mergeRanges<float>(first, last, step, 0.01f, 0.99f, 0.01f);
      BOOST_CHECK(comparable<float>(first, 0.01f));
      BOOST_CHECK(comparable<float>(last, 0.99f));
      BOOST_CHECK(comparable<float>(step, 0.01f));
    }

    {
      first = 0.1f;
      last =  0.9f;
      step =  0.2f;
      mergeRanges<float>(first, last, step, 0.1f, 0.9f, 0.2f);

      BOOST_CHECK(comparable<float>(first, 0.1f));
      BOOST_CHECK(comparable<float>(last, 0.9f));
      BOOST_CHECK(comparable<float>(step, 0.2f));
    }


    {
      first = 0.1f;
      last =  0.9f;
      step =  0.2f;
      mergeRanges<float>(first, last, step, 0.1f, 0.1f, 0.2f);

      BOOST_CHECK(comparable<float>(first, 0.1f));
      BOOST_CHECK(comparable<float>(last, 0.9f));
      BOOST_CHECK(comparable<float>(step, 0.2f));
    }

    {
      first = 0.1f;
      last =  0.9f;
      step =  0.2f;
      mergeRanges<float>(first, last, step, 0.2f, 0.2f, 0.2f);

      BOOST_CHECK(comparable<float>(first, 0.1f));
      BOOST_CHECK(comparable<float>(last, 0.9f));
      BOOST_CHECK(comparable<float>(step, 0.1f));
    }

    {
      first = 0.1f;
      last =  0.9f;
      step =  0.2f;
      mergeRanges<float>(first, last, step, 0.01f, 0.99f, 0.01f);

      BOOST_CHECK(comparable<float>(first, 0.01f));
      BOOST_CHECK(comparable<float>(last, 0.99f));
      BOOST_CHECK(comparable<float>(step, 0.01f));
    }

    {
      first = 0.1f;
      last =  0.9f;
      step =  0.2f;
      mergeRanges<float>(first, last, step, 0.01f, 0.01f, 0.01f);

      BOOST_CHECK(comparable<float>(first, 0.01f));
      BOOST_CHECK(comparable<float>(last, 0.9f));
      BOOST_CHECK(comparable<float>(step, 0.01f));
    }

    {
      first = 0.1f;
      last =  0.9f;
      step =  0.2f;
      mergeRanges<float>(first, last, step, 0.99f, 0.99f, 0.01f);

      BOOST_CHECK(comparable<float>(first, 0.1f));
      BOOST_CHECK(comparable<float>(last, 0.99f));
      BOOST_CHECK(comparable<float>(step, 0.01f));
    }

    {
      first = 0.1f;
      last =  0.9f;
      step =  0.2f;
      mergeRanges<float>(first, last, step, 0.9f, 0.9f, 0.01f);

      BOOST_CHECK(comparable<float>(first, 0.1f));
      BOOST_CHECK(comparable<float>(last, 0.9f));
      // Fails on windows only?
      BOOST_CHECK(comparable<float>(step, 0.2f));
    }
  }
}


BOOST_AUTO_TEST_CASE(round_)
{
  using namespace dal;

  BOOST_CHECK_EQUAL((round<float, int>(0.0f )), 0);
  BOOST_CHECK_EQUAL((round<float, int>(0.4f )), 0);
  BOOST_CHECK_EQUAL((round<float, int>(0.5f )), 0);
  BOOST_CHECK_EQUAL((round<float, int>(0.51f)), 1);
  BOOST_CHECK_EQUAL((round<float, int>(0.6f )), 1);

  BOOST_CHECK_EQUAL((round<float, int>(-0.4f )),  0);
  BOOST_CHECK_EQUAL((round<float, int>(-0.5f )),  0);
  BOOST_CHECK_EQUAL((round<float, int>(-0.51f)), -1);
  BOOST_CHECK_EQUAL((round<float, int>(-0.6f )), -1);

  BOOST_CHECK_EQUAL((round<float, size_t>(0.0f )), size_t(0));
  BOOST_CHECK_EQUAL((round<float, size_t>(0.4f )), size_t(0));
  BOOST_CHECK_EQUAL((round<float, size_t>(0.5f )), size_t(0));
  BOOST_CHECK_EQUAL((round<float, size_t>(0.51f)), size_t(1));
  BOOST_CHECK_EQUAL((round<float, size_t>(0.6f )), size_t(1));

  BOOST_CHECK_EQUAL((round<float, size_t>(-0.4f )), size_t(0));
  BOOST_CHECK_THROW((round<float, size_t>(-0.51f )), std::bad_cast);
}


BOOST_AUTO_TEST_CASE(rintf_)
{
  using namespace dal;

#ifdef _MSC_VER
  bool seeIfVS2005HasRintfC99=false;
  BOOST_WARN(seeIfVS2005HasRintfC99);
#endif
}


BOOST_AUTO_TEST_CASE(value_in_range)
{
  using namespace dal;

  BOOST_CHECK(valueInRange(0, 10, 2, 4));
  BOOST_CHECK(!valueInRange(0, 10, 2, 5));

  BOOST_CHECK(valueInRange(0.0, 1.0, 0.2, 0.4));
  BOOST_CHECK(!valueInRange(0.0, 1.0, 0.2, 0.5));

  BOOST_CHECK(valueInRange(0.0f, 1.0f, 0.2f, 0.4f));
  BOOST_CHECK(!valueInRange(0.0f, 1.0f, 0.2f, 0.5f));
}


BOOST_AUTO_TEST_CASE(clockwise_angle)
{
  using namespace dal;

  BOOST_CHECK_CLOSE(clockwiseAngle( 3.0,  4.0),         36.8699, 0.001);
  BOOST_CHECK_CLOSE(clockwiseAngle( 4.0, -3.0),  90.0 + 36.8699, 0.001);
  BOOST_CHECK_CLOSE(clockwiseAngle(-3.0, -4.0), 180.0 + 36.8699, 0.001);
  BOOST_CHECK_CLOSE(clockwiseAngle(-4.0,  3.0), 270.0 + 36.8699, 0.001);

  BOOST_CHECK_CLOSE(clockwiseAngle( 0.0,  4.0),   0.0, 0.001);
  BOOST_CHECK_CLOSE(clockwiseAngle( 4.0,  0.0),  90.0, 0.001);
  BOOST_CHECK_CLOSE(clockwiseAngle( 0.0, -4.0), 180.0, 0.001);
  BOOST_CHECK_CLOSE(clockwiseAngle(-4.0,  0.0), 270.0, 0.001);

  BOOST_CHECK_CLOSE(clockwiseAngle(0.0, 0.0), 0.0, 0.001);
}
