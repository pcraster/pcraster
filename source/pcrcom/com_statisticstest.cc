#define BOOST_TEST_MODULE pcraster com statistics
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <cmath>
#include "com_algorithm.h"
#include "com_statistics.h"

// KDJ, 20150928: Some tests used to be excluded on certain platforms:

// #ifndef __x86_64__
//   // bugzilla #80
//   suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testVariance1, instance));
//   suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testVariance2, instance));
//   suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testStandardDeviation, instance));
//   suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testPercentile, instance));
// #else
//   suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testSuse, instance));
// #endif


namespace com
{
namespace statisticsTest
{
struct Reverse {
  bool operator()(const double &e1, const double &e2) const
  {
    return e1 > e2;
  }
};
}  // namespace statisticsTest
}  // namespace com

BOOST_AUTO_TEST_CASE(sum)
{
  using namespace com;

  int values[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  const size_t nrValues = ARRAY_SIZE(values);

  Sum<int> sum = std::for_each(values, values + nrValues, Sum<int>());

  BOOST_TEST(sum == 55);
  BOOST_TEST(sum.sum() == 55);
}

BOOST_AUTO_TEST_CASE(sum_nr)
{
  using namespace com;

  int values[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  const size_t nrValues = ARRAY_SIZE(values);
  SumNr<int> sum = std::for_each(values, values + nrValues, SumNr<int>());

  BOOST_TEST(sum.sum() == 55);
  BOOST_TEST(sum.nr() == nrValues);
}

BOOST_AUTO_TEST_CASE(average)
{
  using namespace com;

  double values[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  const size_t nrValues = ARRAY_SIZE(values);
  Average<> a = std::for_each(values, values + nrValues, Average<>());

  BOOST_TEST(a.sum() == 55);
  BOOST_TEST(a.nr() == nrValues);
  BOOST_TEST(a.average() == 5.5);

  Average<double> empty;
  BOOST_TEST(empty.nr() == 0);
  BOOST_TEST(empty.average(-1) == -1);
}

BOOST_AUTO_TEST_CASE(average_min_max)
{
  using namespace com;

  {
    double values[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    const size_t nrValues = ARRAY_SIZE(values);
    AverageMinMax<double> a = std::for_each(values, values + nrValues, AverageMinMax<double>());

    BOOST_TEST(a.sum() == 55);
    BOOST_TEST(a.nr() == nrValues);
    BOOST_TEST(a.average() == 5.5);
    BOOST_TEST(a.minimum() == 1);
    BOOST_TEST(a.maximum() == 10);
  }
  {
    double values[] = {6, 2, 10, 4, 5, 1, 7, 8, 9, 3};
    const size_t nrValues = ARRAY_SIZE(values);
    AverageMinMax<double> a = std::for_each(values, values + nrValues, AverageMinMax<double>());

    BOOST_TEST(a.sum() == 55);
    BOOST_TEST(a.nr() == nrValues);
    BOOST_TEST(a.average() == 5.5);
    BOOST_TEST(a.minimum() == 1);
    BOOST_TEST(a.maximum() == 10);
  }
}

BOOST_AUTO_TEST_CASE(variance_1)
{
  using namespace com;

  // Empty collection.
  {
    double *values = nullptr;
    const size_t nrValues = 0u;  // ARRAY_SIZE requires non-empty array.
    Variance1<double> variance = std::for_each(values, values + nrValues, Variance1<double>(0.0));
    BOOST_TEST(variance == 0.0);
  }

  // Collection with equal values.
  {
    double values[] = {3, 3, 3, 3, 3, 3, 3, 3};
    const size_t nrValues = ARRAY_SIZE(values);
    Variance1<double> variance = std::for_each(values, values + nrValues, Variance1<double>(3.0));
    BOOST_TEST(variance == 0.0);
  }

  // Collection with different values.
  {
    double values[] = {2, 2, 2, 2, 2.5, 3, 3, 3, 3};
    const size_t nrValues = ARRAY_SIZE(values);
    Variance1<double> variance = std::for_each(values, values + nrValues, Variance1<double>(2.5));
    BOOST_TEST(variance == 0.25);
  }
}

BOOST_AUTO_TEST_CASE(variance_2)
{
  using namespace com;

  // Empty collection.
  {
    double *values = nullptr;
    const size_t nrValues = 0u;  // ARRAY_SIZE requires non-empty array.
    Variance2<double> variance = std::for_each(values, values + nrValues, Variance2<double>());
    BOOST_TEST(variance == 0.0);
  }

  // Collection with equal values.
  {
    double values[] = {3, 3, 3, 3, 3, 3, 3, 3};
    const size_t nrValues = ARRAY_SIZE(values);
    Variance2<double> variance = std::for_each(values, values + nrValues, Variance2<double>());
    BOOST_TEST(variance == 0.0);
  }

  // Collection with different values.
  {
    double values[] = {2, 2, 2, 2, 2.5, 3, 3, 3, 3};
    const size_t nrValues = ARRAY_SIZE(values);
    Variance2<double> variance = std::for_each(values, values + nrValues, Variance2<double>());
    BOOST_TEST(variance == 0.25);
  }
}

BOOST_AUTO_TEST_CASE(standard_deviation)
{
  using namespace com;

  // Empty collection.
  {
    double *values = nullptr;
    const size_t nrValues = 0u;  // ARRAY_SIZE requires non-empty array.
    StandardDeviation<double> stdDev =
        std::for_each(values, values + nrValues, StandardDeviation<double>());
    BOOST_TEST(stdDev == 0.0);
  }

  // Collection with equal values.
  {
    double values[] = {3, 3, 3, 3, 3, 3, 3, 3};
    const size_t nrValues = ARRAY_SIZE(values);
    StandardDeviation<double> stdDev =
        std::for_each(values, values + nrValues, StandardDeviation<double>());
    BOOST_TEST(stdDev == 0.0);
  }

  // Collection with different values.
  {
    double values[] = {2, 2, 2, 2, 2.5, 3, 3, 3, 3};
    const size_t nrValues = ARRAY_SIZE(values);
    StandardDeviation<double> stdDev =
        std::for_each(values, values + nrValues, StandardDeviation<double>());
    BOOST_TEST(stdDev == 0.5);
  }
}

BOOST_AUTO_TEST_CASE(average_sd_min_max)
{
  using namespace com;

  // Collection with different values.
  {
    double values[] = {2, 2, 2, 2, 2.5, 3, 3, 3, 3};
    const size_t nrValues = ARRAY_SIZE(values);
    AverageSdMinMax<double> asmm = std::for_each(values, values + nrValues, AverageSdMinMax<double>());
    BOOST_TEST(asmm.sd() == 0.5);
    BOOST_TEST(asmm.average() == 2.5);
    BOOST_TEST(asmm.minimum() == 2.0);
    BOOST_TEST(asmm.maximum() == 3.0);
  }

  // Empty collection.
  {

    double values[1] = {99999};
    AverageSdMinMax<double> asmm = std::for_each(values, values + 0, AverageSdMinMax<double>());
    BOOST_TEST(asmm.sd() == 0.0);
    BOOST_TEST(asmm.nr() == 0);
    BOOST_TEST(asmm.average(5) == 5);
  }

  // Collection with equal values.
  {
    double values[] = {3, 3, 3, 3, 3, 3, 3, 3};
    const size_t nrValues = ARRAY_SIZE(values);
    AverageSdMinMax<double> asmm = std::for_each(values, values + nrValues, AverageSdMinMax<double>());
    BOOST_TEST(asmm.sd() == 0.0);
    BOOST_TEST(asmm.average() == 3);
    BOOST_TEST(asmm.minimum() == 3);
    BOOST_TEST(asmm.maximum() == 3);
  }
}

BOOST_AUTO_TEST_CASE(percentile_)
{
#ifndef __x86_64__
  using namespace com;

  typedef std::vector<double> T;
  typedef T::iterator I;
  {
    double data[] = {1, 3, 2, 5, 4, 9, 8, 0, 6, 7};
    T val;
    std::copy(data, data + ARRAY_SIZE(data), std::back_inserter(val));

    // out of range
    BOOST_TEST(percentile<I>(val.begin(), val.end(), 5) == val.end());
    BOOST_TEST(percentile<I>(val.begin(), val.end(), -123) == val.end());

    BOOST_TEST(*percentile<I>(val.begin(), val.end(), 0) == 0);
    BOOST_TEST(percentile<I>(val.begin(), val.end(), 0) == val.begin());
    BOOST_TEST(*percentile<I>(val.begin(), val.end(), 0.005) == 0);
    BOOST_TEST(percentile<I>(val.begin(), val.end(), 0.005) == val.begin());

    BOOST_TEST(*percentile<I>(val.begin(), val.end(), 1) == val.size() - 1);
    BOOST_TEST(percentile<I>(val.begin(), val.end(), 1) == val.end() - 1);
    BOOST_TEST(*percentile<I>(val.begin(), val.end(), 0.9994) == val.size() - 1);
    BOOST_TEST(percentile<I>(val.begin(), val.end(), 0.9994) == val.end() - 1);

#ifdef _MSC_VER
    // bugzilla #80
    BOOST_TEST_WARN(*percentile<I>(val.begin(), val.end(), 0.6) == 5);
#else
    BOOST_TEST(*percentile<I>(val.begin(), val.end(), 0.6) == 5);
#endif

    BOOST_TEST(*percentile<I>(val.begin(), val.end(), 0.47) == 4);

    BOOST_TEST(*percentile<I>(val.begin(), val.end(), 0, statisticsTest::Reverse()) == 9);
    BOOST_TEST(*percentile<I>(val.begin(), val.end(), 1, statisticsTest::Reverse()) == 0);

#ifdef _MSC_VER
    // bugzilla #80
    BOOST_TEST_WARN(*percentile<I>(val.begin(), val.end(), 0.6, statisticsTest::Reverse()) == 4);
#else
    BOOST_TEST(*percentile<I>(val.begin(), val.end(), 0.6, statisticsTest::Reverse()) == 4);
#endif
  }
  {
    T e;
    BOOST_TEST(percentile<I>(e.begin(), e.end(), 0) == e.end());
    BOOST_TEST(percentile<I>(e.begin(), e.end(), 0.005) == e.end());
    BOOST_TEST(percentile<I>(e.begin(), e.end(), 1) == e.end());
    BOOST_TEST(percentile<I>(e.begin(), e.end(), 0.9994) == e.end());
  }
  {
    T e;
    e.push_back(4);
    BOOST_TEST(percentile<I>(e.begin(), e.end(), 0) == e.begin());
    BOOST_TEST(percentile<I>(e.begin(), e.end(), 0.005) == e.begin());
    BOOST_TEST(percentile<I>(e.begin(), e.end(), 0.5) == e.begin());
    BOOST_TEST(percentile<I>(e.begin(), e.end(), 1) == e.begin());
    BOOST_TEST(percentile<I>(e.begin(), e.end(), 0.9994) == e.begin());
    BOOST_TEST(*percentile<I>(e.begin(), e.end(), 0) == 4);
    BOOST_TEST(*percentile<I>(e.begin(), e.end(), 0.005) == 4);
    BOOST_TEST(*percentile<I>(e.begin(), e.end(), 1) == 4);
    BOOST_TEST(*percentile<I>(e.begin(), e.end(), 0.9994) == 4);
  }
#endif
}

BOOST_AUTO_TEST_CASE(suse)
{
  using namespace com;

  bool suse64FailsALot = false;
  BOOST_TEST_WARN(suse64FailsALot);
}
