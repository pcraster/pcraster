#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_STATISTICSTEST
#include "com_statisticstest.h"
#define INCLUDED_COM_STATISTICSTEST
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

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
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif

// Module headers.
#ifndef INCLUDED_COM_STATISTICS
#include "com_statistics.h"
#define INCLUDED_COM_STATISTICS
#endif



/*!
  \file
  This file contains the implementation of the StatisticsTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC STATISTICS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::StatisticsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<StatisticsTest> instance(new StatisticsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testSum, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testSumNr, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testAverage, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testAverageMinMax, instance));

#ifndef __x86_64__
  // bugzilla #80
  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testVariance1, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testVariance2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testStandardDeviation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testPercentile, instance));
#else
  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testSuse, instance));
#endif
  suite->add(BOOST_CLASS_TEST_CASE(&StatisticsTest::testAverageSdMinMax, instance));
  return suite;
}

namespace com {
 namespace statisticsTest {
    struct Reverse {
      bool operator()(const double& e1, const double& e2) const
      {
        return e1>e2;
      }
    };
 }
}



//------------------------------------------------------------------------------
// DEFINITION OF STATISTICS MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::StatisticsTest::StatisticsTest()
{
}



//! setUp
void com::StatisticsTest::setUp()
{
}

//! tearDown
void com::StatisticsTest::tearDown()
{
}

void com::StatisticsTest::testSum()
{
  int values[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  const size_t nrValues = ARRAY_SIZE(values);

  Sum<int> sum = std::for_each(values, values + nrValues, Sum<int>());

  BOOST_CHECK(sum == 55);
  BOOST_CHECK(sum.sum() == 55);
}

void com::StatisticsTest::testSumNr()
{
  int values[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  const size_t nrValues = ARRAY_SIZE(values);
  SumNr<int> sum = std::for_each(values, values + nrValues, SumNr<int>());

  BOOST_CHECK(sum.sum() == 55);
  BOOST_CHECK(sum.nr() ==  nrValues);
}

void com::StatisticsTest::testAverage()
{
  double values[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  const size_t nrValues = ARRAY_SIZE(values);
  Average<> a = std::for_each(values, values + nrValues, 
      Average<>());

  BOOST_CHECK(a.sum() == 55);
  BOOST_CHECK(a.nr() ==  nrValues);
  BOOST_CHECK(a.average() ==  5.5);

  Average<double> empty;
  BOOST_CHECK(empty.nr()==0);
  BOOST_CHECK(empty.average(-1)==-1);
}

void com::StatisticsTest::testAverageMinMax()
{
  {
    double values[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    const size_t nrValues = ARRAY_SIZE(values);
    AverageMinMax<double> a = std::for_each(values, values + nrValues, 
        AverageMinMax<double>());

    BOOST_CHECK(a.sum() == 55);
    BOOST_CHECK(a.nr() ==  nrValues);
    BOOST_CHECK(a.average() ==  5.5);
    BOOST_CHECK(a.minimum() ==  1);
    BOOST_CHECK(a.maximum() ==  10);
  }
  {
    double values[] = { 6, 2, 10, 4, 5, 1, 7, 8, 9, 3 };
    const size_t nrValues = ARRAY_SIZE(values);
    AverageMinMax<double> a = std::for_each(values, values + nrValues, 
        AverageMinMax<double>());

    BOOST_CHECK(a.sum() == 55);
    BOOST_CHECK(a.nr() ==  nrValues);
    BOOST_CHECK(a.average() ==  5.5);
    BOOST_CHECK(a.minimum() ==  1);
    BOOST_CHECK(a.maximum() ==  10);
  }
}

void com::StatisticsTest::testVariance1()
{
  // Empty collection.
  {
    double *values = 0;
    const size_t nrValues = 0u;  // ARRAY_SIZE requires non-empty array.
    Variance1<double> variance = std::for_each(values, values + nrValues,
                   Variance1<double>(0.0));
    BOOST_CHECK(variance == 0.0);
  }

  // Collection with equal values.
  {
    double values[] = { 3, 3, 3, 3, 3, 3, 3, 3 };
    const size_t nrValues = ARRAY_SIZE(values);
    Variance1<double> variance = std::for_each(values, values + nrValues,
                   Variance1<double>(3.0));
    BOOST_CHECK(variance == 0.0);
  }

  // Collection with different values.
  {
    double values[] = { 2, 2, 2, 2, 2.5, 3, 3, 3, 3 };
    const size_t nrValues = ARRAY_SIZE(values);
    Variance1<double> variance = std::for_each(values, values + nrValues,
                   Variance1<double>(2.5));
    BOOST_CHECK(variance == 0.25);
  }
}



void com::StatisticsTest::testVariance2()
{
  // Empty collection.
  {
    double *values = 0;
    const size_t nrValues = 0u;  // ARRAY_SIZE requires non-empty array.
    Variance2<double> variance = std::for_each(values, values + nrValues,
                   Variance2<double>());
    BOOST_CHECK(variance == 0.0);
  }

  // Collection with equal values.
  {
    double values[] = { 3, 3, 3, 3, 3, 3, 3, 3 };
    const size_t nrValues = ARRAY_SIZE(values);
    Variance2<double> variance = std::for_each(values, values + nrValues,
                   Variance2<double>());
    BOOST_CHECK(variance == 0.0);
  }

  // Collection with different values.
  {
    double values[] = { 2, 2, 2, 2, 2.5, 3, 3, 3, 3 };
    const size_t nrValues = ARRAY_SIZE(values);
    Variance2<double> variance = std::for_each(values, values + nrValues,
                   Variance2<double>());
    BOOST_CHECK(variance == 0.25);
  }
}



void com::StatisticsTest::testStandardDeviation()
{
  // Empty collection.
  {
    double *values = 0;
    const size_t nrValues = 0u;  // ARRAY_SIZE requires non-empty array.
    StandardDeviation<double> stdDev = std::for_each(values,
                   values + nrValues, StandardDeviation<double>());
    BOOST_CHECK(stdDev == 0.0);
  }

  // Collection with equal values.
  {
    double values[] = { 3, 3, 3, 3, 3, 3, 3, 3 };
    const size_t nrValues = ARRAY_SIZE(values);
    StandardDeviation<double> stdDev = std::for_each(values, values + nrValues,
                   StandardDeviation<double>());
    BOOST_CHECK(stdDev == 0.0);
  }

  // Collection with different values.
  {
    double values[] = { 2, 2, 2, 2, 2.5, 3, 3, 3, 3 };
    const size_t nrValues = ARRAY_SIZE(values);
    StandardDeviation<double> stdDev = std::for_each(values, values + nrValues,
                   StandardDeviation<double>());
    BOOST_CHECK(stdDev == 0.5);
  }
}

void com::StatisticsTest::testAverageSdMinMax()
{

  // Collection with different values.
  {
    double values[] = { 2, 2, 2, 2, 2.5, 3, 3, 3, 3 };
    const size_t nrValues = ARRAY_SIZE(values);
    AverageSdMinMax<double> asmm = std::for_each(values, values + nrValues,
                   AverageSdMinMax<double>());
    BOOST_CHECK(asmm.sd() == 0.5);
    BOOST_CHECK(asmm.average() == 2.5);
    BOOST_CHECK(asmm.minimum() == 2.0);
    BOOST_CHECK(asmm.maximum() == 3.0);
  }

  // Empty collection.
  {

    double values[1] = { 99999 };
    AverageSdMinMax<double> asmm = std::for_each(values, values+0,
                   AverageSdMinMax<double>());
    BOOST_CHECK(asmm.sd() == 0.0);
    BOOST_CHECK(asmm.nr() == 0);
    BOOST_CHECK(asmm.average(5) == 5);
   }

  // Collection with equal values.
  {
    double values[] = { 3, 3, 3, 3, 3, 3, 3, 3 };
    const size_t nrValues = ARRAY_SIZE(values);
    AverageSdMinMax<double> asmm = std::for_each(values, values + nrValues,
                   AverageSdMinMax<double>());
    BOOST_CHECK(asmm.sd() == 0.0);
    BOOST_CHECK(asmm.average() == 3);
    BOOST_CHECK(asmm.minimum() == 3);
    BOOST_CHECK(asmm.maximum() == 3);
  }
}

void com::StatisticsTest::testPercentile()
{
  typedef std::vector<double> T;
  typedef T::iterator         I;
 {
  double data[]={1,3,2,5,4,9,8,0,6,7};
  T val;
  std::copy(data,data+ARRAY_SIZE(data),std::back_inserter(val));

  // out of range
  BOOST_CHECK(percentile<I>(val.begin(),val.end(),5) == val.end());
  BOOST_CHECK(percentile<I>(val.begin(),val.end(),-123) == val.end());

  BOOST_CHECK(*percentile<I>(val.begin(),val.end(),0) == 0);
  BOOST_CHECK( percentile<I>(val.begin(),val.end(),0) == val.begin());
  BOOST_CHECK(*percentile<I>(val.begin(),val.end(),0.005) == 0);
  BOOST_CHECK( percentile<I>(val.begin(),val.end(),0.005) == val.begin());

  BOOST_CHECK(*percentile<I>(val.begin(),val.end(),1) == val.size()-1);
  BOOST_CHECK( percentile<I>(val.begin(),val.end(),1) == val.end()-1);
  BOOST_CHECK(*percentile<I>(val.begin(),val.end(),0.9994) == val.size()-1);
  BOOST_CHECK( percentile<I>(val.begin(),val.end(),0.9994) == val.end()-1);

#ifdef _MSC_VER
  // bugzilla #80
  BOOST_WARN(*percentile<I>(val.begin(),val.end(),0.6)  == 5);
#else
  BOOST_CHECK(*percentile<I>(val.begin(),val.end(),0.6)  == 5);
#endif

  BOOST_CHECK(*percentile<I>(val.begin(),val.end(),0.47) == 4);

  BOOST_CHECK(*percentile<I>(val.begin(),val.end(),0,
                         statisticsTest::Reverse())  == 9);
  BOOST_CHECK(*percentile<I>(val.begin(),val.end(),1,
                         statisticsTest::Reverse())  == 0);

#ifdef _MSC_VER
  // bugzilla #80
  BOOST_WARN(*percentile<I>(val.begin(),val.end(),0.6,
                         statisticsTest::Reverse())  == 4);
#else
  BOOST_CHECK(*percentile<I>(val.begin(),val.end(),0.6,
                         statisticsTest::Reverse())  == 4);
#endif
 }
 {
  T e;
  BOOST_CHECK( percentile<I>(e.begin(),e.end(),0)      == e.end());
  BOOST_CHECK( percentile<I>(e.begin(),e.end(),0.005)  == e.end());
  BOOST_CHECK( percentile<I>(e.begin(),e.end(),1)      == e.end());
  BOOST_CHECK( percentile<I>(e.begin(),e.end(),0.9994) == e.end());
 }
 {
  T e;
  e.push_back(4);
  BOOST_CHECK( percentile<I>(e.begin(),e.end(),0)      == e.begin());
  BOOST_CHECK( percentile<I>(e.begin(),e.end(),0.005)  == e.begin());
  BOOST_CHECK( percentile<I>(e.begin(),e.end(),0.5)    == e.begin());
  BOOST_CHECK( percentile<I>(e.begin(),e.end(),1)      == e.begin());
  BOOST_CHECK( percentile<I>(e.begin(),e.end(),0.9994) == e.begin());
  BOOST_CHECK(*percentile<I>(e.begin(),e.end(),0)      == 4);
  BOOST_CHECK(*percentile<I>(e.begin(),e.end(),0.005)  == 4);
  BOOST_CHECK(*percentile<I>(e.begin(),e.end(),1)      == 4);
  BOOST_CHECK(*percentile<I>(e.begin(),e.end(),0.9994) == 4);
 }
}

void com::StatisticsTest::testSuse()
{
  bool suse64FailsALot=false;
  BOOST_WARN(suse64FailsALot);
}
