#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_ALGORITHMTEST
#include "com_algorithmtest.h"
#define INCLUDED_COM_ALGORITHMTEST
#endif

#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
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

// Module headers.



/*!
  \file
  This file contains the implementation of the AlgorithmTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ALGORITHM MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::AlgorithmTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<AlgorithmTest> instance(new AlgorithmTest());

  suite->add(BOOST_CLASS_TEST_CASE(&AlgorithmTest::testHasElement, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&AlgorithmTest::testSeq0123etc, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&AlgorithmTest::testFindValue, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ALGORITHM MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::AlgorithmTest::AlgorithmTest(
         )
{
}



//! setUp
void com::AlgorithmTest::setUp()
{
}

//! tearDown
void com::AlgorithmTest::tearDown()
{
}



void com::AlgorithmTest::testHasElement()
{
  std::vector<int> v1;
  BOOST_CHECK(!hasElement(v1,9));
  v1.push_back(9);
  BOOST_CHECK(hasElement(v1,9));
}

void com::AlgorithmTest::testSeq0123etc()
{
  {
    size_t buf[6];
    std::generate_n(buf,6, SeqInc<size_t>());
    BOOST_CHECK(buf[0]==0);
    BOOST_CHECK(buf[4]==4);
    BOOST_CHECK(buf[5]==5);
  }
  {
    double buf[6];
    SeqInc<double> ts(-1, 0.5);
    std::generate_n(buf,6, ts);
    BOOST_CHECK(buf[0]==-1.0);
    BOOST_CHECK(buf[1]==-0.5);
    BOOST_CHECK(buf[4]==1.0);
  }
  {
    double buf[6];
    std::generate_n(buf,6, SeqInc<double>(-1, 0.5));
    BOOST_CHECK(buf[0]==-1.0);
    BOOST_CHECK(buf[1]==-0.5);
    BOOST_CHECK(buf[4]== 1.0);
  }
  {
    double buf[6];
    std::generate_n(buf,6, SeqInc<double>(-2));
    BOOST_CHECK(buf[0]==-2.0);
    BOOST_CHECK(buf[1]==-1.0);
    BOOST_CHECK(buf[4]== 2.0);
  }
}

void com::AlgorithmTest::testFindValue()
{
  std::map<std::string,std::string> str;
  FindValue<std::string,std::string> fStr("xx");

  BOOST_CHECK(fStr.find(str,std::string("key")) == "xx");

  str.insert(std::make_pair(std::string("KEY1"),std::string("VAL1")));
  BOOST_CHECK(fStr.find(str,std::string("KEY1")) == "VAL1");

  std::map<std::string,double> dob;
  FindValue<std::string,double> dStr(4);
  BOOST_CHECK(dStr.find(dob,std::string("key")) == 4);
  dob.insert(std::make_pair(std::string("KEY1"),1));
  BOOST_CHECK(dStr.find(dob,std::string("KEY1")) == 1);
}
