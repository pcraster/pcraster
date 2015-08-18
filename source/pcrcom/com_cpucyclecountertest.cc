#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_CPUCYCLECOUNTERTEST
#include "com_cpucyclecountertest.h"
#define INCLUDED_COM_CPUCYCLECOUNTERTEST
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
#ifndef INCLUDED_COM_CPUCYCLECOUNTER
#include "com_cpucyclecounter.h"
#define INCLUDED_COM_CPUCYCLECOUNTER
#endif



/*!
  \file
  This file contains the implementation of the CpuCycleCounterTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CPUCYCLECOUNTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::CpuCycleCounterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CpuCycleCounterTest> instance(new CpuCycleCounterTest());

#ifndef _MSC_VER // OK
  suite->add(BOOST_CLASS_TEST_CASE(&CpuCycleCounterTest::test, instance));
#endif

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CPUCYCLECOUNTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::CpuCycleCounterTest::CpuCycleCounterTest()
{
}



//! setUp
void com::CpuCycleCounterTest::setUp()
{
}



//! tearDown
void com::CpuCycleCounterTest::tearDown()
{
}

void com::CpuCycleCounterTest::test()
{
  CpuCycleCounter c;
  // get big value by "starting" on cpu epoch
  //  hence trust on init 0 by ctor instead
  //   of starting and waiting "long"
  BOOST_CHECK(c.d_counters[0].d_start==0);
  c.stop(0);
  BOOST_CHECK(c.counters(10000)[0] < 10000);
  BOOST_CHECK(c.counters(100  )[0] < 100);
}
