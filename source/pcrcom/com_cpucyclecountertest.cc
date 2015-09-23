#define BOOST_TEST_MODULE pcraster com cpu_cycle_counter
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#define private public  // Testing private parts too.
#include "com_cpucyclecounter.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace com;

  CpuCycleCounter c;
  // get big value by "starting" on cpu epoch
  //  hence trust on init 0 by ctor instead
  //   of starting and waiting "long"
  BOOST_CHECK(c.d_counters[0].d_start==0);
  c.stop(0);
  BOOST_CHECK(c.counters(10000)[0] < 10000);
  BOOST_CHECK(c.counters(100  )[0] < 100);
}
