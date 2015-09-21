#define BOOST_TEST_MODULE tab test suite
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif


#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif
#ifndef INCLUDED_TAB_CLASSCOUNTMAPTEST
#include "tab_classcountmaptest.h"
#define INCLUDED_TAB_CLASSCOUNTMAPTEST
#endif
#ifndef INCLUDED_TAB_CLASSINTERVALMAPTEST
#include "tab_classintervalmaptest.h"
#define INCLUDED_TAB_CLASSINTERVALMAPTEST
#endif


boost::unit_test::test_suite* init_unit_test_suite(int /*argc*/, char ** const /*argv*/)
{
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(tab::ClassCountMapTest().suite());
  test->add(tab::ClassIntervalMapTest().suite());

  return test;
}

