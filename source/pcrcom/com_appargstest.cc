#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_APPARGSTEST
#include "com_appargstest.h"
#define INCLUDED_COM_APPARGSTEST
#endif

#ifndef INCLUDED_COM_APPARGS
#include "com_appargs.h"
#define INCLUDED_COM_APPARGS
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_STDEXCEP
#include <stdexcept>
#define INCLUDED_STDEXCEP
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



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*com::AppArgsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<AppArgsTest> instance(new AppArgsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&AppArgsTest::testArgvArgc, instance));
  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

com::AppArgsTest::AppArgsTest()
{
}

void com::AppArgsTest::setUp()
{
}

void com::AppArgsTest::tearDown()
{
}

void com::AppArgsTest::testArgvArgc()
{
 {
  com::AppArgs in1("no whe at end");
  char **argv=in1.argv();
  BOOST_CHECK(in1.argc()==4);
  BOOST_CHECK(argv[0] == std::string("no"));
  BOOST_CHECK(argv[1] == std::string("whe"));
  BOOST_CHECK(argv[2] == std::string("at"));
  BOOST_CHECK(argv[3] == std::string("end"));
 }
 {
  com::AppArgs in1("no"," whe at end");
  char **argv=in1.argv();
  BOOST_CHECK(in1.argc()==4);
  BOOST_CHECK(argv[0] == std::string("no"));
  BOOST_CHECK(argv[1] == std::string("whe"));
  BOOST_CHECK(argv[2] == std::string("at"));
  BOOST_CHECK(argv[3] == std::string("end"));
 }
}
