#define BOOST_TEST_MODULE col2map test suite
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

#include "dev_FilesystemUtils.h"
#include "dal_Client.h"

#ifndef INCLUDED_COL2MAP_COL2MAPTEST
#include "col2map_col2maptest.h"
#define INCLUDED_COL2MAP_COL2MAPTEST
#endif


boost::unit_test::test_suite* init_unit_test_suite(
         int argc,
         char ** const argv) {

  struct TestSuite: public boost::unit_test::test_suite,
                    public dal::Client
  {
    TestSuite(
         int& argc,
         char** argv)
      : boost::unit_test::test_suite("Master test suite"),
        dal::Client(dev::prefix(argv[0]), false)
    {
      assert(dal::Client::isInitialized());
    }
  };

  TestSuite* test = new TestSuite(argc, const_cast<char**>(argv));
  test->add(col2map::Col2MapTest().suite());
  return test;
}
