#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_DRAWPROPERTIESFACTORYTEST
#include "DrawPropertiesFactoryTest.h"
#define INCLUDED_DRAWPROPERTIESFACTORYTEST
#endif

#ifndef INCLUDED_DRAWPROPERTIESMANAGERTEST
#include "DrawPropertiesManagerTest.h"
#define INCLUDED_DRAWPROPERTIESMANAGERTEST
#endif

#ifndef INCLUDED_DRAWPROPERTIESTEST
#include "DrawPropertiesTest.h"
#define INCLUDED_DRAWPROPERTIESTEST
#endif

#ifndef INCLUDED_PALETTETEST
#include "PaletteTest.h"
#define INCLUDED_PALETTETEST
#endif



boost::unit_test::test_suite* init_unit_test_suite(
         int /* argc */,
         char** /* argv */) {

  struct TestSuite: public boost::unit_test::test_suite
  {
    TestSuite()
      : boost::unit_test::test_suite("Master test suite")
    {
    }

    ~TestSuite() {
    }
  };

  TestSuite* test = new TestSuite();

  test->add(ag::PaletteTest::suite());

  test->add(ag::DrawPropertiesTest::suite());
  test->add(ag::DrawPropertiesFactoryTest::suite());
  test->add(ag::DrawPropertiesManagerTest::suite());

  return test;
}

