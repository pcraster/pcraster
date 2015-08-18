// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_PCRXSD_LIBRARY
#include "pcrxsd_library.h"
#define INCLUDED_PCRXSD_LIBRARY
#endif
#ifndef INCLUDED_PCRXSD_XSDTEST
#include "pcrxsd_xsdtest.h"
#define INCLUDED_PCRXSD_XSDTEST
#endif
#ifndef INCLUDED_PCRXSD_UTILSTEST
#include "pcrxsd_utilstest.h"
#define INCLUDED_PCRXSD_UTILSTEST
#endif
#ifndef INCLUDED_PCRXSD_DOMINPUTTEST
#include "pcrxsd_dominputtest.h"
#define INCLUDED_PCRXSD_DOMINPUTTEST
#endif
#ifndef INCLUDED_PCRXSD_COMMONTYPESTEST
#include "pcrxsd_commontypestest.h"
#define INCLUDED_PCRXSD_COMMONTYPESTEST
#endif
#ifndef INCLUDED_PCRXSD_DOMALGORITHMTEST
#include "pcrxsd_DomAlgorithmTest.h"
#define INCLUDED_PCRXSD_DOMALGORITHMTEST
#endif

boost::unit_test::test_suite* init_unit_test_suite(int /*argc*/, char ** const /*argv*/)
{
    struct TestSuite : public boost::unit_test::test_suite {
     pcrxsd::Library *lib;
     TestSuite():
      boost::unit_test::test_suite("pcrxsd test suite"),
      lib(new pcrxsd::Library())
     {}
     ~TestSuite() {
      delete lib;
     }
    };

    TestSuite* test = new TestSuite();

    test->add(pcrxsd::UtilsTest().suite());
    test->add(pcrxsd::DOMInputTest().suite());
    test->add(pcrxsd::XsdTest().suite());
    test->add(pcrxsd::CommonTypesTest().suite());
    test->add(pcrxsd::DomAlgorithmTest().suite());

    return test;
}
