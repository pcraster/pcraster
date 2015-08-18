#include <boost/test/included/unit_test.hpp>
#include <QApplication>
#include "dev_FilesystemUtils.h"
#include "dev_GDalClient.h"
#include "dev_QtClient.h"
#include "dal_Client.h"
#include "ag_AguilaTest.h"
#include "ag_AguilaProgramOptionsTest.h"



/*
#include "ag_classclassifiertest.h"
#include "ag_dummyclassifiertest.h"
#include "ag_rangeclassifiertest.h"
*/

/// #ifndef INCLUDED_COM_BITVECTORTEST
/// #include "com_bitvectortest.h"
/// #define INCLUDED_COM_BITVECTORTEST
/// #endif
#include "com_fileformatinfotest.h"
#include "com_matrixtest.h"
#include "com_rangemaptest.h"
#include "com_rgbtupletest.h"
#include "com_userdefinedclassifiertest.h"
#include "com_utiltest.h"
#include "com_vectortest.h"

// #ifndef INCLUDED_QT_FILEFORMATINFOTEST
// #include "qt_fileformatinfotest.h"
// #define INCLUDED_QT_FILEFORMATINFOTEST
// #endif
#include "ag_ClassifierTest.h"
#include "ag_ColourSelectorTest.h"
#include "DataObjectBaseTest.h"
#include "DataManagerTest.h"



namespace {
  static int argc = 1;
  static char const* argv[1] = { "/my/path/dalTest" };

// There is an interaction between the Boost Unit.Test framework and Qt which
// results in a crash in the destructor of QApplication. It is not
// reproducable in other contexts.
// Workaround is to not use the TestSuite class as a vehicle to create and
// destruct the QtClient class, but some other class and a static global
// variable.
// Important thing for the test code is that QtClient's constructor is run.
// For the memory leakage checks it is relevant that the global variable is
// destructed (which isn't right now).
struct QtClientHack: public dev::QtClient<QApplication>
{
  QtClientHack(
       int& argc,
       char** argv)
    : dev::QtClient<QApplication>(argc, argv)
  {
    assert(dev::QtClient<QApplication>::isInitialized());
  }
};

// This won't work either...
// static QtClientHack qtClientHack(argc, const_cast<char**>(argv));

// This won't work either...
// static std::auto_ptr<QtClientHack> qtClientHack(new QtClientHack(argc,
//          const_cast<char**>(argv)));

// Let is dangle for now...
static QtClientHack* qtClientHack(new QtClientHack(argc,
         const_cast<char**>(argv)));

}



boost::unit_test::test_suite* init_unit_test_suite(
         int /* argc */,
         char **const /* argv */)
{
  // boost::unit_test::test_suite* test = BOOST_TEST_SUITE("Master test suite");

  struct TestSuite: public boost::unit_test::test_suite,
                    public dev::GDalClient,
                    // public dev::QtClient<QApplication>,
                    public dal::Client
  {
    TestSuite(
         int& /* argc */,
         char** argv)
      : boost::unit_test::test_suite("Master test suite"),
        dev::GDalClient(),
        // dev::QtClient<QApplication>(argc, argv),
        dal::Client(dev::prefix(argv[0]), true)
    {
      assert(dev::GDalClient::isInitialized());
      // assert(dev::QtClient<QApplication>::isInitialized());
      assert(dal::Client::isInitialized());
    }
  };

  TestSuite* test = new TestSuite(::argc, const_cast<char**>(::argv));

  /// test->add(com::BitVectorTest::suite());
  test->add(com::MatrixTest::suite());
  test->add(com::FileFormatInfoTest::suite());
  test->add(com::UserDefinedClassifierTest::suite());
  test->add(com::RangeMapTest::suite());
  test->add(com::RgbTupleTest::suite());
  test->add(com::UtilTest::suite());
  test->add(com::VectorTest::suite());

  test->add(ag::ClassifierTest::suite());
  test->add(ag::ColourSelectorTest::suite());

  test->add(ag::DataManagerTest::suite());
  test->add(ag::DataObjectBaseTest::suite());

  test->add(ag::AguilaProgramOptionsTest::suite());

  // TODO Fails on linux32 test->add(ag::AguilaTest::suite());


  /*
  test->add(ag::DummyClassifierTest::suite());
  test->add(ag::ClassClassifierTest::suite());
  test->add(ag::RangeClassifierTest::suite());
         */
  // test->add(qt::FileFormatInfoTest::suite());

  return test;
}


