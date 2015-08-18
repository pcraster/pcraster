#ifndef INCLUDED_DEV_QTCLIENTTEST
#include "dev_QtClientTest.h"
#define INCLUDED_DEV_QTCLIENTTEST
#endif

// External headers.
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

#ifndef INCLUDED_QCOREAPPLICATION
#include <QCoreApplication>
#define INCLUDED_QCOREAPPLICATION
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_DEV_QTCLIENT
#include "dev_QtClient.h"
#define INCLUDED_DEV_QTCLIENT
#endif



/*!
  \file
  This file contains the implementation of the QtClientTest class.
*/



namespace dev {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC QTCLIENTTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* QtClientTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<QtClientTest> instance(
         new QtClientTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &QtClientTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF QTCLIENTTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
QtClientTest::QtClientTest()
{
}



void QtClientTest::test()
{
  struct Client: public dev::QtClient<QCoreApplication>
  {
    Client(
         int argc,
         char** argv)
      : dev::QtClient<QCoreApplication>(argc, argv)
    {
    }
  };

  static int const argc = 1;
  static char const* argv[1] = { "/my/dir/test.exe" };

  Client client(argc, const_cast<char**>(argv));

  BOOST_CHECK(client.isInitialized());
}

} // namespace dev

