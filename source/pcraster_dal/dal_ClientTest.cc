#ifndef INCLUDED_DAL_CLIENTTEST
#include "dal_ClientTest.h"
#define INCLUDED_DAL_CLIENTTEST
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

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif



/*!
  \file
  This file contains the implementation of the ClientTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLIENTTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* ClientTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ClientTest> instance(new ClientTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &ClientTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLIENTTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
ClientTest::ClientTest()
{
}



void ClientTest::test()
{
  // These statements should work without a problem.

  // Instantiate clients in sequence.
  {
    Client client("/usr/bin/dal");
    BOOST_CHECK(client.isInitialized());
  }

  {
    Client client("/usr/bin/dal");
    BOOST_CHECK(client.isInitialized());
  }

  // Instantiate clients in parallel.
  {
    Client client1("/usr/bin/dal");
    Client client2("/usr/bin/dal");

    BOOST_CHECK(client1.isInitialized());
    BOOST_CHECK(client2.isInitialized());
  }
}

} // namespace dal

