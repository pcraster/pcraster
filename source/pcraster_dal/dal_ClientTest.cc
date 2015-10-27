#define BOOST_TEST_MODULE pcraster dal client
#include <boost/test/unit_test.hpp>
#define protected public
#include "dal_Client.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

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
