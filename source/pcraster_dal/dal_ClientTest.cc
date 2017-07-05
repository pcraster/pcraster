#define BOOST_TEST_MODULE pcraster dal client
#include <boost/test/unit_test.hpp>
#include "dal_Client.h"

class ClientWrapper : public dal::Client {
public:
  ClientWrapper(boost::filesystem::path const& prefix,
                   bool addAllDrivers=false,
                   bool cacheDatasetInfo=true)
  : dal::Client(prefix) {
  }
};


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  // These statements should work without a problem.

  // Instantiate clients in sequence.
  {
    ClientWrapper client("/usr/bin/dal");
    BOOST_CHECK(client.isInitialized());
  }

  {
    ClientWrapper client("/usr/bin/dal");
    BOOST_CHECK(client.isInitialized());
  }

  // Instantiate clients in parallel.
  {
    ClientWrapper client1("/usr/bin/dal");
    ClientWrapper client2("/usr/bin/dal");

    BOOST_CHECK(client1.isInitialized());
    BOOST_CHECK(client2.isInitialized());
  }
}
