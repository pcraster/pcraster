#define BOOST_TEST_MODULE pcraster dal client
#include <boost/test/unit_test.hpp>
#include "dal_Client.h"

class ClientWrapper : public dal::Client {
public:
  ClientWrapper(std::filesystem::path const& prefix,
                   bool  /*addAllDrivers*/=false,
                   bool  /*cacheDatasetInfo*/=true)
  : dal::Client(prefix) {
  }
};


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  // These statements should work without a problem.

  // Instantiate clients in sequence.
  {
    ClientWrapper const client("/usr/bin/dal");
    BOOST_TEST(client.isInitialized());
  }

  {
    ClientWrapper const client("/usr/bin/dal");
    BOOST_TEST(client.isInitialized());
  }

  // Instantiate clients in parallel.
  {
    ClientWrapper const client1("/usr/bin/dal");
    ClientWrapper const client2("/usr/bin/dal");

    BOOST_TEST(client1.isInitialized());
    BOOST_TEST(client2.isInitialized());
  }
}
