#define BOOST_TEST_MODULE pcraster qt_client
#include <boost/test/unit_test.hpp>
#include <QCoreApplication>
#include "dev_QtClient.h"


BOOST_AUTO_TEST_CASE(is_initialized)
{
  using namespace dev;

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
