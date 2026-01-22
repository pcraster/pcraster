#define BOOST_TEST_MODULE pcraster dal connection_info
#include <boost/test/unit_test.hpp>
#include "dev_Utils.h"
#include "dal_ConnectionInfo.h"
#include "dal_Exception.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  std::string user;

#ifdef WIN32
  BOOST_TEST_REQUIRE(dev::environmentVariableSet("USERNAME"));
  user = dev::environmentVariable("USERNAME");
#else
  BOOST_TEST_REQUIRE(dev::environmentVariableSet("LOGNAME"));
  user = dev::environmentVariable("LOGNAME");
#endif

  BOOST_TEST_REQUIRE(!user.empty());

  if(user.find(' ') != std::string::npos) {
    // We don't support user names with spaces in them yet.
    return;
  }

  // Valid examples ------------------------------------------------------------
  {
    ConnectionInfo const info(user + "(348uk)@colossal:randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == user);
    BOOST_TEST(info.password() == "348uk");
    BOOST_TEST(info.host() == "colossal");
    BOOST_TEST(info.database() == "randstad");
    BOOST_TEST(info.table() == "households");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == user + "(348uk)@colossal:randstad/households");
  }

  {
    ConnectionInfo const info(user + "()@colossal:randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == user);
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "colossal");
    BOOST_TEST(info.database() == "randstad");
    BOOST_TEST(info.table() == "households");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == user + "@colossal:randstad/households");
  }

  {
    ConnectionInfo const info(user + "@colossal:randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == user);
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "colossal");
    BOOST_TEST(info.database() == "randstad");
    BOOST_TEST(info.table() == "households");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == user + "@colossal:randstad/households");
  }

  {
    ConnectionInfo const info(user + "(348uk):randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == user);
    BOOST_TEST(info.password() == "348uk");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == "randstad");
    BOOST_TEST(info.table() == "households");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == user + "(348uk):randstad/households");
  }

  {
    ConnectionInfo const info(user + "():randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == user);
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == "randstad");
    BOOST_TEST(info.table() == "households");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == user + ":randstad/households");
  }

  {
    ConnectionInfo const info(user + ":randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == user);
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == "randstad");
    BOOST_TEST(info.table() == "households");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == user + ":randstad/households");
  }

  {
    ConnectionInfo const info("@colossal:randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == "");
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "colossal");
    BOOST_TEST(info.database() == "randstad");
    BOOST_TEST(info.table() == "households");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == "@colossal:randstad/households");
  }

  {
    ConnectionInfo const info("randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == "");
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == "randstad");
    BOOST_TEST(info.table() == "households");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == "randstad/households");
  }

  {
    ConnectionInfo const info("randstad");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == "");
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == "randstad");
    BOOST_TEST(info.table() == "");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == "randstad");
  }

  {
    ConnectionInfo const info("extensiontest.sql3");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == "");
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == "extensiontest.sql3");
    BOOST_TEST(info.table() == "");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == "extensiontest.sql3");
  }

  {
    ConnectionInfo const info(user + ":randstad1995/grid_info");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == user);
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == "randstad1995");
    BOOST_TEST(info.table() == "grid_info");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == user + ":randstad1995/grid_info");
  }

  {
    ConnectionInfo const info(user + "@randstad");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == "");
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == user + "@randstad");
    BOOST_TEST(info.table() == "");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == user + "@randstad");
  }

  {
    ConnectionInfo const info(user + "@");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == "");
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == user + "@");
    BOOST_TEST(info.table() == "");
    BOOST_TEST(info.fields().empty());
    BOOST_TEST(info.name() == user + "@");
  }

  {
    ConnectionInfo const info("temporal/co2/co2");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == "");
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == "temporal");
    BOOST_TEST(info.table() == "co2");
    BOOST_REQUIRE_EQUAL(info.fields().size(), size_t(1));
    BOOST_TEST(info.fields()[0] == "co2");
    BOOST_TEST(info.name() == "temporal/co2");
  }

  {
    ConnectionInfo const info("temporal/co2/{fid,co2}");
    BOOST_TEST(info.isValid());
    BOOST_TEST(info.user() == "");
    BOOST_TEST(info.password() == "");
    BOOST_TEST(info.host() == "");
    BOOST_TEST(info.database() == "temporal");
    BOOST_TEST(info.table() == "co2");
    BOOST_REQUIRE_EQUAL(info.fields().size(), size_t(2));
    BOOST_TEST(info.fields()[0] == "fid");
    BOOST_TEST(info.fields()[1] == "co2");
    BOOST_TEST(info.name() == "temporal/co2");
  }

  // Invalid examples ----------------------------------------------------------
  // {
  //   ConnectionInfo info(user + "@randstad");
  //   BOOST_TEST(info.isValid());
  // }

  // {
  //   ConnectionInfo info(user + "@");
  //   BOOST_TEST(!info.isValid());
  // }

  {
    ConnectionInfo const info("");
    BOOST_TEST(!info.isValid());
  }
}
