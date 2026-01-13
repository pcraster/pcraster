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
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "348uk");
    BOOST_CHECK_EQUAL(info.host(), "colossal");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "(348uk)@colossal:randstad/households");
  }

  {
    ConnectionInfo const info(user + "()@colossal:randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "colossal");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "@colossal:randstad/households");
  }

  {
    ConnectionInfo const info(user + "@colossal:randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "colossal");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "@colossal:randstad/households");
  }

  {
    ConnectionInfo const info(user + "(348uk):randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "348uk");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "(348uk):randstad/households");
  }

  {
    ConnectionInfo const info(user + "():randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + ":randstad/households");
  }

  {
    ConnectionInfo const info(user + ":randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + ":randstad/households");
  }

  {
    ConnectionInfo const info("@colossal:randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "colossal");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), "@colossal:randstad/households");
  }

  {
    ConnectionInfo const info("randstad/households");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), "randstad/households");
  }

  {
    ConnectionInfo const info("randstad");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), "randstad");
  }

  {
    ConnectionInfo const info("extensiontest.sql3");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "extensiontest.sql3");
    BOOST_CHECK_EQUAL(info.table(), "");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), "extensiontest.sql3");
  }

  {
    ConnectionInfo const info(user + ":randstad1995/grid_info");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad1995");
    BOOST_CHECK_EQUAL(info.table(), "grid_info");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + ":randstad1995/grid_info");
  }

  {
    ConnectionInfo const info(user + "@randstad");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), user + "@randstad");
    BOOST_CHECK_EQUAL(info.table(), "");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "@randstad");
  }

  {
    ConnectionInfo const info(user + "@");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), user + "@");
    BOOST_CHECK_EQUAL(info.table(), "");
    BOOST_TEST(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "@");
  }

  {
    ConnectionInfo const info("temporal/co2/co2");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "temporal");
    BOOST_CHECK_EQUAL(info.table(), "co2");
    BOOST_REQUIRE_EQUAL(info.fields().size(), size_t(1));
    BOOST_CHECK_EQUAL(info.fields()[0], "co2");
    BOOST_CHECK_EQUAL(info.name(), "temporal/co2");
  }

  {
    ConnectionInfo const info("temporal/co2/{fid,co2}");
    BOOST_TEST(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "temporal");
    BOOST_CHECK_EQUAL(info.table(), "co2");
    BOOST_REQUIRE_EQUAL(info.fields().size(), size_t(2));
    BOOST_CHECK_EQUAL(info.fields()[0], "fid");
    BOOST_CHECK_EQUAL(info.fields()[1], "co2");
    BOOST_CHECK_EQUAL(info.name(), "temporal/co2");
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
