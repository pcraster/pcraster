#define BOOST_TEST_MODULE pcraster dal connection_info
#include <boost/test/unit_test.hpp>
#include "dev_Utils.h"
#include "dal_ConnectionInfo.h"
#include "dal_Exception.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  std::string user;

#if defined(WIN32)
  BOOST_REQUIRE(dev::environmentVariableSet("USERNAME"));
  user = dev::environmentVariable("USERNAME");
#else
  BOOST_REQUIRE(dev::environmentVariableSet("LOGNAME"));
  user = dev::environmentVariable("LOGNAME");
#endif

  BOOST_REQUIRE(!user.empty());

  if(user.find(' ') != std::string::npos) {
    // We don't support user names with spaces in them yet.
    return;
  }

  // Valid examples ------------------------------------------------------------
  {
    ConnectionInfo info(user + "(348uk)@colossal:randstad/households");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "348uk");
    BOOST_CHECK_EQUAL(info.host(), "colossal");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "(348uk)@colossal:randstad/households");
  }

  {
    ConnectionInfo info(user + "()@colossal:randstad/households");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "colossal");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "@colossal:randstad/households");
  }

  {
    ConnectionInfo info(user + "@colossal:randstad/households");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "colossal");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "@colossal:randstad/households");
  }

  {
    ConnectionInfo info(user + "(348uk):randstad/households");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "348uk");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "(348uk):randstad/households");
  }

  {
    ConnectionInfo info(user + "():randstad/households");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + ":randstad/households");
  }

  {
    ConnectionInfo info(user + ":randstad/households");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + ":randstad/households");
  }

  {
    ConnectionInfo info("@colossal:randstad/households");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "colossal");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), "@colossal:randstad/households");
  }

  {
    ConnectionInfo info("randstad/households");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "households");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), "randstad/households");
  }

  {
    ConnectionInfo info("randstad");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad");
    BOOST_CHECK_EQUAL(info.table(), "");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), "randstad");
  }

  {
    ConnectionInfo info("extensiontest.sql3");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "extensiontest.sql3");
    BOOST_CHECK_EQUAL(info.table(), "");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), "extensiontest.sql3");
  }

  {
    ConnectionInfo info(user + ":randstad1995/grid_info");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), user);
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), "randstad1995");
    BOOST_CHECK_EQUAL(info.table(), "grid_info");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + ":randstad1995/grid_info");
  }

  {
    ConnectionInfo info(user + "@randstad");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), user + "@randstad");
    BOOST_CHECK_EQUAL(info.table(), "");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "@randstad");
  }

  {
    ConnectionInfo info(user + "@");
    BOOST_CHECK(info.isValid());
    BOOST_CHECK_EQUAL(info.user(), "");
    BOOST_CHECK_EQUAL(info.password(), "");
    BOOST_CHECK_EQUAL(info.host(), "");
    BOOST_CHECK_EQUAL(info.database(), user + "@");
    BOOST_CHECK_EQUAL(info.table(), "");
    BOOST_CHECK(info.fields().empty());
    BOOST_CHECK_EQUAL(info.name(), user + "@");
  }

  {
    ConnectionInfo info("temporal/co2/co2");
    BOOST_CHECK(info.isValid());
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
    ConnectionInfo info("temporal/co2/{fid,co2}");
    BOOST_CHECK(info.isValid());
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
  //   BOOST_CHECK(info.isValid());
  // }

  // {
  //   ConnectionInfo info(user + "@");
  //   BOOST_CHECK(!info.isValid());
  // }

  {
    ConnectionInfo info("");
    BOOST_CHECK(!info.isValid());
  }
}
