#ifndef INCLUDED_DAL_CONNECTIONINFOTEST
#include "dal_ConnectionInfoTest.h"
#define INCLUDED_DAL_CONNECTIONINFOTEST
#endif

// Library headers.
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

// PCRaster library headers.
#ifndef INCLUDED_DEV_UTILS
#include "dev_Utils.h"
#define INCLUDED_DEV_UTILS
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONNECTIONINFO
#include "dal_ConnectionInfo.h"
#define INCLUDED_DAL_CONNECTIONINFO
#endif

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif



/*!
  \file
  This file contains the implementation of the ConnectionInfoTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CONNECTIONINFO MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*dal::ConnectionInfoTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ConnectionInfoTest> instance(new ConnectionInfoTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ConnectionInfoTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CONNECTIONINFO MEMBERS
//------------------------------------------------------------------------------

//! ctor
dal::ConnectionInfoTest::ConnectionInfoTest()
{
}



//! setUp
void dal::ConnectionInfoTest::setUp()
{
}



//! tearDown
void dal::ConnectionInfoTest::tearDown()
{
}



void dal::ConnectionInfoTest::test()
{
  std::string user;

  if(dev::environmentVariableSet("USER")) {
    user = dev::environmentVariable("USER");
  }
  else {
    BOOST_REQUIRE(dev::environmentVariableSet("LOGNAME"));
    user = dev::environmentVariable("LOGNAME");
  }

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

