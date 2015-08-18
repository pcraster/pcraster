#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CLIENTINTERFACETEST
#include "calc_clientinterfacetest.h"
#define INCLUDED_CALC_CLIENTINTERFACETEST
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

// Module headers.
#ifndef INCLUDED_PCRCALC
#include "pcrcalc.h"
#define INCLUDED_PCRCALC
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif
/*!
  \file
  This file contains the implementation of the ClientInterfaceTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ClientInterfaceTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ClientInterfaceTest> instance(new ClientInterfaceTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ClientInterfaceTest::testCapi, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ClientInterfaceTest::ClientInterfaceTest()
{
}



//! setUp
void calc::ClientInterfaceTest::setUp()
{
}

//! tearDown
void calc::ClientInterfaceTest::tearDown()
{
}
// piece to copy into user docs
#include "pcrcalc.h"
static void foo() {

    PcrScript *s=pcr_createScript("c:\\tmp\\case.xml");
    if (!s) {
      printf("PANIC allocation of a few bytes failed");
      abort();
    }
    if (pcr_ScriptError(s)) {
      /* typical error: case.xml is not existant */
      printf("ERROR: %s\n",pcr_ScriptErrorMessage(s));
      exit(1);
    }

    pcr_ScriptExecute(s);
    if (pcr_ScriptError(s)) {
      /* typical errors:
       * - case.xml is malformed
       * - some inputs are not found
       * - resource error: memory/disk full
       */
      printf("ERROR: %s\n",pcr_ScriptErrorMessage(s));
      exit(1);
    }

    pcr_destroyScript(s);
    if (pcr_ScriptError(s)) {
      /* very unlikely, program corruption
       */
      printf("ERROR: %s\n",pcr_ScriptErrorMessage(s));
      exit(1);
    }
}

//! test the C-Api
void calc::ClientInterfaceTest::testCapi()
{

  { // NULL pointer to pcr_createScript
    PcrScript *s=pcr_createScript(0);
    BOOST_CHECK(s);
    BOOST_CHECK(pcr_ScriptError(s));
    std::string msg(pcr_ScriptErrorMessage(s));
    BOOST_CHECK(msg.find("pcr_createScript") != std::string::npos);
    pcr_destroyScript(s);
  }

  { // file not found
    const char *f="failureExpectedNotExistant";
    PcrScript *s=pcr_createScript(f);
    BOOST_CHECK(s);
    BOOST_CHECK(pcr_ScriptError(s));
    std::string msg(pcr_ScriptErrorMessage(s));
    BOOST_CHECK(msg.find(f) != std::string::npos);
    pcr_destroyScript(s);
  }

  { // some input not found
    com::write("piet.map = not_existant.map * 2;","pcrscripttest.mod");
    PcrScript *s=pcr_createScript("pcrscripttest.mod");
    BOOST_CHECK(s);
    pcr_ScriptExecute(s);
    BOOST_CHECK(pcr_ScriptError(s));
    std::string msg(pcr_ScriptErrorMessage(s));
    BOOST_CHECK(msg.find("not_existant.map") != std::string::npos);
    pcr_destroyScript(s);
  }

  { // execute
    com::write("piet.map = inp1s.map * 2;","pcrscripttest.mod");
    PcrScript *s=pcr_createScript("pcrscripttest.mod");
    BOOST_CHECK(s);
    pcr_ScriptExecute(s);
    BOOST_CHECK(!pcr_ScriptError(s));
    pcr_destroyScript(s);
    BOOST_CHECK(com::pathExists("piet.map"));
  }

  { // execute again
    com::write("piet2.map = inp1s.map * 3;","pcrscripttest.mod");
    PcrScript *s=pcr_createScript("pcrscripttest.mod");
    BOOST_CHECK(s);
    if (!s) foo(); // supress not used message
    pcr_ScriptExecute(s);
    BOOST_CHECK(!pcr_ScriptError(s));
    pcr_destroyScript(s);
    BOOST_CHECK(com::pathExists("piet2.map"));
  }
}
