#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MANUALEXAMPLETESTERTEST
#include "calc_manualexampletestertest.h"
#define INCLUDED_CALC_MANUALEXAMPLETESTERTEST
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
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_MANUALEXAMPLETESTER
#include "calc_manualexampletester.h"
#define INCLUDED_CALC_MANUALEXAMPLETESTER
#endif



/*!
  \file
  This file contains the implementation of the ManualExampleTesterTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MANUALEXAMPLETESTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ManualExampleTesterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ManualExampleTesterTest> instance(new ManualExampleTesterTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ManualExampleTesterTest::testTest, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ManualExampleTesterTest::testOption, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ManualExampleTesterTest::testClone, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MANUALEXAMPLETESTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ManualExampleTesterTest::ManualExampleTesterTest(){
}



//! setUp
void calc::ManualExampleTesterTest::setUp()
{
}

//! tearDown
void calc::ManualExampleTesterTest::tearDown()
{
}

void calc::ManualExampleTesterTest::testTest()
{
 {
    ManualExampleTester mte("inp1s.map+4");
    mte.addResult("TesterTest.map");
    try {
      mte.test();
    } catch (...) {
      BOOST_CHECK(false);
    }
 }
 {
    bool catchError(false);
    ManualExampleTester mte("inp1s.map+4");
    mte.addResult("failureExpectedNotExistant.map");
    try {
      mte.test();
    } catch (const com::Exception& e) {
      catchError=true;
    }
    BOOST_CHECK(catchError);
 }
 {
    bool catchError(false);
    ManualExampleTester mte("inp1s.map+4");
    mte.addResult("TesterTest1s.map");
    try {
      mte.test();
    } catch (const com::Exception& e) {
      BOOST_CHECK(e.messages().find("Not equal") != std::string::npos);
      catchError=true;
    }
    BOOST_CHECK(catchError);
 }
}

void calc::ManualExampleTesterTest::testOption()
{
 {
    ManualExampleTester mte("(inp5s.map*0)+cellarea()");
    mte.addResult("TesterOption.map");
    mte.addOption("unitcell");
    try {
      mte.test();
    } catch (...) {
      BOOST_CHECK(false);
    }
 }
}

void calc::ManualExampleTesterTest::testClone()
{
 { // not set but needed
    ManualExampleTester mte("cellarea()");
    mte.addResult("TesterOption.map");
    mte.addOption("unitcell");
    bool catched(false);
    try {
      mte.test();
    } catch (const com::Exception& e) {
      BOOST_CHECK(e.messages().find("cloneNotSet"));
      catched=true;
    }
    BOOST_CHECK(catched);
 }
 { // set and needed
    ManualExampleTester mte("cellarea()*5");
    mte.addResult("TesterClone.map");
    mte.addOption("unitcell");
    mte.setClone("inp5s.map");
    try {
      mte.test();
    } catch (...) {
      BOOST_CHECK(false);
    }
 }
}
