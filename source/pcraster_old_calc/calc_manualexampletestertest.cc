#define BOOST_TEST_MODULE pcraster old_calc manual_example_tester
#include <boost/test/unit_test.hpp>
#include "com_exception.h"
#include "calc_manualexampletester.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace calc;

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


BOOST_AUTO_TEST_CASE(option)
{
  using namespace calc;

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


BOOST_AUTO_TEST_CASE(clone_)
{
  using namespace calc;

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
