#define BOOST_TEST_MODULE pcraster model_engine manualexampletester
#include <boost/test/unit_test.hpp>
#include "com_exception.h"
#include "calc_manualexampletester.h"
#include "calc_globallibdefs.h"

struct Fixture
{

    Fixture()
    {
        calc::globalInit();
    }


    ~Fixture()
    {
        calc::globalEnd();
    }

};


BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(testTest)
{
  using namespace calc;

 {
    ManualExampleTester mte("TesterTest.map=inp1s.map+4");
    mte.addResult("TesterTest.map");
    try {
      mte.test();
    } catch (...) {
      bool bilRelatedIGuess=false;
      BOOST_CHECK(bilRelatedIGuess);
    }
 }
 {
    bool catchError(false);
    ManualExampleTester mte("failureExpectedNotExistant.map=inp1s.map+4");
    mte.addResult("failureExpectedNotExistant.map");
    try {
      mte.test();
    } catch (const com::Exception& ) {
      catchError=true;
    }
    BOOST_CHECK(catchError);
 }
 {
    bool catchError(false);
    ManualExampleTester mte("TesterTest1s.map=inp1s.map+4;");
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

BOOST_AUTO_TEST_CASE(testOption)
{
  using namespace calc;

 {
    ManualExampleTester mte(
      "#! --unitcell\n"
      "TesterOption.map=(inp5s.map*0)+cellarea();");
    mte.addResult("TesterOption.map");
    try {
      mte.test();
    } catch (...) {
      BOOST_CHECK(false);
    }
 }
}

BOOST_AUTO_TEST_CASE(testClone)
{
  using namespace calc;

 { // not set but needed
    ManualExampleTester mte(
      "#! --unitcell\n"
      "TesterOption.map=cellarea();");
    mte.addResult("TesterOption.map");
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
    ManualExampleTester mte(
      "#! --unitcell\n"
      "areamap inp5s.map;\n"
      "TesterClone.map=cellarea()*5;");
    mte.addResult("TesterClone.map");
    try {
      mte.test();
    } catch (...) {
      BOOST_CHECK(false);
    }
 }
}
