#define BOOST_TEST_MODULE pcraster model_engine manualexamples
#include <boost/test/unit_test.hpp>
#include <vector>
#include <sstream>
#include "com_pathinfo.h"
#include "com_exception.h"
#include "calc_manualexampletester.h"
#include "calc_globallibdefs.h"
#include "com_pathname.h"
#ifdef WIN32
#include <iostream>
#endif

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

BOOST_AUTO_TEST_CASE(testAll)
{
  using namespace calc;

    std::vector<ManualExampleTester> et;
#include "cpptest.cc"
    std::vector<std::string> failMsgs;
    for(size_t i=0; i < et.size(); i++)
      try {
        et[i].test();
      } catch (const com::Exception& e) {
        failMsgs.push_back(e.messages());
      }

    size_t nrFailuresAllowed=0;
#ifdef WIN32
    if (failMsgs.size()==1) {
     std::cerr << "TODO view on win32 slightly different" << std::endl;
     nrFailuresAllowed=1;
    }
#endif

    std::ostringstream msgs;
    if (failMsgs.size()) {
     // msgs << " compile mode:  " << compile[c] << "\n";
     for (size_t i=0; i < failMsgs.size(); ++i)
     {
        msgs << "---- MANUAL EXAMPLE FAILURE # " << i << "---------------------------------" << "\n";
        msgs << failMsgs[i];
     }
     msgs << failMsgs.size() << " MANUAL EXAMPLES FAILURES OUT OF " << et.size() << "\n";
    }
#if _MSC_VER
#ifdef DEBUG_DEVELOP
   // Bugzilla 178
   nrFailuresAllowed=2;
#endif
#endif
    // TODO this will fail on WIN32 due to view function (see above)
    BOOST_CHECK_MESSAGE(failMsgs.size() == nrFailuresAllowed, msgs.str());
}
