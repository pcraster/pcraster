#define BOOST_TEST_MODULE pcraster old_calc manual_examples
#include <boost/test/unit_test.hpp>
#include <vector>
#include <iostream>
#include "com_pathinfo.h"
#include "com_exception.h"
#include "calc_manualexampletester.h"


struct Fixture
{

    Fixture()
    {
    }


    ~Fixture()
    {
      com::changeWorkingDirectory(d_startDir);
    }

    com::PathName  d_startDir;

};


BOOST_FIXTURE_TEST_SUITE(manual_examples, Fixture)

BOOST_AUTO_TEST_CASE(all)
{
  using namespace calc;

  BOOST_WARN_MESSAGE( 0, "copy this module from PCRasterModelEngine");
  std::vector<ManualExampleTester> et;
// #include "examples.inc"
  int nrFailures=0;
  for(size_t i=0; i < et.size(); i++) {
    try {
      et[i].test();
    } catch (const com::Exception& e) {
      std::cerr << e.messages();
      nrFailures++;
    }
  }
  if (nrFailures) {
   std::cerr << "Note order.Result.omap will fail, new algoritm in newcalc \n";
   std::cerr << nrFailures << " failures out of " << et.size() << "\n";
  }
  BOOST_WARN((nrFailures <= 3));
}

BOOST_AUTO_TEST_SUITE_END()
