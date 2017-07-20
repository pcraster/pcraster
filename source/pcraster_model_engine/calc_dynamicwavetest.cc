#define BOOST_TEST_MODULE pcraster model_engine dynamicwave
#include <boost/test/unit_test.hpp>

#include "calc_globallibdefs.h"
#include "geo_filecreatetester.h"
#include "com_csfcell.h"
#include "com_directory.h"
#include "com_file.h"
#include "calc_field.h"
#include "calc_p5stack.h"


// NOTE use string failureExpected in files expected to fail, see style guide

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



BOOST_FIXTURE_TEST_SUITE(dynamicwave, Fixture)


BOOST_AUTO_TEST_CASE(testSimpleInput)
{
  using namespace calc;

  // pcrcalc507 basic dynamicwave
  // profileId=0,H=1,A=2,P=3
  com::write("1 0 0 1\n1 1 2000 2000","tmp507.tbl");

  // first run no tests
  execTest("pcrcalc507");
/*
  // next some alterations:
  const char* pcrcalc507a=" \
      report tmp.state,tmp.flux=\n \
      dynwavestate,dynwaveflux( \n \
      tmp507.tbl,                  \n \
      inp1n.map,  # profile id  \n \
      inpldd.map,               \n \
      %1%, # oldState            \n \
      0, # inflow               \n \
      0, # bottomLevel          \n \
      1, # roughness            \n \
      1, # segmentLength        \n \
      10, # nrTimeSlices        \n \
      1, # timestepInSecs       \n \
      0  # constantState        \n \
      );                        \n \
      report p = lookuppotential(tmp507.tbl,inp1n.map,0,1,tmp.state); \n \
      report s = lookupstate(tmp507.tbl,inp1n.map,0,1,p); \n \
      report tmp.res = (tmp.state==s) && %2%;     \n";


  { // state == 5 everywhere
    geo::FileCreateTester state("tmp.state");
    geo::FileCreateTester  flux("tmp.flux");
    std::string testCmd((boost::format(pcrcalc507a)
          % "5" % "1==1").str());
    execTest(testCmd);

    BOOST_CHECK(flux.equalTo("inp0s.map",false));
    BOOST_CHECK(state.equalTo("inp5s.map",false));
  }
  { // state == distance ==> drop in the network
    geo::FileCreateTester  res("tmp.res");
    std::string testCmd((boost::format(pcrcalc507a) %
          "ldddist(inpldd.map,inpldd.map==5,1)" %
          "tmp.state >= 0 && tmp.flux >= 0"
          ).str());
    execTest(testCmd);

    BOOST_CHECK(res.equalTo("inp1b.map",false));
  }
  { // state == 300-distance ==> only uphill
    geo::FileCreateTester  res("tmp.res");
    std::string testCmd((boost::format(pcrcalc507a) %
     "300-ldddist(inpldd.map,inpldd.map==5,1)" %
     "tmp.flux == 0 && tmp.state==(300-ldddist(inpldd.map,inpldd.map==5,1))"
     ).str());
    execTest(testCmd);

    BOOST_CHECK(res.equalTo("inp1b.map",false));
  }

  { // state == 100000, to generate DomainError
    geo::FileCreateTester state("tmp.state");
    geo::FileCreateTester  flux("tmp.flux");
    std::string testCmd((boost::format(pcrcalc507a)
          % "100000" % "1==1").str());
    TRY_TEST_MSG {
      execTest(testCmd);
    } CATCH_TEST_MSG("pcrcalc508");
    BOOST_CHECK(catched);
  }
*/
}

BOOST_AUTO_TEST_SUITE_END()
