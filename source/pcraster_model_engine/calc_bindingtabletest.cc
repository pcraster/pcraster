#define BOOST_TEST_MODULE pcraster model_engine bindingtable
#include <boost/test/unit_test.hpp>
#include "calc_asttestfactory.h"
#include "calc_globallibdefs.h"
#define private public
#include "calc_astscript.h"


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



BOOST_FIXTURE_TEST_SUITE(bindingtable, Fixture)


BOOST_AUTO_TEST_CASE(testApplyErrors)
{
  using namespace calc;

  EXEC_ERROR_TEST("pcrcalc43");
  EXEC_ERROR_TEST("pcrcalc43a");
  EXEC_ERROR_TEST("pcrcalc43b");

  EXEC_ERROR_TEST("pcrcalc0");
  EXEC_ERROR_TEST("pcrcalc10d");
  EXEC_ERROR_TEST("pcrcalc513");

}

BOOST_AUTO_TEST_CASE(testApply)
{
  using namespace calc;

 typedef std::auto_ptr<ASTScript> S;
 {
   const char *model=
  "binding constantNr=1;areamap inp1s.map; initial tmp.res = 5*constantNr;";
  S s(ASTTestFactory::createFromIdOrStr(model));
  BOOST_CHECK(s->bindings().size()==1);
  BOOST_CHECK(s->symbols().size()==0);

  s->buildTypesFullClosure();

  BOOST_CHECK(s->symbols().size()==3);

  BOOST_CHECK(s->symbols().contains("inp1s.map"));

  BOOST_CHECK(s->symbols().contains("constantNr"));

  BOOST_CHECK(!s->symbols()["constantNr"].isConstant());

  s->applyInterface();

  BOOST_CHECK( s->symbols()["constantNr"].isConstant());

 }
 {
   const char *model=
   "binding inpBinding=inp1s.map; initial tmp.res = 5*inpBinding;";
  S s(ASTTestFactory::createFromIdOrStr(model));
  BOOST_CHECK(s->bindings().size()==1);
  BOOST_CHECK(s->symbols().size()==0);

  s->buildTypesFullClosure();

  BOOST_CHECK(s->symbols().size()==2);
  BOOST_CHECK(s->symbols().contains("inpBinding"));

  BOOST_CHECK(s->symbols()["inpBinding"].name()     == "inpBinding");
  BOOST_CHECK(s->symbols()["inpBinding"].externalName() == "inpBinding");

  s->applyInterface();

  BOOST_CHECK(s->symbols()["inpBinding"].name()     == "inpBinding");
  BOOST_CHECK(s->symbols()["inpBinding"].externalName() == "inp1s.map");
 }
 { // binding use each others values
   S s(ASTTestFactory::createFromIdOrStr(
    "binding t=inp1s.map;u=t; initial tmp.res=4+u;"));

   s->buildTypesFullClosure();
   s->applyInterface();

   BOOST_CHECK(s->d_bindings.size()==2); // t,u
   BOOST_CHECK(s->symbols().size()==2);  // tmp.res, u

  BOOST_CHECK(s->symbols()["u"].name()     == "u");
  BOOST_CHECK(s->symbols()["u"].externalName() == "inp1s.map");
 }
 { // binding use each others values
   S s(ASTTestFactory::createFromIdOrStr(
    "binding t=inp1s.map;x=t;u=x; initial tmp.res=4+u;"));

   s->buildTypesFullClosure();
   s->applyInterface();

   BOOST_CHECK(s->d_bindings.size()==3); // t,u,x
   BOOST_CHECK(s->symbols().size()==2);  // tmp.res, u

  BOOST_CHECK(s->symbols()["u"].name()     == "u");
  BOOST_CHECK(!s->symbols()["u"].isConstant());
  BOOST_CHECK(s->symbols()["u"].externalName() == "inp1s.map");
 }
 { // binding use each others values
   S s(ASTTestFactory::createFromIdOrStr(
    "binding t=1;x=t;u=x; initial tmp.res=4+u;"));

   s->buildTypesFullClosure();
   s->applyInterface();

   BOOST_CHECK(s->d_bindings.size()==3); // t,u,x
   BOOST_CHECK(s->symbols().size()==2);  // tmp.res, u

  BOOST_CHECK(s->symbols()["u"].name()     == "u");
  BOOST_CHECK(s->symbols()["u"].isConstant());
 }
}

BOOST_AUTO_TEST_SUITE_END()
