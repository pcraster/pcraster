#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_BINDINGTABLETEST
#include "calc_bindingtabletest.h"
#define INCLUDED_CALC_BINDINGTABLETEST
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
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif



/*!
  \file
  This file contains the implementation of the BindingTableTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BINDINGTABLE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::BindingTableTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BindingTableTest> instance(new BindingTableTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BindingTableTest::testApplyErrors, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BindingTableTest::testApply, instance));

  return suite;
}

//------------------------------------------------------------------------------
// DEFINITION OF BINDINGTABLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::BindingTableTest::BindingTableTest()
{
}



//! setUp
void calc::BindingTableTest::setUp()
{
}

//! tearDown
void calc::BindingTableTest::tearDown()
{
}



void calc::BindingTableTest::testApplyErrors()
{

 EXEC_ERROR_TEST("pcrcalc43");
 EXEC_ERROR_TEST("pcrcalc43a");
 EXEC_ERROR_TEST("pcrcalc43b");

 EXEC_ERROR_TEST("pcrcalc0");
 EXEC_ERROR_TEST("pcrcalc10d");
 EXEC_ERROR_TEST("pcrcalc513");

}


void calc::BindingTableTest::testApply()
{
 typedef std::auto_ptr<ASTScript> S;
 {
   const char *model=
  "binding constantNr=1;areamap inp1s.map; initial tmp.res = 5*constantNr;";
  S s(createFromIdOrStr(model));
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
  S s(createFromIdOrStr(model));
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
   S s(createFromIdOrStr(
    "binding t=inp1s.map;u=t; initial tmp.res=4+u;"));

   s->buildTypesFullClosure();
   s->applyInterface();

   BOOST_CHECK(s->d_bindings.size()==2); // t,u
   BOOST_CHECK(s->symbols().size()==2);  // tmp.res, u

  BOOST_CHECK(s->symbols()["u"].name()     == "u");
  BOOST_CHECK(s->symbols()["u"].externalName() == "inp1s.map");
 }
 { // binding use each others values
   S s(createFromIdOrStr(
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
   S s(createFromIdOrStr(
    "binding t=1;x=t;u=x; initial tmp.res=4+u;"));

   s->buildTypesFullClosure();
   s->applyInterface();

   BOOST_CHECK(s->d_bindings.size()==3); // t,u,x
   BOOST_CHECK(s->symbols().size()==2);  // tmp.res, u

  BOOST_CHECK(s->symbols()["u"].name()     == "u");
  BOOST_CHECK(s->symbols()["u"].isConstant());
 }
}
