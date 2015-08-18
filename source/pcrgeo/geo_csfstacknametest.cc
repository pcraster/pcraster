#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CSFSTACKNAMETEST
#include "geo_csfstacknametest.h"
#define INCLUDED_GEO_CSFSTACKNAMETEST
#endif

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

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_GEO_CSFSTACKNAME
#include "geo_csfstackname.h"
#define INCLUDED_GEO_CSFSTACKNAME
#endif



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*geo::CSFStackNameTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CSFStackNameTest> instance(new CSFStackNameTest());
  suite->add(BOOST_CLASS_TEST_CASE(&CSFStackNameTest::testConstructor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFStackNameTest::testBadFormats, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFStackNameTest::testBaseName, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFStackNameTest::testFileName, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CSFStackNameTest::testAsAguilaArgument, instance));
  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

geo::CSFStackNameTest::CSFStackNameTest()

{
}



void geo::CSFStackNameTest::setUp()
{
}



void geo::CSFStackNameTest::tearDown()
{
}



void geo::CSFStackNameTest::testBaseName()
{
  com::PathName pn;

  // Static stack in current dir.
  pn = "soil.csf";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(sn.baseName() == "soil.csf");

  // Dynamic stack in current dir.
  pn = "soil0000.010+100";
  sn = pn;
  BOOST_CHECK(sn.baseName() == "soil0000");

  pn = "soilsoil.010+100";
  sn = pn;
  BOOST_CHECK(sn.baseName() == "soilsoil");

  pn = "soilsoil.s10+100";
  sn = pn;
  BOOST_CHECK(sn.baseName() == "soilsoil.s");

  {
    // Test functionality that based on the last timestep the digits for the
    // first time step are determined. In this case, 9700 cannot be part of the
    // timestep and thus is part of the basename.
    pn = "lisw9700.001+999";
    sn = pn;
    BOOST_CHECK(sn.baseName() == "lisw9700");
  }
}



void geo::CSFStackNameTest::testFileName()
{

 {
  // Static stack in current dir.
  com::PathName pn = "soil.csf";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(sn.fileName(50) == "soil.csf");
 } 
 // pcrcalc has a trick that if the extension
 // is given each timestep is written to the
 // same file
 {
  // CW wanted to use this one  in pcrcalc
  com::PathName pn = "tmp.res";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(!sn.isDynamic());
  BOOST_CHECK(sn.fileName(1) == "tmp.res");
  BOOST_CHECK(sn.fileName(300) == "tmp.res");
 }
 {
  // and not this one in pcrcalc
  com::PathName pn = "tmp.res";
  geo::CSFStackName sn(pn,1,1000);
  BOOST_CHECK(sn.isDynamic());
  BOOST_CHECK(sn.fileName(1) == "tmp.res");
  BOOST_CHECK(sn.fileName(300) == "tmp.res");
 }
 {
  // Dynamic stack in current dir.
  com::PathName pn = "soil0000.010+100";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(sn.fileName(50) == "soil0000.050");
 }

  {
    com::PathName pn = "soilsoil.010+100";
    geo::CSFStackName sn(pn);
    BOOST_CHECK(sn.fileName(50) == "soilsoil.050");
  }

  {
    com::PathName pn = "soilsoil.s10+100";
    geo::CSFStackName sn(pn);
    BOOST_CHECK(sn.fileName(50) == "soilsoil.s50");
  }
}



void geo::CSFStackNameTest::testNrLayers()
{
  com::PathName pn;

  // Static stack in current dir.
  pn = "soil.csf";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(sn.nrLayers() == 1);

  // Dynamic stack in current dir.
  pn = "soil0000.010+100";
  sn = pn;
  BOOST_CHECK(sn.nrLayers() == 0); // Stack doesn't exist in current dir.
}



void geo::CSFStackNameTest::testConstructor()
{
  com::PathName pn;

  // Static stack in current dir.
  pn = "soil.csf";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(!sn.isDynamic());

  // Dynamic stack in current dir.
  pn = "soil0000.010+100";
  sn = pn;
  BOOST_CHECK(sn.scanned());
  BOOST_CHECK(sn.isDynamic());

  // Dynamic stack in current dir.
  pn = "XXXeight.001+20000";
  sn = pn;
  BOOST_CHECK(sn.scanned());
  BOOST_CHECK(sn.isDynamic());

  // baseName ends with numeric digits
  pn = "XXXX970.001+30";
  sn = pn;
  BOOST_CHECK(sn.scanned());
  BOOST_CHECK(sn.isDynamic());

}

void geo::CSFStackNameTest::testBadFormats()
{
  bool catchWrongFormat;
  
  // stupid error
  catchWrongFormat=false;
  try {
       com::PathName pn("+");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
   catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);

  // no last timestep
  catchWrongFormat=false;
  try {
       com::PathName pn("rnpnts00.001+");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
   catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);
  
  // last timestep is not numeric
  catchWrongFormat=false;
  try {
       com::PathName pn("rnpnts00.001+XXX");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
   catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);

  // no first timestep
  catchWrongFormat=false;
  try {
       com::PathName pn("rnpntsxx.xxx+431");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
   catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);

  catchWrongFormat=false;
  // first timestep larger than second
  try {
       com::PathName pn("XXXX970.009+3");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
    catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);

}

void geo::CSFStackNameTest::testAsAguilaArgument()
{
  std::string r(CSFStackName::asAguilaArgument("prefix",1,100));
  BOOST_CHECK(r == "prefix00.001+100");
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


