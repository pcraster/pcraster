#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SPAWNTEST
#include "com_spawntest.h"
#define INCLUDED_COM_SPAWNTEST
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
#ifndef INCLUDED_COM_SPAWN
#include "com_spawn.h"
#define INCLUDED_COM_SPAWN
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif


/*!
  \file
  This file contains the implementation of the SpawnTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPAWN MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::SpawnTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SpawnTest> instance(new SpawnTest());

  suite->add(BOOST_CLASS_TEST_CASE(&SpawnTest::testNoArg, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&SpawnTest::testArgs, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SPAWN MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::SpawnTest::SpawnTest()
{
}



//! setUp
void com::SpawnTest::setUp()
{
}



//! tearDown
void com::SpawnTest::tearDown()
{
}



void com::SpawnTest::testNoArg()
{
#if defined(WIN32) || defined(__x86_64__)
  // or use Qt/process
  bool noSpawnWorking=false;
  BOOST_WARN(noSpawnWorking);
#else
  std::string sigR,sig("KILLROY WAS HERE\n");

  com::PathName pn("spawn.txt");

  com::remove(pn);

  int i=spawn("spawnScript");

  BOOST_CHECK(i==0);
  BOOST_CHECK(com::exists(pn));
  com::read(sigR,pn);
  BOOST_CHECK(sig==sigR);
#endif
}

void com::SpawnTest::testArgs()
{
#if defined(WIN32) || defined(__x86_64__)
  bool noSpawnWorking=false;
  BOOST_WARN(noSpawnWorking);
#else
  std::string sigR,sig("KILLROY WAS HERE\n");
  com::PathName pn("spawn.txt");

 {
  com::remove(pn);

  // explicit exit 2 in working script
  spawn("spawnScript2","KILLROY WAS    HERE");

  BOOST_CHECK(com::exists(pn));
  sigR.clear();
  com::read(sigR,pn);
  BOOST_CHECK(sig==sigR);
 }
 {
  const char *args[5] = {"spawnScript2", "KILLROY","WAS","HERE",0};
  com::remove(pn);

  // explicit exit 2 in working script
  int expectExplicitExitCode=spawn("spawnScript2",args);

  if (expectExplicitExitCode!=2);
   BOOST_WARN(expectExplicitExitCode);
  BOOST_CHECK(com::exists(pn));
  com::read(sigR,pn);
  BOOST_CHECK(sig==sigR);
 }
#endif
}
