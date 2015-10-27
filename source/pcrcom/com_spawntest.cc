#define BOOST_TEST_MODULE pcraster com spawn
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_spawn.h"
#include "com_file.h"


BOOST_AUTO_TEST_CASE(no_arg)
{
  using namespace com;

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


BOOST_AUTO_TEST_CASE(args)
{
  using namespace com;

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
