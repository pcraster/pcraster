#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CMDLINECALCTEST
#include "calc_cmdlinecalctest.h"
#define INCLUDED_CALC_CMDLINECALCTEST
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
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif

// Module headers.
#ifndef INCLUDED_CALC_MESSAGESTESTDB
#include "calc_messagestestdb.h"
#define INCLUDED_CALC_MESSAGESTESTDB
#endif
#ifndef INCLUDED_CALC_CMDLINECALC
#include "calc_cmdlinecalc.h"
#define INCLUDED_CALC_CMDLINECALC
#endif


/*!
  \file
  This file contains the implementation of the CmdLineCalcTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CMDLINECALC MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::CmdLineCalcTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CmdLineCalcTest> instance(new CmdLineCalcTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CmdLineCalcTest::testScriptFile, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CmdLineCalcTest::testModelAsArgs, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CMDLINECALC MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::CmdLineCalcTest::CmdLineCalcTest()
{
}



//! setUp
void calc::CmdLineCalcTest::setUp()
{
}



//! tearDown
void calc::CmdLineCalcTest::tearDown()
{
}



void calc::CmdLineCalcTest::testScriptFile()
{
  {
   // TODO test this result: "ERROR: File 'failureExpectedNotExistant.mod': No such file or directory"
   char *argv[3]= { "pcrcalc", "-f", "failureExpectedNotExistant.mod" };
   int r= calc::executeCommandLine(3, argv);
   BOOST_CHECK(r==1);
  }

  // model outcomes are already tested in calc_executortest.cc
  // TODO NO THEY ARE NOT
  MessagesTestDB *db=MessagesTestDB::instance();
  std::string model = db->model("pcrcalc382");

  com::write(model,"tmp.mod");

  {
   char *argv[3]= { "pcrcalc", "-f", "tmp.mod" };
   int r= calc::executeCommandLine(3, argv);
   BOOST_CHECK(r==0);
  }
  {
   char *argv[6]= { "pcrcalc", "--clone", "inp1b.map", "-m", "-f", "tmp.mod" };
   int r= calc::executeCommandLine(6, argv);
   BOOST_CHECK(r==0);
  }
  {
   char *argv[6]= { "pcrcalc", "-m", "-r", "/home/cees/tmp/pcrtest",
                                     "-f", "/home/cees/tmp/pcrtest/pcrtest.mod" 
                  };
   bool absolutePathInRunDirectory=false;
   BOOST_WARN(absolutePathInRunDirectory);
   int r= 1; // TODO calc::executeCommandLine(6, argv); MAKE a bugzilla note
   BOOST_WARN(r==0);
  }
}

void calc::CmdLineCalcTest::testModelAsArgs()
{
  {
   geo::FileCreateTester fct("tmp.res");
   char *argv[2]= { "pcrcalc", "tmp.res = inp1s.map + 4" };
   int r= calc::executeCommandLine(2, argv);
   BOOST_CHECK(r==0);
   BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
  {
   geo::FileCreateTester fct("tmp.res");
   char *argv[]= { "pcrcalc", "tmp.res","=","inp1s.map","+"," 4" };
   int r= calc::executeCommandLine(ARRAY_SIZE(argv), argv);
   BOOST_CHECK(r==0);
   BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
}
