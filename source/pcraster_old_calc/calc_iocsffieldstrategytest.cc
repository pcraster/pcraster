#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST
#include "calc_iocsffieldstrategytest.h"
#define INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST
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
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

// Module headers.
#ifndef INCLUDED_CALC_IOCSFFIELDSTRATEGY
#include "calc_iocsffieldstrategy.h"
#define INCLUDED_CALC_IOCSFFIELDSTRATEGY
#endif
#ifndef INCLUDED_CALC_RUNDIRECTORY
#include "calc_rundirectory.h"
#define INCLUDED_CALC_RUNDIRECTORY
#endif
#ifndef INCLUDED_CALC_STACKREADER
#include "calc_stackreader.h"
#define INCLUDED_CALC_STACKREADER
#endif



/*!
  \file
  This file contains the implementation of the IoCsfFieldStrategyTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOCSFFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::IoCsfFieldStrategyTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<IoCsfFieldStrategyTest> instance(new IoCsfFieldStrategyTest());

//  suite->add(BOOST_CLASS_TEST_CASE(&IoCsfFieldStrategyTest::testGetStackReaderDefault, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&IoCsfFieldStrategyTest::testGetStackReaderPath, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF IOCSFFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::IoCsfFieldStrategyTest::IoCsfFieldStrategyTest(){
}



//! setUp
void calc::IoCsfFieldStrategyTest::setUp()
{
}

//! tearDown
void calc::IoCsfFieldStrategyTest::tearDown()
{
}


void calc::IoCsfFieldStrategyTest::testGetStackReaderDefault()
{
 {
  IoCsfFieldStrategy s;
  RunDirectory rd;
  std::string stackName("tmp");
  const StackReader *sr = s.createStackReader(rd,stackName);
  std::string result("tmp00000.001");
  BOOST_CHECK(s.makeStackItemName(stackName,1) == result);
  BOOST_CHECK(sr->itemName(1) == result);
  BOOST_CHECK(sr->stackName() == stackName);
  delete sr;
 }

 {
  IoCsfFieldStrategy s;
  RunDirectory rd;
  com::PathName stackName("./stackReader/tmp00000.001");
  BOOST_CHECK(com::pathExists(stackName.toString()));
  const StackReader *sr = s.createStackReader(rd,stackName.toString());
  com::PathName result("./stackReader/tmp00000.001");
  BOOST_CHECK(s.makeStackItemName(stackName.toString(),1) == result.toString());
  BOOST_CHECK(sr->itemName(1) == result);
  BOOST_CHECK(sr->stackName() == stackName.toString());
  delete sr;
 }


}

void calc::IoCsfFieldStrategyTest::testGetStackReaderPath()
{
  IoCsfFieldStrategy s;
  RunDirectory rd;
  com::PathName path("stackReader");

  rd.setRunDirectory(path.toString(),"");
  path.makeAbsolute();
  std::string stackName("tmp");
  const StackReader *sr = s.createStackReader(rd,stackName);
  std::string item1("tmp00000.001");
  com::PathName result(path+item1);
  BOOST_CHECK(s.makeStackItemName(stackName,1) == item1);
//PRINT_VAR(sr->itemName(1))
//PRINT_VAR(result.toString());
#ifdef WIN32
  bool onWin32=true;
  BOOST_WARN( (onWin32 && sr->itemName(1) == result.toString()));
  BOOST_CHECK( (onWin32 && sr->stackName() == (path+stackName)) );
#endif

  delete sr;
}
