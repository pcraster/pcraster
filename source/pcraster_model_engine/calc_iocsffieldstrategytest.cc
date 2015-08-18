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
#ifndef INCLUDED_CALC_IOFIELDSTRATEGY
#include "calc_iofieldstrategy.h"
#define INCLUDED_CALC_IOFIELDSTRATEGY
#endif
#ifndef INCLUDED_CALC_RUNDIRECTORY
#include "calc_rundirectory.h"
#define INCLUDED_CALC_RUNDIRECTORY
#endif

/*!
  \file
  This file contains the implementation of the IoFieldStrategyTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOCSFFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::IoFieldStrategyTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<IoFieldStrategyTest> instance(new IoFieldStrategyTest());

  suite->add(BOOST_CLASS_TEST_CASE(&IoFieldStrategyTest::testGridMap, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&IoFieldStrategyTest::testGetStackReaderDefault, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&IoFieldStrategyTest::testGetStackReaderPath, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF IOCSFFIELDSTRATEGY MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::IoFieldStrategyTest::IoFieldStrategyTest(){
}



//! setUp
void calc::IoFieldStrategyTest::setUp()
{
}

//! tearDown
void calc::IoFieldStrategyTest::tearDown()
{
}

void calc::IoFieldStrategyTest::testGetStackReaderDefault()
{
 {
  IoFieldStrategy s(APP_IO_PCRASTER);
  RunDirectory rd;
  std::string stackName("tmp");
  std::string sr = s.inPathStack(rd,stackName,1);
  std::string result("tmp00000.001");
  BOOST_CHECK(s.makeStackItemName(stackName,1) == result);
  BOOST_CHECK(sr == stackName);
 }

 {
  IoFieldStrategy s(APP_IO_PCRASTER);
  RunDirectory rd;
  std::string stackName("./stackReader/tmp00000.001");
  BOOST_CHECK(com::pathExists(stackName));
  std::string sr = s.inPathStack(rd,stackName,1);
  com::PathName  result("./stackReader/tmp00000.001");
  result.makeNative();

  BOOST_CHECK(s.makeStackItemName(stackName,1) == result.toString());

  BOOST_CHECK(sr == result.toString());
 }
}

void calc::IoFieldStrategyTest::testGetStackReaderPath()
{
  IoFieldStrategy s(APP_IO_PCRASTER);
  RunDirectory rd;
  com::PathName path("stackReader");

  rd.setRunDirectory(path.toString(),"");
  path.makeAbsolute();
  std::string stackName("tmp");
  std::string sr = s.inPathStack(rd,stackName,1);
  std::string item1("tmp00000.001");
  BOOST_CHECK(s.makeStackItemName(stackName,1) == item1);
  com::PathName result(path+stackName);
  BOOST_CHECK(sr == result.toString());

}

#ifndef INCLUDED_CALC_GRIDMAP
#include "calc_gridmap.h"
#define INCLUDED_CALC_GRIDMAP
#endif
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif
void calc::IoFieldStrategyTest::testGridMap()
{
  {
    GridMapIn g("inp5s.map");
    BOOST_CHECK(g.nrRows()==5);
    BOOST_CHECK(g.nrCols()==5);
    BOOST_CHECK(g.vs()    ==VS_S);
    REAL4 data[25];
    data[0]=8;
    data[24]=8;
    g.createSpatial(data,VS_S);
    BOOST_CHECK(pcr::isMV(data[0]));
    BOOST_CHECK(data[1]==5);
    BOOST_CHECK(data[24]==5);
  }
  {
    GridMapIn g("all1_float.bil");
    BOOST_CHECK(g.nrRows()==4);
    BOOST_CHECK(g.nrCols()==4);
    BOOST_CHECK(g.vs()    ==VS_S);
    REAL4 data[16];
    data[0]=8;
    data[15]=8;
    g.createSpatial(data,VS_S);
    BOOST_CHECK(pcr::isMV(data[0]));
    BOOST_CHECK(data[1]==1.0f);
    BOOST_CHECK(data[14]==1.0f);
  }
}
