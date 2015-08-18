#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_PARSERTEST
#include "calc_parsertest.h"
#define INCLUDED_CALC_PARSERTEST
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
#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_RUNSCRIPT
#include "calc_runscript.h"
#define INCLUDED_CALC_RUNSCRIPT
#endif

/*!
  \file
  This file contains the implementation of the ParserTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PARSER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ParserTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ParserTest> instance(new ParserTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ParserTest::testModelParser, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PARSER MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ParserTest::ParserTest()
{
}



//! setUp
void calc::ParserTest::setUp()
{
}

//! tearDown
void calc::ParserTest::tearDown()
{
}

void calc::ParserTest::testModelParser()
{
  geo::FileCreateTester mt("parsertest.res");
  runScriptString("parsertest.res = inp1s.map + 4;");
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
}
