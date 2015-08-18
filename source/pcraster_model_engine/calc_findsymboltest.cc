#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_FINDSYMBOLTEST
#include "calc_findsymboltest.h"
#define INCLUDED_CALC_FINDSYMBOLTEST
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
#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif


/*!
  \file
  This file contains the implementation of the FindSymbolTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FINDSYMBOL MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::FindSymbolTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FindSymbolTest> instance(new FindSymbolTest());

  suite->add(BOOST_CLASS_TEST_CASE(&FindSymbolTest::testOpName2op, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF FINDSYMBOL MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::FindSymbolTest::FindSymbolTest()
{
}



//! setUp
void calc::FindSymbolTest::setUp()
{
}

//! tearDown
void calc::FindSymbolTest::tearDown()
{
}



void calc::FindSymbolTest::testOpName2op()
{
   {
      const Operator* op(opName2op("max"));
      BOOST_CHECK(op);
      BOOST_CHECK(op->opCode() == OP_MAX);
   }
   {
      const Operator* op(opName2op("xxxxxxxxxxxxxxxxxxxxxxx"));
      BOOST_CHECK(!op);
   }
   {
      const Operator* op(opName2op("*"));
      BOOST_CHECK(op);
      BOOST_CHECK(op->opCode() == OP_MUL);
   }
   {
      const Operator* op(opName2op("+"));
      BOOST_CHECK(op);
      BOOST_CHECK(op->opCode() == OP_BADD);
   }
   {
      const Operator* op(opName2op("+",1));
      BOOST_CHECK(op);
      BOOST_CHECK(op->opCode() == OP_UADD);
   }
   { // MRF inserted correct
      const Operator* op(opName2op("spread"));
      BOOST_CHECK(op);
      BOOST_CHECK(op->opCode() == OP_SPREAD);
   }
}
