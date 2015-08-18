#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OPERATORTEST
#include "calc_operatortest.h"
#define INCLUDED_CALC_OPERATORTEST
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
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif



/*!
  \file
  This file contains the implementation of the OperatorTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC OPERATOR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::OperatorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<OperatorTest> instance(new OperatorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&OperatorTest::testFirstFieldInput, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&OperatorTest::testActualInput, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF OPERATOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::OperatorTest::OperatorTest()
{
}



//! setUp
void calc::OperatorTest::setUp()
{
}

//! tearDown
void calc::OperatorTest::tearDown()
{
}


void calc::OperatorTest::testFirstFieldInput()
{
  {
    const Operator *op(opName2op("lookupscalar"));
    BOOST_CHECK(op);
    BOOST_CHECK(op->firstFieldInput() == 1);
  }
  {
    const Operator *op(opName2op("timeinputscalar"));
    BOOST_CHECK(op);
    BOOST_CHECK(op->firstFieldInput() == 1);
  }
  {
    const Operator *op(opName2op("spread"));
    BOOST_CHECK(op);
    BOOST_CHECK(op->firstFieldInput() == 0);
  }
}

void calc::OperatorTest::testActualInput()
{
  {
    const Operator *op(opName2op("spread"));
    BOOST_CHECK(op->actualInput(0)==0);
    BOOST_CHECK(op->actualInput(1)==1);
  }
  {
    const Operator *op(opName2op("lookupscalar"));
    BOOST_CHECK(op->actualInput(0)==0);
    BOOST_CHECK(op->actualInput(1)==1);
    BOOST_CHECK(op->actualInput(2)==1);
    BOOST_CHECK(op->actualInput(3)==1);
  }
  {
    const Operator *op(opName2op("argorderwithidarealimited"));
    BOOST_CHECK(op);
    BOOST_CHECK(op->actualInput(0)==0);
    BOOST_CHECK(op->actualInput(1)==1);
    BOOST_CHECK(op->actualInput(2)==2);
    BOOST_CHECK(op->actualInput(3)==0);
    BOOST_CHECK(op->actualInput(4)==1);
    BOOST_CHECK(op->actualInput(5)==2);
    BOOST_CHECK(op->actualInput(6)==0);
  }
}
