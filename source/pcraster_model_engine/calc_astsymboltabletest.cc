#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTSYMBOLTABLETEST
#include "calc_astsymboltabletest.h"
#define INCLUDED_CALC_ASTSYMBOLTABLETEST
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
  This file contains the implementation of the ASTSymbolTableTest class.
*/




//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTSYMBOLTABLE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ASTSymbolTableTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ASTSymbolTableTest> instance(new ASTSymbolTableTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ASTSymbolTableTest::testResolve, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF ASTSYMBOLTABLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ASTSymbolTableTest::ASTSymbolTableTest()
{
}



//! setUp
void calc::ASTSymbolTableTest::setUp()
{
}

//! tearDown
void calc::ASTSymbolTableTest::tearDown()
{
}


#define EXPECT_ERROR_ID(msgId) \
  { std::auto_ptr<ASTScript> script(createFromIdOrStr(msgId)); \
    TRY_TEST_MSG {                 \
       script->analyzeAndResolve();          \
    } CATCH_TEST_MSG(msgId);       \
  }
/*
 * \todo move to ASTScriptTest
 */
void calc::ASTSymbolTableTest::testResolve()
{
    EXPECT_ERROR_ID("pcrcalc213b");
    EXPECT_ERROR_ID("pcrcalc10b");
    EXPECT_ERROR_ID("pcrcalc289");
    EXPECT_ERROR_ID("pcrcalc381a");
    EXPECT_ERROR_ID("pcrcalc10c");
}
