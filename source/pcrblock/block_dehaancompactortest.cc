#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_DEHAANCOMPACTORTEST
#include "block_dehaancompactortest.h"
#define INCLUDED_BLOCK_DEHAANCOMPACTORTEST
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
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.
#ifndef INCLUDED_BLOCK_DEHAANCOMPACTOR
#include "block_dehaancompactor.h"
#define INCLUDED_BLOCK_DEHAANCOMPACTOR
#endif



/*!
  \file
  This file contains the implementation of the DeHaanCompactorTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DEHAANCOMPACTORTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*DeHaanCompactorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DeHaanCompactorTest> instance(new DeHaanCompactorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DeHaanCompactorTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DEHAANCOMPACTORTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
DeHaanCompactorTest::DeHaanCompactorTest(
         )
{
}



//! setUp
void DeHaanCompactorTest::setUp()
{
}



//! tearDown
void DeHaanCompactorTest::tearDown()
{
}



void DeHaanCompactorTest::test()
{
  // {
  //   DeHaanCompactor compactor;
  //   BOOST_CHECK(dal::comparable(compactor( 4.0,   5.0, 6.0), REAL4(0.0)));
  // }

  {
    DeHaanCompactor compactor(0.2, 0.02, 6.0);
    BOOST_CHECK(dal::comparable(compactor( 0.0, 5.0, 6.0), REAL4(0.0)));
    BOOST_CHECK(dal::comparable(compactor( 0.0, 5.0, 0.0), REAL4(0.0)));
    BOOST_CHECK(dal::comparable(compactor( 0.0, 0.0, 0.0), REAL4(0.0)));
    BOOST_CHECK(dal::comparable(compactor( 0.0, 0.0, 6.0), REAL4(0.0)));
    BOOST_CHECK(dal::comparable(compactor( 4.0, 5.0, 0.0), REAL4(4.0)));
    BOOST_CHECK(dal::comparable(compactor( 4.0, 0.0, 0.0), REAL4(4.0)));
  }
}

} // namespace block

