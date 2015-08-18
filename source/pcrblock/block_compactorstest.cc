#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCK_COMPACTORSTEST
#include "block_compactorstest.h"
#define INCLUDED_BLOCK_COMPACTORSTEST
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
#ifndef INCLUDED_BLOCK_COMPACTORS
#include "block_compactors.h"
#define INCLUDED_BLOCK_COMPACTORS
#endif

#ifndef INCLUDED_BLOCK_DEHAANCOMPACTOR
#include "block_dehaancompactor.h"
#define INCLUDED_BLOCK_DEHAANCOMPACTOR
#endif



/*!
  \file
  This file contains the implementation of the CompactorsTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace block {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMPACTORS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*CompactorsTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CompactorsTest> instance(new CompactorsTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CompactorsTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF COMPACTORS MEMBERS
//------------------------------------------------------------------------------

//! ctor
CompactorsTest::CompactorsTest(
         )
{
}



//! setUp
void CompactorsTest::setUp()
{
}



//! tearDown
void CompactorsTest::tearDown()
{
}



void CompactorsTest::test()
{
  Compactors<DeHaanCompactor> compactors;

  BOOST_CHECK(compactors.empty());
  BOOST_CHECK(!compactors.hasCompactor(3));

  DeHaanCompactor compactor(1.0, 2.0, 3.0);
  compactors.setCompactor(3, compactor);
  BOOST_CHECK(compactors.size() == 1);
  BOOST_CHECK(compactors.hasCompactor(3));
  BOOST_CHECK(compactors.compactor(3) == compactor);
}

} // namespace block

