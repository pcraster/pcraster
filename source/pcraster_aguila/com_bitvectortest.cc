#include "com_bitvectortest.h"

// Library headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// PCRaster library headers.

// Module headers.
#include "com_bitvector.h"



/*!
  \file
  This file contains the implementation of the BitVectorTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC BITVECTOR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::BitVectorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BitVectorTest> instance(new BitVectorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BitVectorTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF BITVECTOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::BitVectorTest::BitVectorTest()
{
}



//! setUp
void com::BitVectorTest::setUp()
{
}



//! tearDown
void com::BitVectorTest::tearDown()
{
}



void com::BitVectorTest::test()
{
  BitVector bv(5);
  bv.set(0);
  bv.set(2);
  bv.set(4);

  BOOST_CHECK( bv[0]);
  BOOST_CHECK(!bv[1]);
  BOOST_CHECK( bv[2]);
  BOOST_CHECK(!bv[3]);
  BOOST_CHECK( bv[4]);
}
