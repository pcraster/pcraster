#include "stddefx.h"

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST
#include <boost/test/unit_test.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST
#endif

#include "mathx.h"

#include "mathx_dirconvtest.h"

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*mathx::DirConvTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DirConvTest> instance(new DirConvTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DirConvTest::testScaleDeg, instance));

  return suite;
}

//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

mathx::DirConvTest::DirConvTest(){
}

void mathx::DirConvTest::setUp()
{
}

void mathx::DirConvTest::tearDown()
{
}

void mathx::DirConvTest::testScaleDeg()
{
  BOOST_CHECK(ScaleDeg(45) == 45);
  BOOST_CHECK(ScaleDeg(-300) == 60);

  BOOST_CHECK(ScaleDeg(0) == 0);
  BOOST_CHECK(ScaleDeg(-360) == 0);
  BOOST_CHECK(ScaleDeg(360) == 0);

  // sin bug in pcrcalc Mon Aug 14 10:14:28 CEST 2000:
  BOOST_CHECK(ScaleDeg(-80640) == 0);
}
