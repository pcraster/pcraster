#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PT_PARTICLETEST
#include "pt_ParticleTest.h"
#define INCLUDED_PT_PARTICLETEST
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
#ifndef INCLUDED_PT_PARTICLE
#include "pt_Particle.h"
#define INCLUDED_PT_PARTICLE
#endif



/*!
  \file
  This file contains the implementation of the ParticleTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PARTICLE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pt::ParticleTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ParticleTest> instance(new ParticleTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ParticleTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PARTICLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
pt::ParticleTest::ParticleTest()
{
}



//! setUp
void pt::ParticleTest::setUp()
{
}



//! tearDown
void pt::ParticleTest::tearDown()
{
}



void pt::ParticleTest::test()
{
  {
    Particle p(0, 0, 1.0, 1.0, 5.5);
    BOOST_CHECK(p.concentration() == 5.5);
  }
}

