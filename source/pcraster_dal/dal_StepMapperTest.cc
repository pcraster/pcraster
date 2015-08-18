#ifndef INCLUDED_DAL_STEPMAPPERTEST
#include "dal_StepMapperTest.h"
#define INCLUDED_DAL_STEPMAPPERTEST
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
#ifndef INCLUDED_DAL_STEPMAPPER
#include "dal_StepMapper.h"
#define INCLUDED_DAL_STEPMAPPER
#endif

#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif



/*!
  \file
  This file contains the implementation of the StepMapperTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC STEPMAPPER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*StepMapperTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<StepMapperTest> instance(new StepMapperTest());

  suite->add(BOOST_CLASS_TEST_CASE(&StepMapperTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF STEPMAPPER MEMBERS
//------------------------------------------------------------------------------

//! ctor
StepMapperTest::StepMapperTest(
         )
{
}



//! setUp
void StepMapperTest::setUp()
{
}



//! tearDown
void StepMapperTest::tearDown()
{
}



void StepMapperTest::test()
{
  {
    StepMapper mapper(1.0, 2.0, 100.0, 200.0);
    BOOST_CHECK(comparable(mapper.destination(-1.0), -100.0));
    BOOST_CHECK(comparable(mapper.destination( 0.0),    0.0));
    BOOST_CHECK(comparable(mapper.destination( 1.0),  100.0));
    BOOST_CHECK(comparable(mapper.destination( 2.0),  200.0));
    BOOST_CHECK(comparable(mapper.destination( 3.0),  300.0));
  }

  {
    StepMapper mapper(1.0, 250.0, 1.0, 250.0);
    BOOST_CHECK_EQUAL(mapper.d_conversionFactor, 1.0);
    BOOST_CHECK(comparable(mapper.destination(167.0), 167.0));
    BOOST_CHECK(comparable(std::fmod(mapper.destination(167.0), 1.0), 0.0));
  }
}

} // namespace dal

