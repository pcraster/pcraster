#ifndef INCLUDED_DAL_SPACESTEPMAPPERTEST
#include "dal_SpaceStepMapperTest.h"
#define INCLUDED_DAL_SPACESTEPMAPPERTEST
#endif

// External headers.
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

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

#ifndef INCLUDED_DAL_SPACESTEPMAPPER
#include "dal_SpaceStepMapper.h"
#define INCLUDED_DAL_SPACESTEPMAPPER
#endif



/*!
  \file
  This file contains the implementation of the SpaceStepMapperTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPACESTEPMAPPERTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* SpaceStepMapperTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SpaceStepMapperTest> instance(
         new SpaceStepMapperTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &SpaceStepMapperTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SPACESTEPMAPPERTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
SpaceStepMapperTest::SpaceStepMapperTest()
{
}



void SpaceStepMapperTest::test()
{
  {
    SpaceStepMapper mapper;
    BOOST_CHECK(!mapper.isValid());
  }

  {
    SpaceStepMapper mapper(0, 5.0, 0.5);

    BOOST_CHECK(comparable(mapper.destination(1.0), 5.5));
    BOOST_CHECK(comparable(mapper.destination(0.0), 5.0));
    BOOST_CHECK(comparable(mapper.destination(-1.0), 4.5));

    BOOST_CHECK(comparable(mapper.destination(0.5), 5.25));
    BOOST_CHECK(comparable(mapper.destination(-0.5), 4.75));

    BOOST_CHECK(comparable(mapper.source(5.5), 1.0));
    BOOST_CHECK(comparable(mapper.source(5.0), 0.0));
    BOOST_CHECK(comparable(mapper.source(4.5), -1.0));
    BOOST_CHECK(comparable(mapper.source(5.25), 0.5));
    BOOST_CHECK(comparable(mapper.source(4.75), -0.5));
  }

  {
    SpaceStepMapper mapper(0, 74.85, -0.30);

    BOOST_CHECK(comparable(mapper.destination(0), 74.85));
    BOOST_CHECK(comparable(mapper.destination(1), 74.55));
    BOOST_CHECK(comparable(mapper.destination(-0.5), 75.0));
    BOOST_CHECK(comparable(mapper.source(0.15), 249.0));

    BOOST_CHECK(comparable(mapper.source(74.85), 0.0));
    BOOST_CHECK(comparable(mapper.source(74.55), 1.0));
    BOOST_CHECK(comparable(mapper.source(75.0), -0.5));
  }

  {
    SpaceStepMapper mapper(1, 74.85, -0.30);

    BOOST_CHECK(comparable(mapper.destination(1), 74.85));
    BOOST_CHECK(comparable(mapper.destination(2), 74.55));
    BOOST_CHECK(comparable(mapper.destination(0.5), 75.0));

    BOOST_CHECK(comparable(mapper.source(74.85), 1.0));
    BOOST_CHECK(comparable(mapper.source(74.55), 2.0));
    BOOST_CHECK(comparable(mapper.source(75.0), 0.5));
  }
}

} // namespace dal

