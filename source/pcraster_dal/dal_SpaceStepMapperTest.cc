#define BOOST_TEST_MODULE pcraster dal space step mapper
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "dal_SpaceStepMapper.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace dal;

  {
    SpaceStepMapper const mapper;
    BOOST_TEST(!mapper.isValid());
  }

  {
    SpaceStepMapper const mapper(0, 5.0, 0.5);

    BOOST_TEST(comparable(mapper.destination(1.0), 5.5));
    BOOST_TEST(comparable(mapper.destination(0.0), 5.0));
    BOOST_TEST(comparable(mapper.destination(-1.0), 4.5));

    BOOST_TEST(comparable(mapper.destination(0.5), 5.25));
    BOOST_TEST(comparable(mapper.destination(-0.5), 4.75));

    BOOST_TEST(comparable(mapper.source(5.5), 1.0));
    BOOST_TEST(comparable(mapper.source(5.0), 0.0));
    BOOST_TEST(comparable(mapper.source(4.5), -1.0));
    BOOST_TEST(comparable(mapper.source(5.25), 0.5));
    BOOST_TEST(comparable(mapper.source(4.75), -0.5));
  }

  {
    SpaceStepMapper const mapper(0, 74.85, -0.30);

    BOOST_TEST(comparable(mapper.destination(0), 74.85));
    BOOST_TEST(comparable(mapper.destination(1), 74.55));
    BOOST_TEST(comparable(mapper.destination(-0.5), 75.0));
    BOOST_TEST(comparable(mapper.source(0.15), 249.0));

    BOOST_TEST(comparable(mapper.source(74.85), 0.0));
    BOOST_TEST(comparable(mapper.source(74.55), 1.0));
    BOOST_TEST(comparable(mapper.source(75.0), -0.5));
  }

  {
    SpaceStepMapper const mapper(1, 74.85, -0.30);

    BOOST_TEST(comparable(mapper.destination(1), 74.85));
    BOOST_TEST(comparable(mapper.destination(2), 74.55));
    BOOST_TEST(comparable(mapper.destination(0.5), 75.0));

    BOOST_TEST(comparable(mapper.source(74.85), 1.0));
    BOOST_TEST(comparable(mapper.source(74.55), 2.0));
    BOOST_TEST(comparable(mapper.source(75.0), 0.5));
  }
}
