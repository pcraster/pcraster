#define BOOST_TEST_MODULE pcraster dal step_mapper
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#define private public
#include "dal_StepMapper.h"


BOOST_AUTO_TEST_CASE(step_mapper)
{
  using namespace dal;

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
