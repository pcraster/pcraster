#define BOOST_TEST_MODULE pcraster moc particle
#include <boost/test/unit_test.hpp>
#include "pt_Particle.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace pt;

  {
    Particle p(0, 0, 1.0, 1.0, 5.5);
    BOOST_CHECK(p.concentration() == 5.5);
  }
}
