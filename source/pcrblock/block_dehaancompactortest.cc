#define BOOST_TEST_MODULE pcrblock de_haan_compactor
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "block_dehaancompactor.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace block;

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
