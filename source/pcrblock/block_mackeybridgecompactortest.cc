#define BOOST_TEST_MODULE pcrblock mackey_bridge_compactor
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "block_dummycompactor.h"
#include "block_sandcompactor.h"


BOOST_AUTO_TEST_CASE(dummy_compactor)
{
  using namespace block;

  DummyCompactor compactor;
  BOOST_CHECK(dal::comparable(compactor( 0.0,  0.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 5.0,  0.0), REAL4(5.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, -5.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0,  5.0), REAL4(0.0)));
}


BOOST_AUTO_TEST_CASE(sand_compactor)
{
  using namespace block;

  SandCompactor compactor;
  BOOST_CHECK(dal::comparable(compactor( 0.0,   0.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, 100.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, 100.0), REAL4(0.0)));
}


BOOST_AUTO_TEST_CASE(clay_compactor)
{
  using namespace block;

  SandCompactor compactor;
  BOOST_CHECK(dal::comparable(compactor( 0.0,   0.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, 100.0), REAL4(0.0)));
  BOOST_CHECK(dal::comparable(compactor( 0.0, 100.0), REAL4(0.0)));
}
