#define BOOST_TEST_MODULE pcrblock compactors
#include <boost/test/unit_test.hpp>
#include "block_dehaancompactor.h"
#define private public
#include "block_compactors.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace block;

  Compactors<DeHaanCompactor> compactors;

  BOOST_CHECK(compactors.empty());
  BOOST_CHECK(!compactors.hasCompactor(3));

  DeHaanCompactor compactor(1.0, 2.0, 3.0);
  compactors.setCompactor(3, compactor);
  BOOST_CHECK(compactors.size() == 1);
  BOOST_CHECK(compactors.hasCompactor(3));
  BOOST_CHECK(compactors.compactor(3) == compactor);
}
