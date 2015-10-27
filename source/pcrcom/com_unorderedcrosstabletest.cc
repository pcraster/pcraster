#define BOOST_TEST_MODULE pcraster com unordered_cross_table
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_unorderedcrosstable.h"


// TODO Check that the table mirrors indices.
BOOST_AUTO_TEST_CASE(simple_usage)
{
  using namespace com;

  // Create a cross table.
  UnOrderedCrossTable t(3);
  BOOST_CHECK(t.size() == 3);
  BOOST_CHECK(t.nrCells() == 9);
  BOOST_CHECK(t.cell(0, 0) == 0);
  BOOST_CHECK(t.cell(0, 1) == 0);
  BOOST_CHECK(t.cell(0, 2) == 0);
  BOOST_CHECK(t.cell(1, 1) == 0);
  BOOST_CHECK(t.cell(1, 2) == 0);
  BOOST_CHECK(t.cell(2, 2) == 0);

  // Fill it as folows:
  //   6 5 4
  //   - 0 4
  //   - - 1
  t.cell(0, 0) = 6;
  t.cell(0, 1) = 5;
  t.cell(0, 2) = 4;
  t.cell(1, 1) = 0;
  t.cell(1, 2) = 4;
  t.cell(2, 2) = 1;

  // Check the contents:
  BOOST_CHECK(t.cell(0, 0) == 6);
  BOOST_CHECK(t.cell(0, 1) == 5);
  BOOST_CHECK(t.cell(0, 2) == 4);
  BOOST_CHECK(t.cell(1, 1) == 0);
  BOOST_CHECK(t.cell(1, 2) == 4);
  BOOST_CHECK(t.cell(2, 2) == 1);

  // Fill it as folows:
  // 1 - -
  // 2 3 -
  // 4 5 6
  t.cell(0, 0) = 1;
  t.cell(1, 0) = 2;
  t.cell(1, 1) = 3;
  t.cell(2, 0) = 4;
  t.cell(2, 1) = 5;
  t.cell(2, 2) = 6;

  BOOST_CHECK(t.cell(0, 0) == 1);
  BOOST_CHECK(t.cell(0, 1) == 2);
  BOOST_CHECK(t.cell(0, 2) == 4);
  BOOST_CHECK(t.cell(1, 1) == 3);
  BOOST_CHECK(t.cell(1, 2) == 5);
  BOOST_CHECK(t.cell(2, 2) == 6);
}
