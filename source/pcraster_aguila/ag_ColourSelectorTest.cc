#define BOOST_TEST_MODULE pcraster aguila colour_selector
#include <boost/test/unit_test.hpp>
#include "ag_ColourSelector.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace ag;
  using namespace com;

  std::vector<QColor> selection;
  RawPalette palette(*RawPalette::nominalPalette());
  size_t nrClasses;

  // Non empty palette, 0 classes.
  {
    nrClasses = 0;
    selection = mapEqualInterval(palette, nrClasses);
    BOOST_CHECK(selection.empty());
  }

  // Non empty palette, 5 classes.
  {
    nrClasses = 5;
    selection = mapEqualInterval(palette, nrClasses);
    BOOST_CHECK_EQUAL(selection.size(), nrClasses);
  }
}
