#include "ag_ColourSelectorTest.h"

// External headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// Project headers.

// Module headers.
#include "ag_ColourSelector.h"



/*!
  \file
  This file contains the implementation of the ColourSelectorTest class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC COLOURSELECTORTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* ColourSelectorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ColourSelectorTest> instance(new ColourSelectorTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &ColourSelectorTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF COLOURSELECTORTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
ColourSelectorTest::ColourSelectorTest()
{
}



void ColourSelectorTest::test()
{
  std::vector<QColor> selection;
  com::RawPalette palette(*com::RawPalette::nominalPalette());
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

} // namespace ag

