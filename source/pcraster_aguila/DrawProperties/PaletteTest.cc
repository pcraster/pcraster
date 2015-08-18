#ifndef INCLUDED_PALETTETEST
#include "PaletteTest.h"
#define INCLUDED_PALETTETEST
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
#ifndef INCLUDED_PALETTE
#include "Palette.h"
#define INCLUDED_PALETTE
#endif



/*!
  \file
  This file contains the implementation of the PaletteTest class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PALETTETEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* PaletteTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PaletteTest> instance(
         new PaletteTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &PaletteTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &PaletteTest::testCopy, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PALETTETEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
PaletteTest::PaletteTest()
{
}



void PaletteTest::test()
{
  std::vector<QColor> colors;
  colors.push_back(Qt::red);
  colors.push_back(Qt::white);
  colors.push_back(Qt::blue);

  // Default constructor.
  {
    Palette palette;
    BOOST_CHECK_EQUAL(palette.size(), 0u);

    palette.insert(palette.begin(), colors.begin(), colors.end());
    BOOST_CHECK_EQUAL(palette.size(), 3u);
    BOOST_CHECK(palette[0] == Qt::red);
    BOOST_CHECK(palette[1] == Qt::white);
    BOOST_CHECK(palette[2] == Qt::blue);
  }

  {
    Palette palette(colors.begin(), colors.end());
    BOOST_CHECK_EQUAL(palette.size(), 3u);
    BOOST_CHECK(palette[0] == Qt::red);
    BOOST_CHECK(palette[1] == Qt::white);
    BOOST_CHECK(palette[2] == Qt::blue);
  }
}



void PaletteTest::testCopy()
{
  std::vector<QColor> colors;
  colors.push_back(Qt::red);
  colors.push_back(Qt::white);
  colors.push_back(Qt::blue);

  {
    Palette const palette1(colors.begin(), colors.end());
    Palette palette2(palette1);

    BOOST_CHECK_EQUAL(palette2.size(), 3u);
    BOOST_CHECK(palette2[0] == Qt::red);
    BOOST_CHECK(palette2[1] == Qt::white);
    BOOST_CHECK(palette2[2] == Qt::blue);
  }
}

} // namespace ag

