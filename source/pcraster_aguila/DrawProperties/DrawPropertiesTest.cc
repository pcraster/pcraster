#ifndef INCLUDED_DRAWPROPERTIESTEST
#include "DrawPropertiesTest.h"
#define INCLUDED_DRAWPROPERTIESTEST
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
#ifndef INCLUDED_DRAWPROPERTIES
#include "DrawProperties.h"
#define INCLUDED_DRAWPROPERTIES
#endif



/*!
  \file
  This file contains the implementation of the DrawPropertiesTest class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DRAWPROPERTIESTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* DrawPropertiesTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DrawPropertiesTest> instance(
         new DrawPropertiesTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &DrawPropertiesTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DRAWPROPERTIESTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
DrawPropertiesTest::DrawPropertiesTest()
{
}



void DrawPropertiesTest::test()
{
  std::vector<QColor> colors;
  colors.push_back(Qt::red);
  colors.push_back(Qt::white);
  colors.push_back(Qt::blue);

  // Default constructor.
  {
    DrawProperties drawProperties;
    BOOST_CHECK(drawProperties.pen() == QPen());
    BOOST_CHECK(drawProperties.attributePen() == QPen());
    BOOST_CHECK(drawProperties.brush() == QBrush());
    BOOST_CHECK(drawProperties.palette() == Palette());
  }

  {
    DrawProperties drawProperties(QPen(Qt::red), QPen(Qt::white),
         QBrush(Qt::blue), Palette(colors.begin(), colors.end()));
    BOOST_CHECK(drawProperties.pen() == QPen(Qt::red));
    BOOST_CHECK(drawProperties.attributePen() == QPen(Qt::white));
    BOOST_CHECK(drawProperties.brush() == QBrush(Qt::blue));
    BOOST_CHECK(drawProperties.palette() ==
         Palette(colors.begin(), colors.end()));
  }
}

} // namespace ag

