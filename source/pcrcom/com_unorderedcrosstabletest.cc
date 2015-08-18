#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_UNORDEREDCROSSTABLETEST
#include "com_unorderedcrosstabletest.h"
#define INCLUDED_COM_UNORDEREDCROSSTABLETEST
#endif

// Library headers.
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

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_UNORDEREDCROSSTABLE
#include "com_unorderedcrosstable.h"
#define INCLUDED_COM_UNORDEREDCROSSTABLE
#endif



/*!
  \file
  This file contains the implementation of the UnOrderedCrossTableTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC UNORDEREDCROSSTABLE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::UnOrderedCrossTableTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<UnOrderedCrossTableTest> instance(new UnOrderedCrossTableTest());

  suite->add(BOOST_CLASS_TEST_CASE(&UnOrderedCrossTableTest::testSimpleUsage, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF UNORDEREDCROSSTABLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::UnOrderedCrossTableTest::UnOrderedCrossTableTest()
{
}



//! setUp
void com::UnOrderedCrossTableTest::setUp()
{
}

//! tearDown
void com::UnOrderedCrossTableTest::tearDown()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Check that the table mirrors indices.
*/
void com::UnOrderedCrossTableTest::testSimpleUsage()
{
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


