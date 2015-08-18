#ifndef INCLUDED_DAL_TABLEDALTEST
#include "dal_TableDalTest.h"
#define INCLUDED_DAL_TABLEDALTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SCOPED_PTR
#include <boost/scoped_ptr.hpp>
#define INCLUDED_BOOST_SCOPED_PTR
#endif

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
#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif

#ifndef INCLUDED_DAL_TABLEDAL
#include "dal_TableDal.h"
#define INCLUDED_DAL_TABLEDAL
#endif

#ifndef INCLUDED_DAL_TABLEDRIVER
#include "dal_TableDriver.h"
#define INCLUDED_DAL_TABLEDRIVER
#endif



/*!
  \file
  This file contains the implementation of the TableDalTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLEDALTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*TableDalTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TableDalTest> instance(new TableDalTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TableDalTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF TABLEDALTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
TableDalTest::TableDalTest()
{
}



//! setUp
void TableDalTest::setUp()
{
}



//! tearDown
void TableDalTest::tearDown()
{
}



void TableDalTest::test()
{
  {
    // name = currentPathname + sometablename;
    std::string name = "table1.eas";
    TableDal dal;
    BOOST_CHECK(dal.hasDriverByName("Geo-EAS"));
    BOOST_CHECK_EQUAL(dal.driverByName("Geo-EAS")->datasetType(), TABLE);
    boost::shared_ptr<Table> table;
    boost::tie(table, boost::tuples::ignore) = dal.open(name);
    BOOST_CHECK(table.get());
  }
}

} // namespace dal

