#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_TAB_CLASSCOUNTMAPTEST
#include "tab_classcountmaptest.h"
#define INCLUDED_TAB_CLASSCOUNTMAPTEST
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
#ifndef INCLUDED_TAB_CLASSCOUNTMAP
#include "tab_classcountmap.h"
#define INCLUDED_TAB_CLASSCOUNTMAP
#endif
#ifndef INCLUDED_TAB_CLASSCLASSCOUNTMAP
#include "tab_classclasscountmap.h"
#define INCLUDED_TAB_CLASSCLASSCOUNTMAP
#endif


/*!
  \file
  This file contains the implementation of the ClassCountMapTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASSCOUNTMAP MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*tab::ClassCountMapTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ClassCountMapTest> instance(new ClassCountMapTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ClassCountMapTest::testCountMap, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ClassCountMapTest::testClassClassCountMap, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASSCOUNTMAP MEMBERS
//------------------------------------------------------------------------------

//! ctor
tab::ClassCountMapTest::ClassCountMapTest()
{
}

void tab::ClassCountMapTest::testCountMap()
{
  ClassCountMap<> m;
  std::set<int>   s;

  BOOST_CHECK(s==m.classes());
  BOOST_CHECK(m.size()==0);

  m.addClass(4);

  BOOST_CHECK(m.size()==1);
  BOOST_CHECK(s!=m.classes());
  s.insert(4);
  BOOST_CHECK(s==m.classes());
  s.insert(4);

  m.incr(3);
  BOOST_CHECK(m[3]==1);
  m.incr(3);

  BOOST_CHECK(m[4]==0);
  BOOST_CHECK(m[3]==2);
  BOOST_CHECK(m[1]==0); // implicit add

  BOOST_CHECK(m.size()==3); // 1,3,4

  s.insert(3);
  s.insert(1);

  BOOST_CHECK(s==m.classes());

  m.addClass(3); // no effect, already in
  BOOST_CHECK(m[3]==2);
  BOOST_CHECK(s==m.classes());

  BOOST_CHECK(m.getCount(4)==0);
  BOOST_CHECK(m.getCount(3)==2);
  BOOST_CHECK(m.getCount(1)==0);
  BOOST_CHECK(m.getCount(9)==0);
}

void tab::ClassCountMapTest::testClassClassCountMap()
{
  ClassClassCountMap<> m;
  std::set<int>   row,col;

  BOOST_CHECK(row==m.rowClasses());
  BOOST_CHECK(col==m.colClasses());
  BOOST_CHECK(m.size()==0);

  m.addClass(4,2);

  BOOST_CHECK(m.size()==1);
  BOOST_CHECK(row!=m.rowClasses());
  BOOST_CHECK(col!=m.colClasses());
  row.insert(4);
  col.insert(2);
  BOOST_CHECK(row==m.rowClasses());
  BOOST_CHECK(col==m.colClasses());

  m.incr(3,5);
  m.incr(3,5);
  m.addClass(3,2);

  row.insert(3);
  col.insert(5);
  BOOST_CHECK(row==m.rowClasses());
  BOOST_CHECK(col==m.colClasses());

  m.incr(4,2);

  /*  * 2 5
   *  3 0 2
   *  4 1 0
   */
  BOOST_CHECK(m.getCount(3,2)==0);
  BOOST_CHECK(m.getCount(3,5)==2);
  BOOST_CHECK(m.getCount(4,2)==1);
  BOOST_CHECK(m.getCount(4,5)==0);
  BOOST_CHECK(m.getCount(9,9)==0);

  m.addClass(3,5); // no effect, already in
  BOOST_CHECK(m.getCount(3,5)==2);

  BOOST_CHECK(4==(m.rowClasses().size()*m.colClasses().size()));
}
