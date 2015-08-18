#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_TAB_CLASSINTERVALMAPTEST
#include "tab_classintervalmaptest.h"
#define INCLUDED_TAB_CLASSINTERVALMAPTEST
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
#ifndef INCLUDED_COM_STATISTICS
#include "com_statistics.h"
#define INCLUDED_COM_STATISTICS
#endif
#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif

// Module headers.
#ifndef INCLUDED_TAB_CLASSINTERVALMAP
#include "tab_classintervalmap.h"
#define INCLUDED_TAB_CLASSINTERVALMAP
#endif



/*!
  \file
  This file contains the implementation of the ClassIntervalMapTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASSINTERVALMAP MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*tab::ClassIntervalMapTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ClassIntervalMapTest> instance(new ClassIntervalMapTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ClassIntervalMapTest::test1, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASSINTERVALMAP MEMBERS
//------------------------------------------------------------------------------

//! ctor
tab::ClassIntervalMapTest::ClassIntervalMapTest()
{
}

void tab::ClassIntervalMapTest::test1()
{
  std::vector<const com::Interval<float> *> v;
  v.push_back(new com::LessThan<float>(4));
  v.push_back(new com::GreaterThan<float>(4));

  {
   typedef ClassIntervalMap< com::IntervalMap< com::AverageSdMinMax< >, float > > M;
   M m(v);
   m.addClass(3);
   m.addClass(1);

   m.visit(1, 2);
   m.visit(1, 4); // in outside!
   m.visit(2, 2); // class implicit added
   m.visit(3, 2);
   m.visit(3, 5);

   size_t c=0;
   for(M::const_iterator i=m.begin();i!=m.end();++i) {
     BOOST_CHECK(i->second.size()==2); // < 4  and > 4
     switch(c) {
       case 0:
         BOOST_CHECK(i->first==1);
         BOOST_CHECK(i->second.outside().nr()==1);
         BOOST_CHECK(i->second.outside().maximum()==4);
         break;
       case 1:
         BOOST_CHECK(i->first==2);
         BOOST_CHECK(i->second.outside().nr()==0);
         break;
       case 2:
         BOOST_CHECK(i->first==3);
         BOOST_CHECK(i->second.outside().nr()==0);
         break;
     }
     c++;
   }
  }

  {
   v.push_back(new com::GreaterThan<float>(4.5));
   typedef tab::ClassIntervalMap< com::IntervalMultiMap< com::AverageSdMinMax<>,float > > M;
   M m(v);
   m.addClass(3);
   m.addClass(1);

   m.visit(1, 2);
   m.visit(1, 4); // in outside!
   m.visit(2, 2); // class implicit added
   m.visit(3, 2);
   m.visit(3, 5);

   size_t c=0;
   for(M::const_iterator i=m.begin();i!=m.end();++i) {
     BOOST_CHECK(i->second.size()==3); // < 4, > 4 > 4.5
     switch(c) {
       case 0:
         BOOST_CHECK(i->first==1);
         BOOST_CHECK(i->second.outside().nr()==1);
         BOOST_CHECK(i->second.outside().maximum()==4);
         break;
       case 1:
         BOOST_CHECK(i->first==2);
         BOOST_CHECK(i->second.outside().nr()==0);
         break;
       case 2:
         BOOST_CHECK(i->first==3);
         BOOST_CHECK(i->second.outside().nr()==0);
         break;
     }
     c++;
   }
  }
  clearClone(v);
}
