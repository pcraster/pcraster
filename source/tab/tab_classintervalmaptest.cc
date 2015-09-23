#define BOOST_TEST_MODULE pcraster tab class_interval_map
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_statistics.h"
#include "com_intervaltypes.h"
#include "tab_classintervalmap.h"


BOOST_AUTO_TEST_CASE(class_interval_map)
{
  using namespace tab;

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
