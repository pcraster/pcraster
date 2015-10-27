#define BOOST_TEST_MODULE pcraster tab class_count_map
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "tab_classcountmap.h"
#include "tab_classclasscountmap.h"


BOOST_AUTO_TEST_CASE(count_map)
{
  using namespace tab;

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


BOOST_AUTO_TEST_CASE(class_class_count_map)
{
  using namespace tab;

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
