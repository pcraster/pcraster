#define BOOST_TEST_MODULE pcraster com rle_ptr_vector_test
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <algorithm>
#include "com_rleptrvector.h"


BOOST_AUTO_TEST_CASE(rle_item)
{
#ifndef __x86_64__
  using namespace com;

  {
  typedef RLEItem<int,4> I4;
  int data;
  int *ptr=&data;

  I4 i;
  BOOST_CHECK(i.basePtr(ptr)==ptr);
  BOOST_CHECK(i.count(ptr)==0);
  int *ptrWithCount=(int *)(((int)ptr)+1);
  BOOST_CHECK(i.basePtr(ptrWithCount)==ptr);
  BOOST_CHECK(i.count(ptrWithCount)==1);

  ptrWithCount=(int *)(((int)ptr)+2);
  BOOST_CHECK(i.basePtr(ptrWithCount)==ptr);
  BOOST_CHECK(i.count(ptrWithCount)==2);

  ptrWithCount=(int *)(((int)ptr)+3);
  BOOST_CHECK(i.basePtr(ptrWithCount)==ptr);
  BOOST_CHECK(i.count(ptrWithCount)==3);

  ptrWithCount=(int *)(((int)ptr)+4);
  BOOST_CHECK(i.basePtr(ptrWithCount)==(ptr+1)); /* point to next integer */
  BOOST_CHECK(i.count(ptrWithCount)==0);
  }
  {
    typedef int DATA[3];
    typedef RLEItem<DATA,12> I12;
    DATA data;
    DATA *ptr=&data;

    I12 i;
    BOOST_CHECK(i.basePtr(ptr)==ptr);
    BOOST_CHECK(i.count(ptr)==0);
    DATA *ptrWithCount=(DATA *)(((int)ptr)+1);
    BOOST_CHECK(i.basePtr(ptrWithCount)==ptr);
    BOOST_CHECK(i.count(ptrWithCount)==1);

    ptrWithCount=(DATA *)(((int)ptr)+2);
    BOOST_CHECK(i.basePtr(ptrWithCount)==ptr);
    BOOST_CHECK(i.count(ptrWithCount)==2);

    ptrWithCount=(DATA *)(((int)ptr)+3);
    BOOST_CHECK(i.basePtr(ptrWithCount)==ptr);
    BOOST_CHECK(i.count(ptrWithCount)==3);

    ptrWithCount=(DATA *)(((int)ptr)+4);
    // to 2nd field in DATA
    BOOST_CHECK((int *)(i.basePtr(ptrWithCount))==(((int *)ptr)+1));
    BOOST_CHECK(i.count(ptrWithCount)==0);
  }
#endif
}


BOOST_AUTO_TEST_CASE(push_back)
{
#ifndef __x86_64__
  using namespace com;

    typedef RLEPtrVector<int> RLEPV;
    int elem[] = { 7,9,10};
   { // order for full rle
    RLEPV d;
    d.push_back(elem+0);
    BOOST_CHECK(d.capacity()==4);
    d.push_back(elem+1);
    BOOST_CHECK(d.capacity()==4);
    d.push_back(elem+2);
    BOOST_CHECK(d.capacity()==4);
    BOOST_CHECK(d.size()==3);
   }
   { // order that can not have rle
    RLEPV d;
    d.push_back(elem+0);
    BOOST_CHECK(d.size()==1);
    BOOST_CHECK(d.capacity()==4);
    d.push_back(elem+2);
    BOOST_CHECK(d.size()==2);
    BOOST_CHECK(d.capacity()==8);
    d.push_back(elem+1);
    BOOST_CHECK(d.capacity()==12);
    BOOST_CHECK(d.size()==3);
   }
   { // order for full rle and extended count
    RLEPV d;
    for(size_t i=0; i < 13; ++i) {
      BOOST_CHECK(d.size()==i);
      d.push_back(elem+i);
      BOOST_CHECK(d.capacity()<=8);
     }
   }
#endif
}

namespace com {
struct TEST_PERM12 {
     int s1,sDontCare,sFillUpTo12bytes;
};
struct TEST_PERM4 {
     int s1;
};

}


BOOST_AUTO_TEST_CASE(iterator)
{
#ifndef __x86_64__
  using namespace com;

   { // EMPTY container
    RLEPtrVector<int> empty;
    RLEPtrVector<int>::const_iterator i(empty);
    BOOST_CHECK(i==empty.begin());
    BOOST_CHECK(!(i!=empty.begin()));
    BOOST_CHECK(i==empty.end());
    BOOST_CHECK(!(i!=empty.end()));
    BOOST_CHECK(empty.end()==empty.begin());
    BOOST_CHECK(!(empty.end()!=empty.begin()));
   }
   { // simple iteration
    int elem[] = { 9,19,29};
    RLEPtrVector<int> d;
    RLEPtrVector<int>::const_iterator i(d);
    d.push_back(elem+0);
    d.push_back(elem+1);
    d.push_back(elem+2);

    int c=0;
    for(i=d.begin(); i!=d.end(); ++i) {
      int *p = *i;
      BOOST_CHECK(*p == (9+(c++*10)));
     }
    BOOST_CHECK(c==3);
   }

   const size_t nrRleTests(8); // 9 is better when changing
//   size_t rleTests[9] = {0,1,2,3,4,5,6,7,8};
     // more than 8 will take considerable time

   for(size_t t=0; t< nrRleTests; t++) {
     // exhaustive test with ptr to 12 byte items
     size_t nrElems=t;
     BOOST_CHECK(sizeof(TEST_PERM12)==12);
     std::vector<TEST_PERM12> data(nrElems);
     std::vector<TEST_PERM12 *> ptrPerm(nrElems);
     for(size_t i=0; i < nrElems; ++i) {
       data[i].s1=i;
       ptrPerm[i]=&(data[i]);
     }

     do {
        RLEPtrVector<TEST_PERM12> d;
        for(size_t e=0; e < nrElems; e++) {
          d.push_back(ptrPerm[e]);
        }
        BOOST_CHECK(d.size()==nrElems);
        // no overhead except for the empty case
        BOOST_CHECK(d.empty() || d.capacity()<=d.size()*4);
        size_t p=0;
        RLEPtrVector<TEST_PERM12>::const_iterator i(d);
        for(i=d.begin(); i!=d.end(); ++i) {
          BOOST_CHECK((*i)->s1==ptrPerm[p]->s1);
          p++;
        }
        BOOST_CHECK(p==nrElems);
     } while(std::next_permutation(ptrPerm.begin(),ptrPerm.end()));
   }
   for(size_t t=0; t< nrRleTests; t++) {
     // exhaustive test with ptr to 4 byte items
     size_t nrElems=t;
     BOOST_CHECK(sizeof(TEST_PERM4)==4);
     std::vector<TEST_PERM4> data(nrElems);
     std::vector<TEST_PERM4 *> ptrPerm(nrElems);
     for(size_t e=0; e < nrElems; ++e) {
       data[e].s1=e;
       ptrPerm[e]=&(data[e]);
     }

     do {
        RLEPtrVector<TEST_PERM4> d;
        for(size_t e=0; e < nrElems; e++) {
          d.push_back(ptrPerm[e]);
        }
        BOOST_CHECK(d.size()==nrElems);
        // no overhead except for the empty case
        BOOST_CHECK(d.empty() || d.capacity()<=d.size()*4);
        size_t p=0;
        RLEPtrVector<TEST_PERM4>::const_iterator i(d);
        for(i=d.begin(); i!=d.end(); ++i) {
          BOOST_CHECK((*i)->s1==ptrPerm[p]->s1);
          p++;
        }
        BOOST_CHECK(p==nrElems);
     } while(std::next_permutation(ptrPerm.begin(),ptrPerm.end()));
   }
#endif
}
