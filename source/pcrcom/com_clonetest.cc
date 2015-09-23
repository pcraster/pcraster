#define BOOST_TEST_MODULE pcraster com clone
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_intervaltypes.h"
#include "com_clone.h"


BOOST_AUTO_TEST_CASE(reset_clone)
{
  using namespace com;

  // typedef Interval<float> I; // virtual base of I
  typedef EqualTo<float>  E;
  {
    E* dest=new E(4);
    E* src(0);
    resetClone(dest,src);
    BOOST_CHECK(dest==0);

    dest=new E(8);
    BOOST_CHECK(dest->min()==8);
    resetClone(dest,new E(2));
    BOOST_CHECK(dest->min()==2);

    delete dest;
  }
}
