#ifndef INCLUDED_CALC_KINEMATICTEST
#define INCLUDED_CALC_KINEMATICTEST

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace calc {


//       1         2         3         4         5         6         7         8

//! This class implements the unit tests for the Kinematic 
class KinematicTest
{

private:

public:

                   KinematicTest  ();

  void             iterate1       ();

  void             iterate2       ();

  static boost::unit_test::test_suite *    suite          ();

};

} // namespace calc

#endif
