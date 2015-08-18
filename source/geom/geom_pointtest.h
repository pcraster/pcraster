#ifndef INCLUDED_GEOM_POINTTEST
#define INCLUDED_GEOM_POINTTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace geom {
  // Point declarations.
}



namespace geom {



//! This class implements the unit tests for the Point class.
class PointTest
{

public:

                   PointTest           ();

  void             testPerpOnCord      ();

  static boost::unit_test::test_suite *    suite               ();

};

} // namespace geom

#endif
