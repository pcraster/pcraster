#ifndef INCLUDED_GEO_GRIDDEDPOINTSTEST
#define INCLUDED_GEO_GRIDDEDPOINTSTEST



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

namespace geo {
  // GriddedPoints declarations.
}



namespace geo {



//! This class implements the unit tests for the GriddedPoints class.
class GriddedPointsTest
{

public:

                   GriddedPointsTest   ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testNrPoints        ();

  void             testPoints          ();

  void             testCopy            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
