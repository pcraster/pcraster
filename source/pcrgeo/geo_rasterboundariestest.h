#ifndef INCLUDED_GEO_RASTERBOUNDARIESTEST
#define INCLUDED_GEO_RASTERBOUNDARIESTEST



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
  // RasterBoundaries declarations.
}



namespace geo {



//! This class implements the unit tests for the RasterBoundaries class.
class RasterBoundariesTest
{

public:

                   RasterBoundariesTest();

  void             setUp               ();

  void             tearDown            ();

  void             testIndexLeft       ();

  void             testIndexTop        ();

  void             testIndexRight      ();

  void             testIndexBottom     ();

  void             testBoundaries      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
