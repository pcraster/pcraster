#ifndef INCLUDED_GEO_SQUARETEST
#define INCLUDED_GEO_SQUARETEST



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
  // Square declarations.
}



namespace geo {



//! This class implements the unit tests for the Square class.
class SquareTest
{

public:

                   SquareTest          ();

  void             setUp               ();

  void             tearDown            ();

  void             testQuadSquareAt    ();
  void             testContains        ();
  void             testIntersects      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
