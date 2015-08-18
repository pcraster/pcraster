#ifndef INCLUDED_GEO_CIRCULARNEIGHBOURHOODTEST
#define INCLUDED_GEO_CIRCULARNEIGHBOURHOODTEST



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
  // CircularNeighbourhood declarations.
}



namespace geo {



//! This class implements the unit tests for the CircularNeighbourhood class.
class CircularNeighbourhoodTest
{

public:

                   CircularNeighbourhoodTest();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
