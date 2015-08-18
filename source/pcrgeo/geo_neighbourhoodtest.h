#ifndef INCLUDED_GEO_NEIGHBOURHOODTEST
#define INCLUDED_GEO_NEIGHBOURHOODTEST



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
  // Neighbourhood declarations.
}



namespace geo {



//! This class implements the unit tests for the Neighbourhood class.
class NeighbourhoodTest
{

private:

public:

                   NeighbourhoodTest   ();

  void             setUp               ();

  void             tearDown            ();

  void             testRandomCellLocations();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
