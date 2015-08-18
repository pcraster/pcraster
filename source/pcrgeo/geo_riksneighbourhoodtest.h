#ifndef INCLUDED_GEO_RIKSNEIGHBOURHOODTEST
#define INCLUDED_GEO_RIKSNEIGHBOURHOODTEST



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
  // RiksNeighbourhood declarations.
}



namespace geo {



//! This class implements the unit tests for the RiksNeighbourhood class.
class RiksNeighbourhoodTest
{

public:

                   RiksNeighbourhoodTest();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
