#ifndef INCLUDED_GEO_NEIGHBOURTEST
#define INCLUDED_GEO_NEIGHBOURTEST



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
  // Neighbour declarations.
}



namespace geo {



//! This class implements the unit tests for the Neighbour class.
class NeighbourTest
{

public:

                   NeighbourTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testLinearDownStream();

  void             testConversion      ();

  void             testNBCode          ();

  void             testDownStreamVisitorCell ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
