#ifndef INCLUDED_GEOM_PRDIMTREETEST
#define INCLUDED_GEOM_PRDIMTREETEST



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
  // PRDimTree declarations.
}



namespace geom {



//! This class implements the unit tests for the PRDimTree class.
class PRDimTreeTest
{

public:

                   PRDimTreeTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testCompile         ();

  void             testKeyElement      ();

  void             testProximitySearch ();

  void             testElementIsKeyPtr ();

  void             testRLEPtr ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geom

#endif
