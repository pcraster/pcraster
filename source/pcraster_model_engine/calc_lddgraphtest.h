#ifndef INCLUDED_CALC_LDDGRAPHTEST
#define INCLUDED_CALC_LDDGRAPHTEST



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

namespace calc {
  // LddGraph declarations.
}



namespace calc {



//! This class implements the unit tests for the LddGraph class.
class LddGraphTest
{

private:

public:

                   LddGraphTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testSimple          ();
  void             testMVCtor          ();
  void             testUpstream        ();
  void             testDownstreamIterator();
  void             testFieldIdToPitId  ();
  void             testDownstream      ();
  void             testInitField       ();
  void             testUnsound         ();
  void             testVisitor         ();
  void             testTimeSliceVisitor();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
