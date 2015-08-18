#ifndef INCLUDED_CALC_LDDDOWNSTREAMTEST
#define INCLUDED_CALC_LDDDOWNSTREAMTEST



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
  // LddDownstream declarations.
}



namespace calc {



//! This class implements the unit tests for the LddDownstream class.
class LddDownstreamTest
{

private:

public:

                   LddDownstreamTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testSimple          ();
  void             testUpstream        ();
  void             testDownstream      ();
  void             testDiagonal        ();
  void             testUnsound         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
