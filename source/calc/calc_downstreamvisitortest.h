#ifndef INCLUDED_CALC_DOWNSTREAMVISITORTEST
#define INCLUDED_CALC_DOWNSTREAMVISITORTEST



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
  // DownStreamVisitor declarations.
}



namespace calc {



//! This class implements the unit tests for the DownStreamVisitor class.
class DownStreamVisitorTest
{

public:

                   DownStreamVisitorTest           ();

  void             testPitOnly         ();

  void             testAllDirs         ();

  void             testKnownOrder      ();

  void             testRealisticLdd    ();

  static boost::unit_test::test_suite *    suite               ();

};

} // namespace calc

#endif
