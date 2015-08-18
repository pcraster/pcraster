#ifndef INCLUDED_MLDD_DAGRASTERTEST
#define INCLUDED_MLDD_DAGRASTERTEST



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


namespace mldd {
  // DagRaster declarations.
  class DagRaster;
}



namespace mldd {



//! This class implements the unit tests for the DagRaster class.
class DagRasterTest
{
    DagRaster *d_case1;

public:

                   DagRasterTest           ();
                  ~DagRasterTest           ();

  void             testUpdateOrder     ();

  void             testDownstreamVisit ();

  void             testCycle           ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace mldd

#endif
