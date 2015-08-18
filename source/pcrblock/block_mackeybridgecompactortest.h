#ifndef INCLUDED_BLOCK_MACKEYBRIDGECOMPACTORTEST
#define INCLUDED_BLOCK_MACKEYBRIDGECOMPACTORTEST



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

namespace block {
  // MackeyBridgeCompactorTest declarations.
}



namespace block {

//! This class implements the unit tests for the MackeyBridgeCompactor class.
class MackeyBridgeCompactorTest
{

private:

public:

                   MackeyBridgeCompactorTest();

  void             setUp               ();

  void             tearDown            ();

  void             testDummyCompactor  ();

  void             testSandCompactor   ();

  void             testClayCompactor   ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
