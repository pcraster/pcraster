#ifndef INCLUDED_BLOCK_ADDTEST
#define INCLUDED_BLOCK_ADDTEST



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
  // Add declarations.
}



namespace block {



//! This class implements the unit tests for the Add class.
class AddTest
{

private:

public:

                   AddTest             ();

  void             setUp               ();

  void             tearDown            ();

  void             testNoCompactionAdd ();

  void             testMackyBridgeAdd  ();

  void             testDeHaanAdd       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
