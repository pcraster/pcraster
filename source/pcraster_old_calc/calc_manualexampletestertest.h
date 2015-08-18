#ifndef INCLUDED_CALC_MANUALEXAMPLETESTERTEST
#define INCLUDED_CALC_MANUALEXAMPLETESTERTEST



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
  // ManualExampleTester declarations.
}



namespace calc {



//! This class implements the unit tests for the ManualExampleTester class.
class ManualExampleTesterTest
{

public:

                   ManualExampleTesterTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testTest            ();
  void             testOption          ();
  void             testClone           ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
