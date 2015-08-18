#ifndef INCLUDED_CALC_LOOKUPLINEARTEST
#define INCLUDED_CALC_LOOKUPLINEARTEST



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
  // LookupLinear declarations.
}



namespace calc {



//! This class implements the unit tests for the LookupLinear class.
class LookupLinearTest
{

public:

                   LookupLinearTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testOldStyleCtor    ();
  void             testAllIntervals    ();
  void             testMultipleRecords ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
