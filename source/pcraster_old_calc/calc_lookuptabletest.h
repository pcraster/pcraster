#ifndef INCLUDED_CALC_LOOKUPTABLETEST
#define INCLUDED_CALC_LOOKUPTABLETEST



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
  // LookupTable declarations.
}



namespace calc {



//! This class implements the unit tests for the LookupTable class.
class LookupTableTest
{

public:

                   LookupTableTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testOldStyleCtor    ();
  void             testAllIntervals    ();
  void             testMultipleKeys    ();
  void             testMultipleRecords ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
