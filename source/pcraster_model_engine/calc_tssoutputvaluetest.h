#ifndef INCLUDED_CALC_TSSOUTPUTVALUETEST
#define INCLUDED_CALC_TSSOUTPUTVALUETEST



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
  // TssOutputValue declarations.
}



namespace calc {



//! This class implements the unit tests for the TssOutputValue class.
class TssOutputValueTest
{

public:

                   TssOutputValueTest  ();

  void             setUp               ();

  void             tearDown            ();

  void             testCppStream       ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
