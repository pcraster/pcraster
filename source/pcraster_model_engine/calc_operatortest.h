#ifndef INCLUDED_CALC_OPERATORTEST
#define INCLUDED_CALC_OPERATORTEST



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
  // Operator declarations.
}



namespace calc {



//! This class implements the unit tests for the Operator class.
class OperatorTest
{

public:

                   OperatorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testFirstFieldInput ();

  void             testActualInput     ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
