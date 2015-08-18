#ifndef INCLUDED_CALC_LEXINPUTTEST
#define INCLUDED_CALC_LEXINPUTTEST



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
  // LexInput declarations.
}



namespace calc {



//! This class implements the unit tests for the LexInput class.
class LexInputTest
{

public:

                   LexInputTest           ();

  void             setUp               ();

  void             tearDown            ();

  void            testInstallStringScript ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
