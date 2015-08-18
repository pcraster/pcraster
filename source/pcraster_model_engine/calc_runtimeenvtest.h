#ifndef INCLUDED_CALC_RUNTIMEENVTEST
#define INCLUDED_CALC_RUNTIMEENVTEST



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
  // RunTimeEnv declarations.
}



namespace calc {



//! This class implements the unit tests for the RunTimeEnv class.
class RunTimeEnvTest
{

public:

                   RunTimeEnvTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testPopField        ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
