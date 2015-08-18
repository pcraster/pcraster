#ifndef INCLUDED_CALC_SCRIPTTEST
#define INCLUDED_CALC_SCRIPTTEST



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
  // Script declarations.
}



namespace calc {



//! This class implements the unit tests for the Script class.
class ScriptTest
{

private:

public:

                   ScriptTest           ();

  void             setUp               ();

  void             tearDown            ();

  static boost::unit_test::test_suite* suite();

  // void             testResolve         ();

};

} // namespace calc

#endif
