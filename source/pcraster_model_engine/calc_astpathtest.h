#ifndef INCLUDED_CALC_ASTPATHTEST
#define INCLUDED_CALC_ASTPATHTEST



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
  // ASTPath declarations.
}



namespace calc {



//! This class implements the unit tests for the ASTPath class.
class ASTPathTest
{

private:

public:

                   ASTPathTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
