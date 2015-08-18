#ifndef INCLUDED_CALC_ASTPARTEST
#define INCLUDED_CALC_ASTPARTEST



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
  // ASTPar declarations.
}



namespace calc {



//! This class implements the unit tests for the ASTPar class.
class ASTParTest
{

public:

                   ASTParTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCompare         ();

  void             testSet             ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
