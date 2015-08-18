#ifndef INCLUDED_COM_MATHTEST
#define INCLUDED_COM_MATHTEST



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

namespace com {
  // Math declarations.
}



namespace com {



//! This class implements the unit tests for the Math class.
class MathTest
{

public:

                   MathTest            ();

  void             setUp               ();

  void             tearDown            ();

  void             testEqualEpsilon    ();
  void             testLim             ();
  void             testIsInteger       ();
  void             testInterpolate2    ();
  void             testMinimizeMaximize();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
