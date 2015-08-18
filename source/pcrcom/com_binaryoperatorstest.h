#ifndef INCLUDED_COM_BINARYOPERATORSTEST
#define INCLUDED_COM_BINARYOPERATORSTEST



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
  // BinaryOperators declarations.
}



namespace com {



//! This class implements the unit tests for the BinaryOperators class.
class BinaryOperatorsTest
{

public:

                   BinaryOperatorsTest ();

  void             setUp               ();

  void             tearDown            ();

  void             testDivide          ();

  void             testMVCast          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
