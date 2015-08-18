#ifndef INCLUDED_BLOCK_ARITHMETICOPERATORSTEST
#define INCLUDED_BLOCK_ARITHMETICOPERATORSTEST



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

namespace block {
  // ArithmeticOperators declarations.
}



namespace block {



//! This class implements the unit tests for the ArithmeticOperators class.
class ArithmeticOperatorsTest
{

private:

public:

                   ArithmeticOperatorsTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
