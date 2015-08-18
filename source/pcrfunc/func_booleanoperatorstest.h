#ifndef INCLUDED_FUNC_BOOLEANOPERATORSTEST
#define INCLUDED_FUNC_BOOLEANOPERATORSTEST



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

namespace func {
  // BooleanOperatorsTest declarations.
}



namespace func {

//! This class implements the unit tests for the BooleanOperators class.
class BooleanOperatorsTest
{

private:

public:

                   BooleanOperatorsTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace func

#endif
