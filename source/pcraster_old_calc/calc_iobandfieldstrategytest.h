#ifndef INCLUDED_CALC_IOBANDFIELDSTRATEGYTEST
#define INCLUDED_CALC_IOBANDFIELDSTRATEGYTEST



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
  // IoBandFieldStrategy declarations.
}



namespace calc {



//! This class implements the unit tests for the IoBandFieldStrategy class.
class IoBandFieldStrategyTest
{

public:

                   IoBandFieldStrategyTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCheckInputMap   ();
  void             testCheckClone      ();
  void             testCreateMap       ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
