#ifndef INCLUDED_CALC_IOBANDTEST
#define INCLUDED_CALC_IOBANDTEST



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
  // IoBand declarations.
}



namespace calc {



/*! This class implements some tests for the WL Habitat project at
     the dll-client level
 */
class IoBandTest
{

public:

                   IoBandTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test1               ();

  void             test2               ();

  void             test3               ();

  void             testTypeChecking    ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
