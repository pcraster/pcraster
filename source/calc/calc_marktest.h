#ifndef INCLUDED_CALC_MARKTEST
#define INCLUDED_CALC_MARKTEST



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

//! This class implements the unit tests for the Mark class.
class MarkTest
{
public:

                   MarkTest           ();

  void             testLe              ();
  void             testGe              ();

  static boost::unit_test::test_suite *    suite               ();

};

} // namespace calc

#endif
