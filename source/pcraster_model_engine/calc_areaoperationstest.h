#ifndef INCLUDED_CALC_AREAOPERATIONSTEST
#define INCLUDED_CALC_AREAOPERATIONSTEST



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
  // AreaOperations declarations.
}



namespace calc {



//! This class implements the unit tests for the AreaOperations class.
class AreaOperationsTest
{

private:

public:

                   AreaOperationsTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();
  void             testAreaOrder       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
