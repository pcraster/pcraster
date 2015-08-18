#ifndef INCLUDED_CALC_VFIELDTEST
#define INCLUDED_CALC_VFIELDTEST



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
  // VField declarations.
}



namespace calc {



//! This class implements the unit tests for the VField class.
class VFieldTest
{

private:

public:

                   VFieldTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();
  void             testUpdateMV        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
