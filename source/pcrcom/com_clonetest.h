#ifndef INCLUDED_COM_CLONETEST
#define INCLUDED_COM_CLONETEST



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
  // Clone declarations.
}



namespace com {



//! This class implements the unit tests for the Clone class.
class CloneTest
{

public:

                   CloneTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testResetClone      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
