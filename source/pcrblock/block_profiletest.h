#ifndef INCLUDED_BLOCK_PROFILETEST
#define INCLUDED_BLOCK_PROFILETEST



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
  // Profile declarations.
}



namespace block {



//! This class implements the unit tests for the Profile class.
class ProfileTest
{

private:

public:

                   ProfileTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
