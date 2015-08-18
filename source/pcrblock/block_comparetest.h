#ifndef INCLUDED_BLOCK_COMPARETEST
#define INCLUDED_BLOCK_COMPARETEST



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
  // Compare declarations.
}



namespace block {



//! This class implements the unit tests for the Compare class.
class CompareTest
{

private:

public:

                   CompareTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
