#ifndef INCLUDED_BLOCK_COMPACTORSTEST
#define INCLUDED_BLOCK_COMPACTORSTEST



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
  // Compactors declarations.
}



namespace block {



//! This class implements the unit tests for the Compactors class.
class CompactorsTest
{

private:

public:

                   CompactorsTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
