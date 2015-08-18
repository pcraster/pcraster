#ifndef INCLUDED_BLOCK_IOTEST
#define INCLUDED_BLOCK_IOTEST



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
  // IO declarations.
}



namespace block {



//! This class implements the unit tests for the IO class.
class IOTest
{

private:

public:

                   IOTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
