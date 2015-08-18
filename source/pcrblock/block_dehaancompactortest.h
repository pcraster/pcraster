#ifndef INCLUDED_BLOCK_DEHAANCOMPACTORTEST
#define INCLUDED_BLOCK_DEHAANCOMPACTORTEST



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
  // DeHaanCompactorTest declarations.
}



namespace block {

//! This class implements the unit tests for the DeHaanCompactor class.
class DeHaanCompactorTest
{

private:

public:

                   DeHaanCompactorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
