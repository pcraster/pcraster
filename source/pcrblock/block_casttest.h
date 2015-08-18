#ifndef INCLUDED_BLOCK_CASTTEST
#define INCLUDED_BLOCK_CASTTEST



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
  // Cast declarations.
}



namespace block {



//! This class implements the unit tests for the Cast class.
class CastTest
{

private:

public:

                   CastTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
