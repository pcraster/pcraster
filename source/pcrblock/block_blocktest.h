#ifndef INCLUDED_BLOCK_BLOCKTEST
#define INCLUDED_BLOCK_BLOCKTEST



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
  // Block declarations.
}



namespace block {



//! This class implements the unit tests for the Block class.
class BlockTest
{

private:

public:

                   BlockTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCreate          ();

  void             testBaseElevation   ();

  void             testSurfaceElevation();

  void             testSetDefaultValue ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
