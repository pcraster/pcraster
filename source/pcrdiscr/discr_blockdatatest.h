#ifndef INCLUDED_DISCR_BLOCKDATATEST
#define INCLUDED_DISCR_BLOCKDATATEST



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

namespace discr {
  // BlockData declarations.
}



namespace discr {



//! This class implements the unit tests for the BlockData class.
class BlockDataTest
{

private:

public:

                   BlockDataTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testSetDefaultValue ();

  void             testAddVoxels       ();

  void             testRemoveVoxels    ();

  void             testCutVoxels       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace discr

#endif
