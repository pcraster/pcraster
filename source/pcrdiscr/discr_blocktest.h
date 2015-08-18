#ifndef INCLUDED_DISCR_BLOCKTEST
#define INCLUDED_DISCR_BLOCKTEST



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
  // Block declarations.
}



namespace discr {



//! This class implements the unit tests for the Block class.
class BlockTest
{

private:

public:

                   BlockTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testNrVoxels        ();

  void             testAddVoxels       ();

  void             testRemoveVoxels    ();

  void             testCutVoxels       ();

  void             testSetMV           ();

  void             testEquals          ();

  void             testIsEmpty         ();

  void             testIsRegular       ();

  void             testBottomELevation ();

  void             testTopElevation    ();

  void             testExtremeElevations();

  static boost::unit_test::test_suite* suite();

};

} // namespace discr

#endif
