#ifndef INCLUDED_DISCR_VOXELSTACKTEST
#define INCLUDED_DISCR_VOXELSTACKTEST



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
  // VoxelStack declarations.
}



namespace discr {



//! This class implements the unit tests for the VoxelStack class.
class VoxelStackTest
{

private:

public:

                   VoxelStackTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testSetMV           ();

  void             testSetBaseElevation();

  void             testThickness       ();

  void             testIsRegular       ();

  void             testSurfaceElevation();

  void             testBottomElevation ();

  void             testTopElevation    ();

  void             testEquals          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace discr

#endif
