#ifndef INCLUDED_VOXELSTACK_VOXELSTACKTEST
#define INCLUDED_VOXELSTACK_VOXELSTACKTEST



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

namespace voxelstack {
  // VoxelStackTest declarations.
}



namespace voxelstack {

//! This class implements the unit tests for the VoxelStack class.
class VoxelStackTest
{

private:

public:

                   VoxelStackTest      ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace voxelstack

#endif
