#ifndef INCLUDED_BLOCK_VOXELATHEIGHTTEST
#define INCLUDED_BLOCK_VOXELATHEIGHTTEST



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
  // VoxelAtHeight declarations.
}



namespace block {



//! This class implements the unit tests for the VoxelAtHeight class.
class VoxelAtHeightTest
{

private:

public:

                   VoxelAtHeightTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
