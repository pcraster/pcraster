#ifndef INCLUDED_BLOCK_REMOVETEST
#define INCLUDED_BLOCK_REMOVETEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace discr {
  class Block;
  template<typename T>
    class BlockData;
  class Raster;
}
namespace block {
  // Remove declarations.
}



namespace block {



//! This class implements the unit tests for the Remove class.
class RemoveTest
{

private:

  discr::Raster* d_raster;

  discr::Block* d_block;

  discr::BlockData<REAL4>* d_originalThickness;

  discr::BlockData<INT4>* d_sediment;

  REAL4 d_maxVoxelThickness;

public:

                   RemoveTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testRemove          ();

  void             testRemoveMoreThanAvailable();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
