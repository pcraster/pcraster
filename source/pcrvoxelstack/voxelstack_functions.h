#ifndef INCLUDED_VOXELSTACK_FUNCTIONS
#define INCLUDED_VOXELSTACK_FUNCTIONS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.


namespace discr {
  class VoxelStack;
}



namespace voxelstack {

size_t             nrVoxels            (discr::VoxelStack const& stack);

REAL4              thickness           (discr::VoxelStack const& stack,
                                        size_t voxel);

template<typename T>
T                  value               (std::vector<T> const& data,
                                        size_t voxel);

}

#endif
