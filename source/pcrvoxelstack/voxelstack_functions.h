#ifndef INCLUDED_VOXELSTACK_FUNCTIONS
#define INCLUDED_VOXELSTACK_FUNCTIONS

#include "stddefx.h"



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
