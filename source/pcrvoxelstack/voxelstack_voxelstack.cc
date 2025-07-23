#include "stddefx.h"
#include "pcrtypes.h"
#include "discr_voxelstack.h"



/*!
  \file
  This file contains the implementation of the VoxelStack class.
*/



namespace voxelstack {

size_t nrVoxels(
         discr::VoxelStack const& stack)
{
  return stack.size();
}



REAL4 thickness(
         discr::VoxelStack const& stack,
         size_t voxel)
{
  DEVELOP_PRECOND(voxel >= 1 && voxel <= stack.size());

  --voxel;

  return stack[voxel];
}



template<typename T>
T value(
         std::vector<T> const& data,
         size_t voxel)
{
  DEVELOP_PRECOND(voxel >= 1 && voxel <= data.size());

  --voxel;

  return data[voxel];
}

template
UINT1 value(
         std::vector<UINT1> const&,
         size_t);
template
INT4 value(
         std::vector<INT4> const&,
         size_t);
template
REAL4 value(
         std::vector<REAL4> const&,
         size_t);

} // namespace voxelstack

