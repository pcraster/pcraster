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

#ifndef INCLUDED_DISCR_VOXELSTACK
#include "discr_voxelstack.h"
#define INCLUDED_DISCR_VOXELSTACK
#endif

// Module headers.



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

