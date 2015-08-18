#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_RASTERDATA
#include "discr_rasterdata.h"
#define INCLUDED_DISCR_RASTERDATA
#endif

#ifndef INCLUDED_DISCR_VOXELSTACK
#include "discr_voxelstack.h"
#define INCLUDED_DISCR_VOXELSTACK
#endif

// Module headers.



namespace block {

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
static void remove(
         discr::Block& block,
         size_t index,
         REAL4 thickness)
{
  discr::VoxelStack& stack(block.cell(index));

  // While voxels fall within the thickness to remove, chop'm off.
  if(!stack.empty()) {
    size_t nr = 0;

    for(int i = static_cast<int>(stack.size() - 1);
         i >= 0 && dal::greaterOrComparable(thickness, stack[i]); --i) {
      thickness -= stack[i];
      ++nr;
    }

    block.removeVoxels(index, nr);
  }

  // Test if there is more to remove. If so, dig in.
  if(thickness > REAL4(0.0)) {

    if(stack.empty()) {

      // There're no voxels left but still stuff to erode. Here we lower
      // the base level with the remaining thicknes.
      stack.setBaseElevation(stack.baseElevation() - thickness);
    }
    else {

      // There is at least one voxel left to cut the remaining thickness of.
      REAL4 fractionToRemove = thickness / stack.back();
      block.cutVoxel(index, fractionToRemove);
    }
  }
}



void remove(
         discr::Block& block,
         discr::RasterData<REAL4> const& thickness)
{
  DEVELOP_PRECOND(
         thickness.raster() == &static_cast<discr::Raster const&>(block) ||
         *thickness.raster() == static_cast<discr::Raster const&>(block));

  for(size_t i = 0; i < block.nrCells(); ++i) {
    if(thickness.isMV(i)) {
      block.cell(i).setMV();
    }
    else if(!block.cell(i).isMV()) {
      PRECOND(dal::greaterOrComparable(thickness.cell(i), REAL4(0.0)));
      remove(block, i, thickness.cell(i));
    }
  }
}

} // namespace block

