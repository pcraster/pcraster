#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif

#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif

// PCRaster library headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
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
#ifndef INCLUDED_BLOCK_COMPACTORS
#include "block_compactors.h"
#define INCLUDED_BLOCK_COMPACTORS
#endif

#ifndef INCLUDED_BLOCK_DEHAANCOMPACTOR
#include "block_dehaancompactor.h"
#define INCLUDED_BLOCK_DEHAANCOMPACTOR
#endif

#ifndef INCLUDED_BLOCK_TYPES
#include "block_types.h"
#define INCLUDED_BLOCK_TYPES
#endif



/*!
  \file
  This file contains the implementation of the Add class.
*/



namespace block {

void noCompactionAdd(
         discr::Block& block,
         size_t nrVoxels,
         REAL4 thickness)
{
  block.addVoxels(nrVoxels, thickness);
}



void noCompactionAdd(
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
      DEVELOP_PRECOND(dal::greaterOrComparable(thickness.cell(i), REAL4(0.0)));
      if(thickness.cell(i) > REAL4(0.0)) {
        block.addVoxels(i, 1, thickness.cell(i));
      }
    }
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Document each argument. Handle compaction. Compaction function
             should be an argument for efficiency.
  \todo      Check implementation with Derek.

  This function is called by a function which handles the argument checking.
  We can assume here that all arguments have valid values.
*/
static void mackeyBridgeAdd(
         discr::Block& block,
         size_t index,
         std::vector<REAL4>& originalThickness,
         REAL4 thickness,
         REAL4 maxVoxelThickness,
         std::vector<INT4>& sediment,
         INT4 sedimentToAdd,
         Compactors<MackeyBridgeCompactor> const& compactors)
{
  double depth;
  double voxelThickness;

  discr::VoxelStack& stack(block.cell(index));

  if(!stack.empty()) {

    // --------------------------------
    // PROCESS EXISTING STACK OF VOXELS
    // --------------------------------
    // Compact existing sediment, from top to bottom. Don't change the
    // voxel at the top.
    depth = stack[stack.size() - 1];

    for(int i = stack.size() - 2; i >= 0; --i) {

      // The depth of burial of the voxel is the thickness of
      // the voxels on top of the current voxel (which are already
      // compacted in this loop).
      // Compact the voxel.
      DEVELOP_PRECOND(!pcr::isMV(originalThickness[i]));
      DEVELOP_PRECOND(originalThickness[i] > 0.0);
      // stack[i] = compactors.compact(sedimentToAdd, originalThickness[i],
      //        depth + thickness);
      stack[i] = compactors.compactor(sedimentToAdd)(originalThickness[i],
             depth + thickness);

      // Calculate depth of next voxel.
      depth += stack[i];
    }

    DEVELOP_PRECOND(!stack.empty());
    size_t i = stack.size() - 1;   // Index of top voxel.
    DEVELOP_PRECOND(!pcr::isMV(originalThickness[i]));
    DEVELOP_PRECOND(originalThickness[i] > 0.0);

    if(sediment[i] == sedimentToAdd &&
             dal::comparable(originalThickness[i], stack[i]) &&
             maxVoxelThickness > originalThickness[i]) {

      // The top voxel:
      //   - has the same sediment type, and
      //   - is not compacted, and
      //   - has room for new material

      // Uncompacted thickness to add, including stuff already in top voxel.
      thickness += originalThickness[i];

      if(maxVoxelThickness >= thickness) {

        // The current top voxel stays the top voxel. We've just enough
        // new material to add some to the top voxel and that's it.
        originalThickness[i] = thickness;
        stack[i] = originalThickness[i];
        thickness -= thickness; // Should be 0.0 by now.
      }
      else {

        // The current top voxel will be buried with other material.
        // We fill it untill its maximum original thickness is reached
        // and compact it.
        DEVELOP_PRECOND(thickness > maxVoxelThickness);
        originalThickness[i] = maxVoxelThickness;
        thickness -= maxVoxelThickness;   // Should be > 0.0 here.
        // stack[i] = compactors.compact(sediment[i], originalThickness[i],
        //       thickness);
        stack[i] = compactors.compactor(sediment[i])(originalThickness[i],
              thickness);
        DEVELOP_PRECOND(thickness > REAL4(0.0));
      }
    }
    else {

      // The top voxel:
      //   - has a different sediment type, or
      //   - is already compacted, or
      //   - has no room for new material
      // We can treat the top voxel as all lower voxels: compact.
      // stack[i] = compactors.compact(sediment[i], stack[i], thickness);
      stack[i] = compactors.compactor(sediment[i])(stack[i], thickness);
    }
  }

  // Check if there's accomodation space left to fill.
  if(thickness > REAL4(0.0)) {

    // ------------------
    // PROCESS NEW VOXELS
    // ------------------
    // Add and compact new voxels from bottom to top. Continue untill
    // the available space left is less than the height of one
    // uncompacted voxel.
    voxelThickness = maxVoxelThickness;
    size_t i = stack.size();

    // Add whole voxels, with thickness maxVoxelThickness.
    for(; dal::greaterOrComparable(thickness, maxVoxelThickness);
         thickness -= voxelThickness, ++i) {

      voxelThickness = compactors.compactor(sedimentToAdd)(maxVoxelThickness,
         thickness - maxVoxelThickness);

      // Add a new voxel with thickness voxelThickness.
      block.addVoxel(index, voxelThickness);
      DEVELOP_PRECOND(i == stack.size() - 1);
      sediment[i] = sedimentToAdd;
      originalThickness[i] = maxVoxelThickness;
    }

    // Fails
    // DEVELOP_PRECOND(dal::greaterOrComparable(thickness, REAL4(0.0)));

    // Now we have filled up thickness except for a small amount of
    // space left which is less than maxVoxelThickness. Here we add a
    // uncompacted voxel to fill up the last part of thickness.
    if(thickness > REAL4(0.0)) {

      // Add a new voxel with thickness thickness.
      DEVELOP_PRECOND(sediment.size() == stack.size());
      DEVELOP_PRECOND(originalThickness.size() == stack.size());
      block.addVoxel(index, thickness);
      DEVELOP_PRECOND(i == stack.size() - 1);
      DEVELOP_POSTCOND(sediment.size() == stack.size());
      DEVELOP_POSTCOND(originalThickness.size() == stack.size());
      sediment[i] = sedimentToAdd;
      originalThickness[i] = thickness;
    }
  }
}



//! Adds sediment to an existing block with sediments, thereby changing the spatial extent of the block and all its data subjects.
/*!
  \param     block Block discretisation to add to. This will be changed.
  \param     originalThickness Original thicknesses (before compaction)
             of the voxels in the block. This will be changed.
  \param     sediment Block data with the current values. This will be changed.
  \param     thickness Amount of sediment to add to the block data.
  \param     maxVoxelThickness Maximum thickness of newly created voxels.
  \return    .
  \exception .
  \warning   .
  \sa        .

  Not an easy function to use, requires some bookkeeping by the user.
  This is also not a general function, it is limited to the domain of
  sedimentation and refers directly to sediment and compaction.

  New voxels created in the nominal block data will have the default
  value set for this argument. This is a common rule in this library:
  when the dicretisation is extended, all data objects are extended
  too. Newly created voxels will be set to the default value set for
  each data object.

  add(block, originalThickness, sediment, thickness, maxVoxelThickness);
*/
void mackeyBridgeAdd(discr::Block& block,
         discr::BlockData<REAL4>& originalThickness,
         discr::BlockData<INT4>& sediment,
         discr::RasterData<REAL4>const & thickness,
         REAL4 maxVoxelThickness,
         Compactors<MackeyBridgeCompactor> const& compactors)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(originalThickness);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(sediment);

  DEVELOP_PRECOND(
         originalThickness.block() == &block ||
         *originalThickness.block() == block);
  DEVELOP_PRECOND(
         sediment.block() == &block ||
         *sediment.block() == block);
  DEVELOP_PRECOND(
         thickness.raster() == &static_cast<discr::Raster const&>(block) ||
         *thickness.raster() == static_cast<discr::Raster const&>(block));
  PRECOND(!pcr::isMV(maxVoxelThickness));
  PRECOND(maxVoxelThickness > 0.0);

  for(size_t i = 0; i < block.nrCells(); ++i) {
    if(!block.cell(i).isMV()) {
      if(!pcr::isMV(sediment.defaultValue().cell(i)) && (!thickness.isMV(i))) {
        PRECOND(dal::greaterOrComparable(thickness.cell(i), REAL4(0.0)));
        mackeyBridgeAdd(block, i,
           originalThickness.cell(i), thickness.cell(i), maxVoxelThickness,
           sediment.cell(i), sediment.defaultValue().cell(i), compactors);
      }
    }
  }

  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(originalThickness);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(sediment);
}



static void deHaanAdd(
         discr::Block& block,
         size_t index,
         std::vector<INT4>& sediment,
         INT4 sedimentToAdd,
         std::vector<REAL4>& initialThickness,
         std::vector<REAL4>& cummulativeLoad,
         std::vector<REAL4>& cummulativeDuration,
         REAL4 duration,
         REAL4 thickness,
         Compactors<DeHaanCompactor> const& compactors)
{
  DEVELOP_PRECOND(!(thickness < 0.0));

  if(dal::comparable(thickness, REAL4(0.0))) {
    return;
  }

  discr::VoxelStack& stack(block.cell(index));

  DeHaanCompactor const& compactor(compactors.compactor(sedimentToAdd));

  // -----------------
  // PROCESS NEW VOXEL
  // -----------------

  // Add new load to current stack of loads.
  REAL4 load = 0.5 * thickness * compactor.buoyancy();
  std::transform(cummulativeLoad.begin(), cummulativeLoad.end(),
         cummulativeLoad.begin(),
         boost::bind(std::plus<REAL4>(), _1, load));
  std::transform(cummulativeDuration.begin(), cummulativeDuration.end(),
         cummulativeDuration.begin(),
         boost::bind(std::plus<REAL4>(), _1, duration));

  // Add voxel to the discretisation which will signal the attributes.
  block.addVoxel(index, compactor(thickness, load, duration));

  // Set attribute values for newly create voxels.
  initialThickness.back() = thickness;
  cummulativeLoad.back() = load;
  DEVELOP_PRECOND(dal::comparable(cummulativeDuration.back(), duration));
  DEVELOP_PRECOND(sediment.back() == sedimentToAdd);

  if(stack.size() > 1) {
    // --------------------------------
    // PROCESS EXISTING STACK OF VOXELS
    // --------------------------------
    // Compact existing sediment.
    for(size_t i = 0; i < stack.size() - 1; ++i) {
      stack[i] = compactor(initialThickness[i], cummulativeLoad[i],
         cummulativeDuration[i]);
    }
  }
}



void deHaanAdd(
         discr::Block& block,
         discr::BlockData<INT4>& sediment,
         discr::BlockData<REAL4>& initialThickness,
         discr::BlockData<REAL4>& cummulativeLoad,
         discr::BlockData<REAL4>& cummulativeDuration,
         discr::RasterData<REAL4> const& thickness,
         Compactors<DeHaanCompactor> const& compactors)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(sediment);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(initialThickness);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(cummulativeLoad);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(cummulativeDuration);

  for(size_t i = 0; i < block.nrCells(); ++i) {
    if(!block.cell(i).isMV()) {
      if(!pcr::isMV(sediment.defaultValue().cell(i)) && !thickness.isMV(i)) {
        PRECOND(dal::greaterOrComparable(thickness.cell(i), REAL4(0.0)));
        deHaanAdd(block, i, sediment.cell(i), sediment.defaultValue().cell(i),
              initialThickness.cell(i), cummulativeLoad.cell(i),
              cummulativeDuration.cell(i),
              cummulativeDuration.defaultValue().cell(i),
              thickness.cell(i), compactors);
      }
    }
  }

  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(sediment);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(initialThickness);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(cummulativeLoad);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(cummulativeDuration);
}

} // namespace block

