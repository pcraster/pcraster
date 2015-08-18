#ifndef INCLUDED_BLOCK_VOXELATHEIGHT
#define INCLUDED_BLOCK_VOXELATHEIGHT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.



namespace block {
  // VoxelAtHeight declarations.
}



namespace block {



//! Functor for finding a voxel at a certain height in a stack.
/*!
  With this functor you can find the voxel in a stack which is at a certain
  height.

  If height equals the bottom of a voxel than this functor returns true for
  that voxel and false for the voxel whose top equals height.
*/
struct VoxelAtHeight: public std::unary_function<REAL4, bool>
{

  friend class VoxelAtHeightTest;

private:

  //! Height for which we need to find a voxel.
  double d_height;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VoxelAtHeight       (REAL4 base,
                                        REAL4 height);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             operator()          (REAL4 thickness);

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     base Height of base of voxel stack.
  \param     height Height of voxel to look for.
  \warning   \a base must be smaller or equal to \a height.
*/
inline VoxelAtHeight::VoxelAtHeight(
         REAL4 base,
         REAL4 height)
  : d_height(height)
{
  DEVELOP_PRECOND(dal::smallerOrComparable(base, height));
  d_height -= base;
}

//! Returns whether the voxel with thickness \a thickness is at the requested height.
/*!
  \param     thickness Thickness of current voxel.
  \return    true or false

  This function should be called for a stack of voxels, starting with the
  lowest voxel. Once the requested height is smaller than the sum of
  thicknesses seen by this function this function returns true.
*/
inline bool VoxelAtHeight::operator()(
         REAL4 thickness)
{
  DEVELOP_PRECOND(dal::greaterOrComparable(thickness, REAL4(0.0)));
  d_height -= thickness;

  return d_height < 0.0;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace block

#endif
