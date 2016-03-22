#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_VOXELSTACK
#include "geo_voxelstack.h"
#define INCLUDED_VOXELSTACK
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------

//       1         2         3         4         5         6         7         8

namespace geo {

struct addDepth: public std::unary_function<const Voxel &, void>
{
  REAL8 depth;

  addDepth(): depth(0.0)
  {}

  void operator()(const Voxel &v)
  { depth += v.thickness(); }
};

} // namespace geo



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

geo::VoxelStack::VoxelStack()

  : d_bottom(0.0)

{
}



geo::VoxelStack::VoxelStack(const VoxelStack& /* vs */)
{
  // geo::Raster needs this, but we haven't implemented it yet.
  PRECOND(false);
}



geo::VoxelStack::~VoxelStack()
{
  clean();
}



void geo::VoxelStack::clean()
{
}



geo::VoxelStack& geo::VoxelStack::operator=(const VoxelStack& vs )
{
  PRECOND(false);
  (void)vs;  // Shut up compiler
  // MSC want a return value, can we live with out the = implementation?
  static VoxelStack dummy;
  return dummy;
}



/*!
  \param     it Iterator to voxel whose depth must be calculated.
  \return    Depth of voxel pointed to by \a it.
  \warning   This function is expensive! If you're looping over the stack to
             do something for each voxel in the stack, you're better of
             calculating the depth yourself:
  \code
    // Compact sediment, from top to bottom. Doesn't change the voxel at the
    // top.
    for(rit = stack.rbegin() + 1, depth = (*rit).thickness();
        rit != stack.rend(); ++rit)
    {
      // The depth of burial of the voxel is the thickness of the voxels on
      // top of the current voxel (which are already compacted in this loop).
      // Compact the voxel.
      (*rit).setThickness(compact(*rit, depth));

      // Calculate depth of next voxel.
      depth += (*rit).thickness();
    }
  \endcode

  The depth of a voxel is equal to the thickness of the voxels on top of it.
*/
REAL8 geo::VoxelStack::depth(const_iterator it) const
{
  REAL8 d = 0.0;

  if(it != d_column.end())
  {
    addDepth a = std::for_each(++it, end(), addDepth());
    d = a.depth;
  }

  return d;
}




//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


