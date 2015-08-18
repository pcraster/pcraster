#ifndef INCLUDED_DISCR_VOXELSTACK
#define INCLUDED_DISCR_VOXELSTACK



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.



namespace discr {
  // VoxelStack declarations.
}



namespace discr {



//! Class for discretisation information of 3D voxel stacks.
/*!
  A voxel stack is part of the Block discretisation and contains information
  about the voxels stacked at a certain cell in the block. A stack is defined
  by the elevation of the bottom of the lowest voxel (the base elevation) and
  the thicknesses of the voxels stacked on top of each other.

  The stack is implemented by inheriting from std::vector (not aparent from the
  doxygen docs) and layering the base elevation.

  Missing values are handled using the base elevation. A missing value
  for base elevation means a missing value for the stack. A non-missing value
  for base elevation means a non-missing value for the stack. An empty
  voxel stack with a valid value for base elevation is just an empty voxel
  stack and does not imply that the cell value is missing. A non-empty voxel
  stack and a missing value for base elevation does imply that the cell value
  is missing, the stack should be emptied immediately. This logic is
  encapsulated by the isMV() and setMV() member functions. Use these instead
  of testing baseElevation() or setting setBaseElevation(REAL4).

  \todo Make the float type of the thicknesses a template argument so that we
        can switch between REAL4 and REAL8 if needed.
*/
class VoxelStack: public std::vector<REAL4>
{

  friend class VoxelStackTest;

private:

  //! Elevation of the bottom of the lowest voxel (the base elevation).
  double           d_baseElevation;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VoxelStack          ();

                   VoxelStack          (REAL4 baseElevation);

                   VoxelStack          (const_iterator begin,
                                        const_iterator end);

  /* virtual */    ~VoxelStack         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setMV               ();

  void             setBaseElevation    (REAL4 elevation);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isMV                () const;

  bool             isRegular           () const;

  REAL4            baseElevation       () const;

  REAL4            thickness           () const;

  REAL4            surfaceElevation    () const;

  REAL4            bottomElevation     (size_t index) const;

  REAL4            topElevation        (size_t index) const;

  bool             equals              (VoxelStack const& stack) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

bool               operator==          (VoxelStack const& lhs,
                                        VoxelStack const& rhs);

bool               operator!=          (VoxelStack const& lhs,
                                        VoxelStack const& rhs);



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace discr

#endif
