#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_DISCR_VOXELSTACK
#include "discr_voxelstack.h"
#define INCLUDED_DISCR_VOXELSTACK
#endif

// Library headers.
#ifndef INCLUDED_NUMERIC
#include <numeric>
#define INCLUDED_NUMERIC
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif



/*!
  \file
  This file contains the implementation of the VoxelStack class.
*/



namespace discr {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VOXELSTACK MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VOXELSTACK MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Default base elevation is 0.0.
*/
VoxelStack::VoxelStack()

  : d_baseElevation(0.0)

{
}



//! Constructor.
/*!
  \param     baseElevation Base elevation of stack.
  \warning   \a baseElevation must not be a missing value.
*/
VoxelStack::VoxelStack(
         REAL4 baseElevation)

  : d_baseElevation(baseElevation)

{
  DEVELOP_PRECOND(!pcr::isMV(baseElevation));
}



//! Constructor.
/*!
  \param     begin Iterator to first thickness.
  \param     end Iterator to one-past-the-last thickness.
  \warning   None of the input thicknesses must be a missing value.

  Default base elevation is 0.0.

  This constructor is needed for the Python extension library (indexing suite).
*/
VoxelStack::VoxelStack(
         const_iterator begin,
         const_iterator end)

  : std::vector<REAL4>(begin, end),
    d_baseElevation(0.0)

{
}



//! Destructor.
/*!
*/
VoxelStack::~VoxelStack()
{
}



//! Sets the base elevation to \a elevation.
/*!
  \param     elevation New base elevation.
  \warning   The stack must not be a missing value. \a elevation must not be a
             missing value.
*/
void VoxelStack::setBaseElevation(
         REAL4 elevation)
{
  DEVELOP_PRECOND(!isMV());
  DEVELOP_PRECOND(!pcr::isMV(elevation));

  d_baseElevation = elevation;
}



//! Returns whether every voxel in the stack has the same thickness.
/*!
  \return    true of false
  \warning   The stack must not be a missing value.

  This function returns true if the stack is empty.
*/
bool VoxelStack::isRegular() const
{
  DEVELOP_PRECOND(!isMV());

  bool result = true;

  if(!empty()) {
    DEVELOP_PRECOND(!pcr::isMV(operator[](0)));
    REAL4 thickness = operator[](0);

    for(size_t i = 1; i < size(); ++i) {
      DEVELOP_PRECOND(!pcr::isMV(operator[](i)));
      if(!dal::comparable(thickness, operator[](i))) {
        // Different thicknesses.
        result = false;
        break;
      }
    }
  }

  return result;
}



//! Returns the base elevation.
/*!
  \return    elevation
  \warning   The stack must not be a missing value.
*/
REAL4 VoxelStack::baseElevation() const
{
  DEVELOP_PRECOND(!isMV());

  return (REAL4)d_baseElevation;
}



//! Returns the thickness of the stack.
/*!
  \return    thickness
  \warning   The stack must not be a missing value.
*/
REAL4 VoxelStack::thickness() const
{
  DEVELOP_PRECOND(!isMV());

  double result = 0.0;

  for(size_t i = 0; i < size(); ++i) {
    result += operator[](i);
  }

  return static_cast<REAL4>(result);
}



//! Returns the elevation of the top of the top voxel.
/*!
  \return    elevation
  \warning   The stack must not be a missing value.
*/
REAL4 VoxelStack::surfaceElevation() const
{
  DEVELOP_PRECOND(!isMV());

  return baseElevation() + thickness();
}



//! Returns the elevation of the bottom of voxel \a index.
/*!
  \return    elevation
  \warning   The stack must not be a missing value.
*/
REAL4 VoxelStack::bottomElevation(
         size_t index) const
{
  DEVELOP_PRECOND(!isMV());
  DEVELOP_PRECOND(index < size());

  double result(d_baseElevation);

  for(size_t i = 0; i < index; ++i) {
    result += operator[](i);
  }

  return static_cast<REAL4>(result);
}



//! Returns the elevation of the top of voxel \a index.
/*!
  \return    elevation
  \warning   The stack must not be a missing value.
*/
REAL4 VoxelStack::topElevation(
         size_t index) const
{
  DEVELOP_PRECOND(!isMV());
  DEVELOP_PRECOND(index < size());

  double result(d_baseElevation);

  for(size_t i = 0; i <= index; ++i) {
    result += operator[](i);
  }

  return static_cast<REAL4>(result);
}



//! Sets the stack to missing value.
/*!
  \warning   The stack must already be empty.
*/
void VoxelStack::setMV()
{
  DEVELOP_PRECOND(empty());

  pcr::setMV(d_baseElevation);
}



//! Returns whether the stack is a missing value.
/*!
  \return    true or false
*/
bool VoxelStack::isMV() const
{
  return pcr::isMV(d_baseElevation);
}



//! Returns whether *this equals \a stack.
/*!
  \param     stack Stack to compare.
  \return    true or false
*/
bool VoxelStack::equals(
         VoxelStack const& stack) const
{
  return dal::comparable(d_baseElevation, stack.d_baseElevation) &&
         size() == stack.size() &&
         dal::comparable(begin(), end(), stack.begin());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

bool operator==(
         VoxelStack const& lhs,
         VoxelStack const& rhs)
{
  return lhs.equals(rhs);
}



bool operator!=(
         VoxelStack const& lhs,
         VoxelStack const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace discr

