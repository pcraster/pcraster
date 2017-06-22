#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the Block class.
*/



namespace discr {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BLOCK MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BLOCK MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     nrRows Number of rows.
  \param     nrCols Number of columns.
  \param     cellSize Cell size.
  \param     west x-coordinate of left side of western most column.
  \param     north y-coordinate of top side of northern most row.
  \todo      Remove this constructor in favour of the one with the Raster arg?

  Creates a block discretisation object based on the raster properties passed
  in.

  The base elevation of all voxel stacks will be the default elevation (see
  VoxelStack class).
*/
Block::Block(
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north)

  : Raster(nrRows, nrCols, cellSize, west, north),
#if _MSC_VER
  #pragma warning(disable:4355)
#endif
    // Yes, in general not a good idea to pass the this pointer to a base
    // class since the object is not construted yet. But we only pass the
    // Raster base stuff to the RasterData base and Raster base IS constructed.
    RasterData<VoxelStack>(static_cast<Raster const*>(this))
#if _MSC_VER
  #pragma warning(default:4355)
#endif

{
}



//! Constructor.
/*!
  \param     raster Raster properties to copy.

  Creates a block discretisation object based on the raster properties passed
  in.

  The base elevation of all voxel stacks will be the default elevation (see
  VoxelStack class).
*/
Block::Block(
         Raster const& raster)

  : Raster(raster),
#if _MSC_VER
  #pragma warning(disable:4355)
#endif
    // See comments in first constructor.
    RasterData<VoxelStack>(static_cast<Raster const*>(this))
#if _MSC_VER
  #pragma warning(default:4355)
#endif

{
}



//! Constructor.
/*!
  \param     raster Raster properties to copy.
  \param     baseElevation Value to use as base elevation of all voxel stacks.

  Creates a block discretisation object based on the raster properties passed
  in.
*/
Block::Block(
         Raster const& raster,
         REAL4 baseElevation)

  : Raster(raster),
#if _MSC_VER
  #pragma warning(disable:4355)
#endif
    // See comments in first constructor.
    RasterData<VoxelStack>(static_cast<Raster const*>(this))
#if _MSC_VER
  #pragma warning(default:4355)
#endif

{
  if(pcr::isMV(baseElevation)) {
    for(size_t i = 0; i < nrCells(); ++i) {
      cell(i).setMV();
    }
  }
  else {
    for(size_t i = 0; i < nrCells(); ++i) {
      cell(i).setBaseElevation(baseElevation);
    }
  }
}



//! Constructor.
/*!
  \param     raster Raster properties to copy.
  \param     baseElevation Values to use as base elevation of all voxel stacks.

  Creates a block discretisation object based on the raster properties passed
  in.
*/
Block::Block(
         RasterData<REAL4> const& baseElevation)

  : Raster(*baseElevation.raster()),
#if _MSC_VER
  #pragma warning(disable:4355)
#endif
    // See comments in first constructor.
    RasterData<VoxelStack>(static_cast<Raster const*>(this))
#if _MSC_VER
  #pragma warning(default:4355)
#endif

{
  for(size_t i = 0; i < nrCells(); ++i) {
    if(pcr::isMV(baseElevation.cell(i))) {
      cell(i).setMV();
    }
    else {
      cell(i).setBaseElevation(baseElevation.cell(i));
    }
  }
}



/* NOT IMPLEMENTED
//! Copy constructor.
Block::Block(
         Block const& rhs)

  : Base(rhs)

{
  if this needs to be implemented check out the base class
  copy constructor is implemented using a memcopy which does not do the
  job for this class with objects at each cell
}
*/



//! Destructor.
/*!
*/
Block::~Block()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
Block& Block::operator=(
         Block const& rhs)
{
  if this needs to be implemented check out the base class
  copy constructor is implemented using a memcopy which does not do the
  job for this class with objects at each cell

  if(this != &rhs) {
  }

  return *this;
}
*/



//! Adds one voxel with thickness \a thickness to the top of voxel stack \a index.
/*!
  \param     index Index of cell to add voxel to.
  \param     thickness Thickness of new voxel.

  After the voxel has been added the addVoxelsSignal() is emitted.
*/
void Block::addVoxel(
         size_t index,
         REAL4 thickness)
{
  DEVELOP_PRECOND(!cell(index).isMV());
  cell(index).push_back(thickness);
  d_addVoxelsSignal(index, 1);
}



//! Adds \a nr voxels with thickness \a thickness to the top of voxel stack \a index.
/*!
  \param     index Index of cell to add voxels to.
  \param     nr Number of voxels to add.
  \param     thickness Thickness of new voxels.

  After the voxels have been added the addVoxelsSignal() is emitted (once).
*/
void Block::addVoxels(
         size_t index,
         size_t nr,
         REAL4 thickness)
{
  DEVELOP_PRECOND(!cell(index).isMV());
  cell(index).insert(cell(index).end(), nr, thickness);
  d_addVoxelsSignal(index, nr);
}



//! Adds \a nr voxels with thickness \a thickness to the top of all voxel stacks.
/*!
  \param     nr Number of voxels to add.
  \param     thickness Thickness of new voxels.

  The addVoxelsSignal() is emitted each time voxels have been added to a
  voxel stack.
*/
void Block::addVoxels(
         size_t nr,
         REAL4 thickness)
{
  for(size_t i = 0; i < nrCells(); ++i) {
    if(!cell(i).isMV()) {
      addVoxels(i, nr, thickness);
    }
  }
}



//! Removes \a nr voxels from the top of voxel stack \a index.
/*!
  \param     index Index of voxel stack to remove voxels from.
  \param     nr Number of voxels to remove.
  \warning   Make sure \a nr is not larger than the number of voxels in stack \a index.
*/
void Block::removeVoxels(
         size_t index,
         size_t nr)
{
  DEVELOP_PRECOND(!cell(index).isMV());
  DEVELOP_PRECOND(nr <= cell(index).size());

  cell(index).erase(cell(index).end() - nr, cell(index).end());
  d_removeVoxelsSignal(index, nr);
}



//! Removes a fraction \a fraction of the top voxel of voxel stack \a index.
/*!
  \param     index Index of voxel stack to cut.
  \param     fraction Fraction to remove.
  \warning   \a fraction must lie in [0.0, 1.0>.

  This function is meant to be used only on the top voxel. That why fraction
  should not be >= 1.0. Use the removeVoxel variants if whole voxels need to
  be removed.
*/
void Block::cutVoxel(
         size_t index,
         REAL4 fraction)
{
  DEVELOP_PRECOND(!cell(index).isMV());
  DEVELOP_PRECOND(dal::greaterOrComparable(fraction, REAL4(0.0)));
  DEVELOP_PRECOND(fraction < REAL4(1.0));

  cell(index).back() -= fraction * cell(index).back();
  // d_cutVoxelSignal(index, fraction);

  DEVELOP_POSTCOND(cell(index).back() > REAL4(0.0));
}



//! Sets cell \a index to a missing value.
/*!
  \param     index Index of voxel stack to set to a missing value.

  Voxels in cell \a index are removed first.
*/
void Block::setMV(
         size_t index)
{
  DEVELOP_PRECOND(!cell(index).isMV());

  removeVoxels(index, cell(index).size());
  cell(index).setMV();

  DEVELOP_POSTCOND(cell(index).isMV());
}



//! Returns the signal which is emitted when voxels have been added to a stack.
/*!
  \return    Signal.
*/
boost::signals2::signal<void(size_t, size_t)>& Block::addVoxelsSignal()
{
  return d_addVoxelsSignal;
}



//! Returns the signal which is emitted when voxels have been removed from a stack.
/*!
  \return    Signal.
*/
boost::signals2::signal<void(size_t, size_t)>& Block::removeVoxelsSignal()
{
  return d_removeVoxelsSignal;
}



// //! Returns the signal which is emitted when a fraction of a voxel has been removed from a stack.
// /*!
//   \return    Signal.
// */
// boost::signals2::signal<void(size_t, REAL4)>& Block::cutVoxelSignal()
// {
//   return d_cutVoxelSignal;
// }



//! Returns whether the block does not contain any voxels.
/*!
  \return    true or false
*/
bool Block::isEmpty() const
{
  for(size_t i = 0; i < nrCells(); ++i) {
    if(!cell(i).isMV() && !cell(i).empty()) {
      return false;
    }
  }

  return true;
}



//! Returns whether the block is a regular block.
/*!
  \return    true or false

  In a regular block all stacks contain the same number of voxels, all with
  the same thickness.

  A block with only missing values is considered a regular block.

  A block with only empty cells is considered a regular block.

  A block without cells is considered a regular block.
*/
bool Block::isRegular() const
{
  bool result = true;

  if(cell(0).isMV()) {
    // Test whether all cells are missing.
    for(size_t i = 1; i < nrCells(); ++i) {
      if(!cell(i).isMV()) {
        // No.
        result = false;
        break;
      }
    }
  }
  else if(cell(0).empty()) {
    // Test whether all cells are not missing but empty.
    for(size_t i = 1; i < nrCells(); ++i) {
      // No.
      if(cell(i).isMV() || !cell(i).empty()) {
        result = false;
        break;
      }
    }
  }
  else {
    size_t nrVoxels = cell(0).size();
    REAL4 thickness = cell(0)[0];

    for(size_t i = 0; i < nrCells(); ++i) {
      // Test whether all cells are not missing and contain an equal number of
      // voxels of equal thickness.
      VoxelStack const& stack(cell(i));

      if(stack.isMV() || stack.size() != nrVoxels || !stack.isRegular() ||
         !dal::comparable(stack[0], thickness)) {
        result = false;
        break;
      }
    }
  }

  return result;
}



//! Returns the number of voxels in the block.
/*!
  \return    Number of voxels.
*/
size_t Block::nrVoxels() const
{
  size_t result = 0;

  for(size_t i = 0; i < nrCells(); ++i) {
    if(!cell(i).isMV()) {
      result += cell(i).size();
    }
  }

  return result;
}



//! Returns whether \a elevation could be set to a valid lowest elevation.
/*!
  \param     elevation Bottom elevation to set.
  \return    true or false

  If this function returns false than no valid elevation could be found,
  so the block contains only missing values.
*/
bool discr::Block::bottomElevation(
         REAL4& elevation) const
{
  size_t i = 0;
  bool result = false;
  pcr::setMV(elevation);

  // Initialise elevation.
  for(; i < nrCells(); ++i) {
    if(!cell(i).isMV()) {
      elevation = cell(i).baseElevation();
      result = true;
      break;
    }
  }

  // Find lowest elevation.
  for(; i < nrCells(); ++i) {
    if(!cell(i).isMV()) {
      elevation = std::min(elevation, cell(i).baseElevation());
    }
  }

  return result;
}



//! Returns whether \a elevation could be set to a valid highest elevation.
/*!
  \param     elevation Top elevation to set.
  \return    true or false

  If this function returns false than no valid elevation could be found,
  so the block contains only missing values.
*/
bool discr::Block::topElevation(
         REAL4& elevation) const
{
  size_t i = 0;
  bool result = false;
  pcr::setMV(elevation);

  // Initialise elevation.
  for(; i < nrCells(); ++i) {
    if(!cell(i).isMV()) {
      elevation = cell(i).surfaceElevation();
      result = true;
      break;
    }
  }

  // Find lowest elevation.
  for(; i < nrCells(); ++i) {
    if(!cell(i).isMV()) {
      elevation = std::max(elevation, cell(i).surfaceElevation());
    }
  }

  return result;
}



//! Returns whether \a bottomElevation and \a topElevation could be set to a valid extreme elevations.
/*!
  \param     bottomElevation Bottom elevation to set.
  \param     topElevation Top elevation to set.
  \return    true or false

  If this function returns false than no valid elevation could be found,
  so the block contains only missing values.
*/
bool discr::Block::extremeElevations(
         REAL4& bottomElevation,
         REAL4& topElevation) const
{
  size_t i = 0;
  bool result = false;
  pcr::setMV(bottomElevation);
  pcr::setMV(topElevation);

  // Initialise elevations.
  for(; i < nrCells(); ++i) {
    VoxelStack const& stack(cell(i));

    if(!stack.isMV()) {
      bottomElevation = stack.baseElevation();
      topElevation = stack.surfaceElevation();
      result = true;
      break;
    }
  }

  // Find extreme elevations.
  for(; i < nrCells(); ++i) {
    VoxelStack const& stack(cell(i));

    if(!stack.isMV()) {
      bottomElevation = std::min(bottomElevation, stack.baseElevation());
      topElevation = std::max(topElevation, stack.surfaceElevation());
    }
  }

  return result;
}



//! Returns whether \a rhs equals *this.
/*!
  \param     rhs Object to compare with.
  \return    true or false
*/
bool Block::equals(Block const& rhs) const
{
  return static_cast<Raster const&>(*this) ==
         static_cast<Raster const&>(rhs) &&
         static_cast<RasterData<VoxelStack> const&>(*this) ==
         static_cast<RasterData<VoxelStack> const&>(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Returns whether \a lhs is equal to \a rhs.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
bool operator==(
         Block const& lhs,
         Block const& rhs)
{
  return lhs.equals(rhs);
}



//! Returns whether \a lhs is not equal to \a rhs.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
bool operator!=(
         Block const& lhs,
         Block const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace discr

