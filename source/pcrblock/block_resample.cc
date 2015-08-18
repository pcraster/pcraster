#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
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

// Module headers.
#ifndef INCLUDED_BLOCK_VOXELATHEIGHT
#include "block_voxelatheight.h"
#define INCLUDED_BLOCK_VOXELATHEIGHT
#endif



namespace block {

//! Creates a resampled version of \a block with voxels of \a thickness height.
/*!
  \param     result Resulting block.
  \param     block Block to resample.
  \param     thickness Thickness of new voxels.
  \exception .
  \warning   \a result must not already contain voxels.
  \sa        .

  The result is a regular block discretisation in which each voxel stack has
  the same base elevation and the same surface elevation. This version can be
  used to communicate with code which doesn't know how to handle our ragged
  vector version of the block discretisation.
*/
void resample(
         discr::Block& result,
         discr::Block const& block,
         REAL4 thickness)
{
  PRECOND(thickness > 0);
  DEVELOP_PRECOND(static_cast<discr::Raster const&>(result) ==
         static_cast<discr::Raster const&>(block));

  REAL4 base, surface;

  if(!block.extremeElevations(base, surface)) {
    // All MV's.
    for(size_t i = 0; i < block.nrCells(); ++i) {
      result.cell(i).setMV();
    }
  }
  else {
    size_t nrVoxels = static_cast<size_t>(
         std::ceil(surface - base) / thickness);
    // No missing values in resulting block!
    for(size_t i = 0; i < block.nrCells(); ++i) {
      result.cell(i).setBaseElevation(base);
      DEVELOP_PRECOND(result.cell(i).empty());
      result.addVoxels(i, nrVoxels, thickness);
    }

    /*
    for(size_t i = 0; i < block.nrCells(); ++i) {
      if(block.cell(i).isMV()) {
        result.cell(i).setMV();
      }
      else {
        result.cell(i).setBaseElevation(base);
        DEVELOP_PRECOND(result.cell(i).empty());
        result.addVoxels(i, nrVoxels, thickness);
      }
    }
    */
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Check commented conditions.
*/
template<typename T>
inline T resampleMajority(
         double firstFraction,
         std::vector<REAL4>::const_iterator firstIter,
         double lastFraction,
         std::vector<REAL4>::const_iterator lastIter,
         typename std::vector<T>::const_iterator valIter,
         REAL4 thickness)
{
  // DEVELOP_PRECOND(firstFraction > REAL4(0.0));
  // DEVELOP_PRECOND(lastFraction > REAL4(0.0));
  DEVELOP_PRECOND((firstIter == lastIter && firstFraction == lastFraction) ||
         (firstIter != lastIter));

  double valueThickness = 0.0;    // Thickness of source voxels with values.
  double mvThickness = 0.0;       // Thickness of source voxels without values.
  std::map<T, double> map;        // Map with source value and thickness.

  // Handle first, fractional source voxel.
  if(!pcr::isMV(*valIter)) {
    valueThickness += firstFraction * *firstIter;
    map[static_cast<T>(*valIter)] += firstFraction * *firstIter;
  }
  else {
    mvThickness += firstFraction * *firstIter;
  }

  if(firstIter != lastIter) {

    // Handle middle, whole source voxels.
    while(++firstIter != lastIter) {

      ++valIter;

      if(!pcr::isMV(*valIter)) {
        valueThickness += *firstIter;
        map[static_cast<T>(*valIter)] += *firstIter;
      }
      else {
        mvThickness += *firstIter;
      }
    }

    ++valIter;

    DEVELOP_POSTCOND(firstIter == lastIter);

    // Handle last, fractional source voxel.
    if(!pcr::isMV(*valIter)) {
      valueThickness += lastFraction * *firstIter;
      map[static_cast<T>(*valIter)] += lastFraction * *firstIter;
    }
    else {
      mvThickness += lastFraction * *firstIter;
    }
  }

  T result;                       // Most occuring value.

  // Calculate result.
  if(mvThickness > valueThickness || valueThickness < (0.5 * thickness)) {
    pcr::setMV(result);
  }
  else {
    DEVELOP_PRECOND(!map.empty());
    double length = 0.0;

    for(typename std::map<T, double>::const_iterator it = map.begin();
         it != map.end(); ++it) {
      // DEVELOP_PRECOND((*it).second > 0.0);
      if((*it).second > length) {
        result = (*it).first;
        length = (*it).second;
      }
    }
  }

  return result;
}



template<typename T>
inline T resampleAverage(
         double firstFraction,
         std::vector<REAL4>::const_iterator firstIter,
         double lastFraction,
         std::vector<REAL4>::const_iterator lastIter,
         typename std::vector<T>::const_iterator valIter,
         REAL4 thickness)
{
  double value = 0.0;        // Average.
  double valueThickness = 0.0;
  double mvThickness = 0.0;

  // std::cout << "firstFraction: " << firstFraction << std::endl;
  // std::cout << "lastFraction: " << lastFraction << std::endl;
  // std::cout << "thickness: " << thickness << std::endl;
  // std::cout << "firstIter == lastIter: " << (firstIter == lastIter) << std::endl;

  // Handle first, fractional source voxel.
  if(pcr::isMV(*valIter)) {
    mvThickness += firstFraction * *firstIter;
  }
  else {
    valueThickness += firstFraction * *firstIter;
    value += firstFraction * *firstIter * static_cast<double>(*valIter);
  }

  if(firstIter != lastIter) {
    // Handle middle, whole source voxels.
    for(++firstIter, ++valIter; firstIter != lastIter;
         ++firstIter, ++valIter) {
      if(!pcr::isMV(*valIter)) {
        valueThickness += *firstIter;
        value += *firstIter * static_cast<double>(*valIter);
      }
      else {
        mvThickness += *firstIter;
      }
    }

    DEVELOP_POSTCOND(firstIter == lastIter);

    // Handle last, fractional source voxel.
    if(pcr::isMV(*valIter)) {
      mvThickness += lastFraction * *firstIter;
    }
    else {
      valueThickness += lastFraction * *firstIter;
      value += lastFraction * *firstIter * static_cast<double>(*valIter);
    }
  }

  T result;                  // Average as a T.

  // std::cout << "mvThickness: " << mvThickness << std::endl;
  // std::cout << "valueThickness: " << valueThickness << std::endl;

  // Calculate result.
  if(mvThickness > valueThickness || valueThickness < (0.5 * thickness)) {
    pcr::setMV(result);
  }
  else {
    value /= valueThickness;
    result = static_cast<T>(value);
  }

  return result;
}



/*
   CW no need for templates of resample if fucntion selection
      works by argument type lookup
 */


inline UINT1 resample(
         double firstFraction,
         std::vector<REAL4>::const_iterator firstIter,
         double lastFraction,
         std::vector<REAL4>::const_iterator lastIter,
         std::vector<UINT1>::const_iterator valIter,
         REAL4 thickness)
{
  return resampleMajority<UINT1>(
              firstFraction, firstIter,
              lastFraction, lastIter,
              valIter, thickness);
}



inline INT4 resample(
         double firstFraction,
         std::vector<REAL4>::const_iterator firstIter,
         double lastFraction,
         std::vector<REAL4>::const_iterator lastIter,
         std::vector<INT4>::const_iterator valIter,
         REAL4 thickness)
{
  return resampleMajority<INT4>(
              firstFraction, firstIter,
              lastFraction, lastIter,
              valIter, thickness);
}



inline REAL4 resample(
         double firstFraction,
         std::vector<REAL4>::const_iterator firstIter,
         double lastFraction,
         std::vector<REAL4>::const_iterator lastIter,
         std::vector<REAL4>::const_iterator valIter,
         REAL4 thickness)
{
  return resampleAverage<REAL4>(
              firstFraction, firstIter,
              lastFraction, lastIter,
              valIter, thickness);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Check commented conditions.
*/
template<class ValueType>
static void resample(
         std::vector<ValueType>& result,
         discr::VoxelStack const& newStack,
         std::vector<ValueType> const& data,
         discr::VoxelStack const& currentStack)
{
  // NO: result does not contain valid data yet!
  // if(currentStack == newStack) {
  //   return;
  // }

  DEVELOP_PRECOND(result.size() == newStack.size());
  DEVELOP_PRECOND(data.size() == currentStack.size());

  if(currentStack.isMV() || currentStack.empty() ||
         newStack.isMV() || newStack.empty() ||
         dal::greaterOrComparable(
              currentStack.baseElevation(), newStack.surfaceElevation()) ||
         dal::smallerOrComparable(
              currentStack.surfaceElevation(), newStack.baseElevation())) {

    // Nothing useful to do. Set all new values to missing values.
    if(!result.empty()) {
      pcr::setMV(&(*result.begin()), result.size());
    }
  }
  else {

    // There is overlap between (some of) the voxels in both stacks.
    // The meat of resample:
    // For every voxel 'newVoxel' of newStack
    //  Determine which voxels of currentStack contribute to the value
    //  of 'newVoxel'.
    //    Determine which voxels of currentStack lie totally within
    //    'newVoxel'.
    //    Determine bottom and upper border voxels of currentStack
    //    which lie partly within 'newVoxel'.
    //  Determine new value, based on voxels of currentStack which
    //  contribute to the value of 'newVoxel'. If the attribute type is a
    //  floating point value: take the (weighted) average value. If the
    //  attribute type is an integral type: take the most prevailing value.

    // Iterator to first contributing attribute value.
    typename std::vector<REAL4>::const_iterator curItFirst;
    // Iterator to new voxel.
    typename std::vector<REAL4>::const_iterator newIt;

    //----------------------------------------------------------------------
    // Init...
    //----------------------------------------------------------------------
    if(dal::greaterOrComparable(
         newStack.baseElevation(), currentStack.baseElevation())) {

      // Find voxel in currentStack which is at the same height as the
      // baseElevation of newStack.
      curItFirst = std::find_if(currentStack.begin(), currentStack.end(),
         VoxelAtHeight(currentStack.baseElevation(), newStack.baseElevation()));
      DEVELOP_POSTCOND(curItFirst != currentStack.end());
      newIt = newStack.begin();
    }
    else {
      // Find the voxel in newStack which is at the same height as the
      // baseElevation of currentStack.
      newIt = std::find_if(newStack.begin(), newStack.end(),
               VoxelAtHeight(newStack.baseElevation(),
               currentStack.baseElevation()));
      DEVELOP_POSTCOND(newIt != newStack.end());
      size_t index = newIt - newStack.begin();

      // Set all new attribute values untill this voxel to MV.
      pcr::setMV(&(*result.begin()), index);

      curItFirst = currentStack.begin();
    }

    // Make sure we have new voxels left to process.
    DEVELOP_PRECOND(curItFirst != currentStack.end());
    DEVELOP_PRECOND(newIt != newStack.end());

    //--------------------------------------------------------------------
    // Init ok, resample...
    //--------------------------------------------------------------------
    // A height in currentStack.
    double currentHeight = currentStack.bottomElevation(curItFirst -
               currentStack.begin());
    // Bottom of new Voxel.
    REAL4 newVoxelBottom;
    // Top of new Voxel.
    REAL4 newVoxelTop;
    // Fraction of first contributing attribute value.
    double firstFraction;
    // Fraction of last contributing attribute value.
    double lastFraction;
    // Iterator to last contributing attribute value.
    typename std::vector<REAL4>::const_iterator curItLast;
    // Index of current attribute value.
    size_t currentValueIndex;
    // Index of new attribute value.
    size_t newValueIndex = newIt - newStack.begin();

    curItLast = curItFirst;

    // Visit all new voxels.
    for(; newIt != newStack.end() && curItFirst != currentStack.end();
         ++newIt, ++newValueIndex) {

      newVoxelBottom = newStack.bottomElevation(newValueIndex);
      newVoxelTop = newVoxelBottom + *newIt;

      // std::cout << "*curItFirst: " << *curItFirst << std::endl;
      // std::cout << "topElevation: " << currentStack.topElevation(curItFirst - currentStack.begin()) << std::endl;
      // std::cout << "newVoxelTop: " << newVoxelTop << std::endl;
      // std::cout << "currentHeight: " << currentHeight << std::endl;
      // std::cout << "newVoxelBottom: " << newVoxelBottom << std::endl;

      // DEVELOP_PRECOND(dal::comparable(currentStack.topElevation(curItFirst - currentStack.begin()) -
      //    REAL4(currentHeight), *curItFirst));
      DEVELOP_PRECOND(*curItFirst > REAL4(0.0));
      // DEVELOP_PRECOND(
      //    currentStack.topElevation(curItFirst - currentStack.begin()) >
      //    newVoxelBottom);

/*
*curItFirst: 0.1
topElevation: 4.2
newVoxelTop: 4.4
currentHeight: 4.1
newVoxelBottom: 4.2
firstFraction: 0

4.2 - 4.2
*/
      firstFraction =
         (double(std::min(currentStack.topElevation(curItFirst - currentStack.begin()),
                 newVoxelTop)) -
         std::max(currentHeight, double(newVoxelBottom))) / *curItFirst;
      // std::cout << "firstFraction: " << firstFraction << std::endl;
      // std::cout << dal::comparable(firstFraction, 0.0) << std::endl;
      // DEVELOP_POSTCOND(firstFraction > 0.0);
      // DEVELOP_POSTCOND(dal::smallerOrComparable(firstFraction, 1.0));
      currentValueIndex = curItFirst - currentStack.begin();

      // PRECOND:
      //   newIt         : on new resampled voxel
      //   newVoxelTop   : top of new voxel
      //   newVoxelBottom: bottom of new voxel
      //   curItFirst    : on first voxel contributing to new value
      //   curItLast     : on curItFirst
      //   firstFraction : fraction of *curItFirst contributing to new
      //                   value
      //   lastFraction  : doesn't matter
      //   currentValueIndex: on same voxel as curItFirst
      //   newValueIndex : on same voxel as newIt
      //   currentHeight : bottom of current voxel contributing to new
      //                   value

      // std::cout << currentHeight << '\t' << newVoxelTop << std::endl;
      // DEVELOP_PRECOND(currentHeight < newVoxelTop);

      // Determine upper voxel still contributing to the new value.
      while((currentHeight + *curItLast) < newVoxelTop &&
             (curItLast + 1) != currentStack.end()) {
        currentHeight += *curItLast++;
      }

      // Determine fraction of *curItLast contributing to new value.
      DEVELOP_PRECOND(curItLast != currentStack.end());
      DEVELOP_PRECOND(*curItLast > REAL4(0.0));
      /*
*curItFirst: 0.1
topElevation: 4.1
newVoxelTop: 4.2
currentHeight: 4
newVoxelBottom: 4
firstFraction: 0.999998
*/
      lastFraction =
             (double(std::min(currentStack.topElevation(curItLast - currentStack.begin()),
                 newVoxelTop)) -
             std::max(currentHeight, double(newVoxelBottom))) / *curItLast;
      // std::cout << "lastFraction: " << lastFraction << std::endl;
      // DEVELOP_POSTCOND(lastFraction > 0.0);
      // DEVELOP_POSTCOND(dal::smallerOrComparable(lastFraction, 1.0));

      // Resample.
      result[newValueIndex] = resample(
            firstFraction, curItFirst,
            lastFraction, curItLast,
            data.begin() + currentValueIndex, *newIt);

      // Init for next new voxel.
      if(dal::comparable(lastFraction, 1.0) && curItLast != currentStack.end()) {
        currentHeight += *curItLast++;
      }

      curItFirst = curItLast;
    }

    // In case the current stack ends before the new one, the loop above
    // exits before all values in the new stack have been assigned to.
    // Set all remaining new attribute values from this voxel up to MV.
    if(newValueIndex < result.size()) {
       pcr::setMV(&result[newValueIndex], result.size() - newValueIndex);
    }
    newIt += result.size() - newValueIndex;
    newValueIndex += result.size() - newValueIndex;

    //--------------------------------------------------------------------
    // Resample ok, wrap up...
    //--------------------------------------------------------------------
    DEVELOP_POSTCOND(newIt == newStack.end());
    DEVELOP_POSTCOND(newValueIndex == newStack.size());
  }
}



template<class ValueType>
void resample(
         discr::BlockData<ValueType>& result,
         discr::BlockData<ValueType> const& data)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(result);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(data);

  DEVELOP_PRECOND(static_cast<discr::Raster const&>(*result.block()) ==
         static_cast<discr::Raster const&>(*data.block()));
  DEVELOP_PRECOND(&result != &data);

  for(size_t i = 0; i < data.block()->nrCells(); ++i) {
    if(data.block()->cell(i).isMV()) {
      // Source stack is missing.
      if(!result.block()->cell(i).isMV()) {
        // Destination stack is not missing, fill it with missing values.
        pcr::setMV(&(*result.cell(i).begin()),
              result.block()->cell(i).size());
      }
    }
    else {
      // Source stack is present.
      if(!result.block()->cell(i).isMV()) {
        // Destination stack is present, resample.
        resample(result.cell(i), result.block()->cell(i),
              data.cell(i), data.block()->cell(i));
      }
    }
  }

  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(result);
}



template
void resample<UINT1>(
         std::vector<UINT1>&,
         discr::VoxelStack const&,
         std::vector<UINT1> const&,
         discr::VoxelStack const&);
template
void resample<INT4>(
         std::vector<INT4>&,
         discr::VoxelStack const&,
         std::vector<INT4> const&,
         discr::VoxelStack const&);
template
void resample<REAL4>(
         std::vector<REAL4>&,
         discr::VoxelStack const&,
         std::vector<REAL4> const&,
         discr::VoxelStack const&);
template
void resample<UINT1>(
         discr::BlockData<UINT1>&,
         discr::BlockData<UINT1> const&);
template
void resample<INT4>(
         discr::BlockData<INT4>&,
         discr::BlockData<INT4> const&);
template
void resample<REAL4>(
         discr::BlockData<REAL4>&,
         discr::BlockData<REAL4> const&);

} // namespace block

