#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif

#ifndef INCLUDED_STRING
#include <std::string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif

#ifndef INCLUDED_DAL_SPATIALCOORDINATE
#include "dal_SpatialCoordinate.h"
#define INCLUDED_DAL_SPATIALCOORDINATE
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



/*!
  \file
  This file contains the implementation of the DataSpace class.
*/



namespace dal {

namespace detail {

//! Inserts the coordinates at \a index in \a addresses into \a values.
/*!
  \param     address Addresses to handle.
  \param     index Index of coordinate to use.
  \param     values Collection of values to fill.

  The \a values collection is first emptied.
*/
template<class T>
void values(
         std::vector<DataSpaceAddress> const& addresses,
         size_t index,
         std::set<T>* values)
{
  values->clear();

  BOOST_FOREACH(DataSpaceAddress const& address, addresses) {
    values->insert(address.coordinate<T>(index));
  }
}



template<typename T>
void extremes(
         std::vector<DataSpaceAddress> const& addresses,
         size_t index,
         boost::any* min,
         boost::any* max)
{
  assert(!addresses.empty());

  // Determine min and max.
  // Initialize by first coordinates.
  T minValue = addresses[0].coordinate<T>(index);
  T maxValue(minValue);

  BOOST_FOREACH(DataSpaceAddress const& address, addresses) {
    T value = address.coordinate<T>(index);
    minValue = std::min<T>(value, minValue);
    maxValue = std::max<T>(value, maxValue);
  }

  *min = minValue;
  *max = maxValue;
}

} // namespace detail

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASPACE MEMBERS
//------------------------------------------------------------------------------

Meaning DataSpace::_meanings[] = {
         Scenarios,
         Samples,
         Time,
         CumulativeProbabilities,
         Space,
         Space
};



size_t DataSpace::_nrMeanings(sizeof(_meanings));



//------------------------------------------------------------------------------
// DEFINITION OF DATASPACE MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  Results in an empty data space (without dimensions). This might be a valid
  dimension in cases where a certain data set is not embedded in a data set
  but provides all the dimensions itself, for example.
*/
DataSpace::DataSpace()

  : _dimensions()

{
}



DataSpace::DataSpace(
         Dimension const& dimension)

  : _dimensions()

{
  addDimension(dimension);
}



//! Constructor.
/*!
  \param     space Space to copy configuration from.
  \param     address Address to copy coordinates from.

  Object is created based on the dimensions in \a space and the coordinates
  in \a address. The dimensions are copied from \a space, except for the
  values. The values of the new object are copied from the \a address. The
  resulting object is a representation of \a address as a DataSpace object.
  This is useful if you want to iterate over a limited set of dimensions
  using a DataSpaceIterator. For example, by extending the values of certain
  dimensions you can iterate over the quantiles while the row and column
  indices stay constant.
*/
DataSpace::DataSpace(
         DataSpace const& space,
         DataSpaceAddress address)

  : _dimensions(space._dimensions)

{
  assert(space.size() == address.size());

  initialiseInvalidCoordinates(address);

  for(size_t i = 0; i < _dimensions.size(); ++i) {
    // _dimensions[i].clear();

    if(address.isValid(i)) {
      switch(_dimensions[i].meaning()) {
        case Scenarios: {
          // Just take the value from the address.
          _dimensions[i].setValue<std::string>(
              address.coordinate<std::string>(i));
          break;
        }
        case CumulativeProbabilities: {
          // First and last from range are taken from the address.
          // Step is taken from the original dimension.
          _dimensions[i].setValues<float>(
              address.coordinate<float>(i),
              address.coordinate<float>(i),
              _dimensions[i].value<float>(2));
          break;
        }
        case Time: {
          // First and last from range are taken from the address.
          // Step is taken from the original dimension.
          _dimensions[i].setValues<size_t>(
              address.coordinate<size_t>(i),
              address.coordinate<size_t>(i),
              _dimensions[i].value<size_t>(2));
          break;
        }
        case Space: {
          SpatialCoordinate const& coordinates(
              address.coordinate<SpatialCoordinate>(i));

          switch(_dimensions[i].discretisation()) {
            case RegularDiscretisation: {
              RasterDimensions const& rasterDimensions(
                   _dimensions[i].value<RasterDimensions>(0));
              size_t index = rasterDimensions.index(coordinates.x(),
                   coordinates.y());

              // Determine coordinates of north west corner of cell.
              double x, y;
              rasterDimensions.coordinates(index, x, y);
              x -= 0.5 * rasterDimensions.cellSize();
              y += 0.5 * rasterDimensions.cellSize();

              // One cell, from the original raster. The cell in which the
              // point from the address is located.
              _dimensions[i].setValue(RasterDimensions(1, 1,
                   rasterDimensions.cellSize(), x, y));
              break;
            }
            case BorderedDiscretisation: {
              _dimensions[i].setValue(SpaceDimensions(coordinates,
                   coordinates));
              break;
            }
            default: {
              assert(false);
              break;
            }
          }

          break;
        }
        default: {
          assert(false);
          break;
        }
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

  If \a addresses is empty, the resulting data space will be empty.
*/
DataSpace::DataSpace(
         DataSpace const& space,
         std::vector<DataSpaceAddress> const& addresses)
{
  if(!addresses.empty()) {
    _dimensions = space._dimensions;

    boost::any min, max;

    for(size_t i = 0; i < space.rank(); ++i) {
      switch(space.dimension(i).meaning()) {
        case Scenarios: {
          assert(_dimensions[i].discretisation() == ExactDiscretisation);
          assert(_dimensions[i].coordinateType() == TextualCoordinates);
          std::set<std::string> values;
          detail::values<std::string>(addresses, i, &values);
          _dimensions[i].setValues(values);
          break;
        }
        case CumulativeProbabilities: {
          assert(_dimensions[i].coordinateType() == NumericalCoordinates);
          assert(space.dimension(i).discretisation() == RegularDiscretisation);

          detail::extremes<float>(addresses, i, &min, &max);
          assert(_dimensions[i].nrValues() == 3);
          _dimensions[i].setValues<float>(
              boost::any_cast<float>(min),
              boost::any_cast<float>(max),
              _dimensions[i].value<float>(2));

          break;
        }
        case Samples: {
          assert(false);
          break;
        }
        case Time: {
          assert(_dimensions[i].coordinateType() == NumericalCoordinates);
          assert(space.dimension(i).discretisation() == RegularDiscretisation);

          detail::extremes<size_t>(addresses, i, &min, &max);
          assert(_dimensions[i].nrValues() == 3);
          _dimensions[i].setValues<size_t>(
              boost::any_cast<size_t>(min),
              boost::any_cast<size_t>(max),
              _dimensions[i].value<size_t>(2));

          break;
        }
        case Space: {
          // FEATURE
          assert(false);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }
    }
  }

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}



//! Destructor.
/*!
*/
DataSpace::~DataSpace()
{
}



//! Checks the consistency of the data space object.
/*!
  \warning   This function aborts when the object is not valid.

  This function is only called in DEBUG_DEVELOP mode.
*/
#ifdef DEBUG_DEVELOP
void DataSpace::checkConsistency()
{
  if(hasScenarios()) {
    assert(indexOf(Scenarios) == 0);
  }

  if(hasSpace()) {
    assert(hasRaster() || hasFeatures());

    if(hasRaster() && hasFeatures()) {
      // Two space dimensions.
      assert(indexOf(Space) == rank() - 2);

      assert(dimension(rank() - 2).meaning() == Space);
      assert(dimension(rank() - 2).discretisation() == RegularDiscretisation);

      assert(dimension(rank() - 1).meaning() == Space);
      assert(dimension(rank() - 1).discretisation() ==
            BorderedDiscretisation);
    }
    else if(hasRaster()) {
      // One space dimension.
      assert(indexOf(Space) == rank() - 1);
      assert(dimension(rank() - 1).meaning() == Space);
      assert(dimension(rank() - 1).discretisation() == RegularDiscretisation);
    }
    else {
      // One space dimension.
      assert(indexOf(Space) == rank() - 1);
      assert(dimension(rank() - 1).meaning() == Space);
      assert(dimension(rank() - 1).discretisation() ==
          BorderedDiscretisation);
    }
  }

  assert(( hasCumProbabilities() && !hasSamples()) ||
         (!hasCumProbabilities() &&  hasSamples()) ||
         (!hasCumProbabilities() && !hasSamples()));

  if(hasTime() && hasCumProbabilities()) {
    assert(indexOf(Time) < indexOf(CumulativeProbabilities));
  }

  if(hasTime() && hasSamples()) {
    assert(indexOf(Time) > indexOf(Samples));
  }

  assert(std::count_if(_dimensions.begin(), _dimensions.end(),
         boost::bind(std::equal_to<Meaning>(),
              boost::bind(&Dimension::meaning, _1),
              Scenarios)) <= 1);
  assert(std::count_if(_dimensions.begin(), _dimensions.end(),
         boost::bind(std::equal_to<Meaning>(),
              boost::bind(&Dimension::meaning, _1),
              Samples)) <= 1);
  assert(std::count_if(_dimensions.begin(), _dimensions.end(),
         boost::bind(std::equal_to<Meaning>(),
              boost::bind(&Dimension::meaning, _1),
              CumulativeProbabilities)) <= 1);
  assert(std::count_if(_dimensions.begin(), _dimensions.end(),
         boost::bind(std::equal_to<Meaning>(),
              boost::bind(&Dimension::meaning, _1),
              Time)) <= 1);
  assert(std::count_if(_dimensions.begin(), _dimensions.end(),
         boost::bind(std::equal_to<Meaning>(),
              boost::bind(&Dimension::meaning, _1),
              Space)) <= 2);
}
#endif



//! Add dimensions from \a space to *this.
/*!
  \param     space Data space configuration to add.
  \return    Reference to result.
  \warning   Make sure that the requirements for a valid data space object are
             still met after calling this function.

  Dimensions from \a space are appended to the collection of dimensions.
*/
DataSpace& DataSpace::operator+=(
         DataSpace const& space)
{
  _dimensions.insert(_dimensions.end(), space._dimensions.begin(),
         space._dimensions.end());

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif

  return *this;
}



DataSpace& DataSpace::merge(
         DataSpace const& space,
         Meaning const* meanings,
         size_t nrMeanings)
{
  size_t lhsIndex = 0;
  size_t rhsIndex = 0;

  for(size_t i = 0; i < nrMeanings; ++i) {
    if(rhsIndex >= space._dimensions.size()) {
      // No more dimensions to merge / copy from.
      // Nothing to merge anymore.
      break;
    }
    else if(lhsIndex >= _dimensions.size()) {
      // No more dimensions to merge / copy to.
      // Add dimension.
      _dimensions.push_back(space.dimension(rhsIndex));
      ++lhsIndex;
      ++rhsIndex;
    }
    else {
      // Valid dimensions on the left and right side.
      if(dimension(lhsIndex).meaning() == meanings[i]) {
        // Dimension on the left side corresponds with the current meaning.

        if(space.dimension(rhsIndex).meaning() == meanings[i]) {
          // Dimension on the right side corresponds with the current meaning.

          if(dimension(lhsIndex).discretisation() ==
                   space.dimension(rhsIndex).discretisation()) {

            // Discretisations of dimensions on both sides are equal.
            // Merge dimensions.
            dimension(lhsIndex) |= space.dimension(rhsIndex);
            ++lhsIndex;
            ++rhsIndex;
          }
          else {
            // Meaning of both dimensions is equal. Discretisation differs.
            // Take ordering of discretisations into account.

            if(dimension(lhsIndex).discretisation() <
                   space.dimension(rhsIndex).discretisation()) {
              // Go for another loop, skipping the lhs and handling the same
              // meaning a second time. This will in effect insert the rhs
              // dimension after the lhs.
              ++lhsIndex;
              --i;
            }
            else {
              // Insert the rhs dimension before the lhs dimension.
              _dimensions.insert(_dimensions.begin() + lhsIndex,
                   space.dimension(rhsIndex));

              // We handled the rhs dimension, but not the lhs dimension.
              ++rhsIndex;
            }
          }
        }
        else {
          // Dimension on the right side does not correspond.
          // Skip dimension.
          ++lhsIndex;
        }
      }
      else if(space.dimension(rhsIndex).meaning() == meanings[i]) {
        // Dimension on the right side corresponds with the current meaning.
        // Dimension on the left side does not correspond.
        // Insert dimension.
        _dimensions.insert(_dimensions.begin() + lhsIndex,
              space.dimension(rhsIndex));
        ++lhsIndex;
        ++rhsIndex;
      }
      // else {
      //   // Dimensions on either side do not correspond with current meaning.
      //   // Skip dimension.
      // }
    }
  }

  assert(lhsIndex <= rank());
  assert(rhsIndex == space.rank());
  assert(rank() >= space.rank());

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif

  return *this;
}



//! Merges dimensions from \a space to *this.
/*!
  \param     space Data space configuration to merge.
  \return    Reference to result.

  When a dimensions in \a space is not present in this data space it is copied.
  When a dimension in \a space is also present in this data space it is
  merged with it.
*/
DataSpace& DataSpace::operator|=(
         DataSpace const& space)
{
  static Meaning meanings1[] = {
    Scenarios,
    Time,
    CumulativeProbabilities,
    Space,
    Space,
  };
  static Meaning meanings2[] = {
    Scenarios,
    Samples,
    Time,
    Space,
    Space,
  };
  static Meaning meanings3[] = {
    Scenarios,
    Time,
    Space,
    Space,
  };

  static Meaning* meanings;
  static size_t nrMeanings;

  if(hasCumProbabilities() || space.hasCumProbabilities()) {
    assert(!hasSamples() && !space.hasSamples());
    meanings = meanings1;
    nrMeanings = sizeof(meanings1);
  }
  else if(hasSamples() || space.hasSamples()) {
    meanings = meanings2;
    nrMeanings = sizeof(meanings2);
  }
  else {
    meanings = meanings3;
    nrMeanings = sizeof(meanings3);
  }

  return merge(space, meanings, nrMeanings);
}



//! Intersects this space with \a space.
/*!
  \param     space Space to intersect with.
  \return    Intersected result space.
  \exception .
  \warning   .
  \sa        .
*/
DataSpace& DataSpace::operator&=(
         DataSpace const& space)
{
  return intersect(space);
}



//!
/*!
  \param     space Space to intersect with.
  \param     flags Flags to steer the behaviour of the intersection.
  \return    This object.
  \exception .
  \warning   .
  \sa        Use trim(DataSpace const&, DataSpaceAddress const&) if you need
             to update an address to the intersected space.
  \todo      Implement intersection of coordinates in Dimension class.

  If \a flags contains DontIntersectCoordinates the resulting space
  is equal to the original space with the dimensions not present in \a
  space removed. Otherwise the coordinates of the dimensions occuring
  in both spaces will be intersected. This means that only coordinates
  present in both dimensions will be part of the resulting space.

  If \a flags contains KeepNonSharedDimensions dimensions present
  in \a space but not in *this will be kept and cleared of coordinates
  instead of erased.
*/
DataSpace& DataSpace::intersect(
         DataSpace const& space,
         IntersectionFlags flags)
{
  size_t lhsIndex = 0;
  size_t rhsIndex = 0;

  for(size_t i = 0; i < _nrMeanings; ++i) {
    if(lhsIndex >= size()) {
      // No more dimensions to intersect with.
      break;
    }
    else if(rhsIndex >= space.size()) {
      // No more dimensions to intersect, handle remaining dimensions.
      if(!(flags & KeepNonSharedDimensions)) {
        while(lhsIndex < size()) {
          eraseDimension(lhsIndex);
        }
      }
      else {
        assert(false); // Is this being used?
        // clear() removed from Dimension.
        // for(;lhsIndex < size(); ++lhsIndex) {
        //   dimension(lhsIndex).clear();
        // }
      }

      break;
    }
    else if(dimension(lhsIndex).meaning() == _meanings[i]) {
      // Dimension on the left corresponds with current meaning.
      if(space.dimension(rhsIndex).meaning() == _meanings[i]) {
        // Dimension on the right corresponds with current meaning.
        if(!(flags & DontIntersectCoordinates)) {
          // Intersect dimensions.
          dimension(lhsIndex) &= space.dimension(rhsIndex);
        }

        // Goto next dimensions.
        ++lhsIndex;
        ++rhsIndex;
      }
      else {
        // Dimension on the right does not correspond with current meaning.
        // Handle dimension on the left side.
        if(!(flags & KeepNonSharedDimensions)) {
          eraseDimension(lhsIndex);
        }
        else {
          assert(false); // Is this being used?
          // clear() removed from Dimension.
          // dimension(lhsIndex).clear();
          // ++lhsIndex;
        }

        // lhsIndex now points to next dimension.
      }
    }
    else {
      // Dimension on the left does not correspond with current meaning.
      if(space.dimension(rhsIndex).meaning() == _meanings[i]) {
        // Dimension on the right corresponds with current meaning.
        // Goto next right dimension.
        ++rhsIndex;
      }
      else {
        // Dimension on the right does not correspond with current meaning.
        // Do nothing.
      }
    }
  }

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif

  return *this;
}



//! Adds a dimension to the data space.
/*!
  \param     dimension Dimension to add to the collection.

  \warning   The data space must remain consistent when calling this function.
*/
void DataSpace::appendDimension(
         Dimension const& dimension)
{
  _dimensions.push_back(dimension);

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}



//! Inserts \a dimension into the data space at \a index.
/*!
  \param     index Position to insert \a dimension.
  \param     dimension Dimension to insert into the collection of dimensions.
  \warning   \a index must be smaller than, or equal to the current number of
             dimensions in the data space.
*/
void DataSpace::insertDimension(
         size_t index,
         Dimension const& dimension)
{
  assert(index <= _dimensions.size());
  _dimensions.insert(_dimensions.begin() + index, dimension);

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}



void DataSpace::addDimension(
         Dimension const& dimension)
{
  switch(dimension.meaning()) {
    case Scenarios: {
      insertDimension(0, dimension);
      break;
    }
    case CumulativeProbabilities: {
      if(hasTime()) {
        insertDimension(indexOf(Time) + 1, dimension);
      }
      else if(hasScenarios()) {
        insertDimension(indexOf(Scenarios) + 1, dimension);
      }
      else {
        insertDimension(0, dimension);
      }

      break;
    }
    case Samples: {
      if(hasScenarios()) {
        insertDimension(indexOf(Scenarios) + 1, dimension);
      }
      else {
        insertDimension(0, dimension);
      }

      break;
    }
    case Time: {
      if(hasSpace()) {
        insertDimension(indexOf(Space), dimension);
      }
      else if(hasCumProbabilities()) {
        insertDimension(indexOf(CumulativeProbabilities), dimension);
      }
      else {
        appendDimension(dimension);
      }

      break;
    }
    case Space: {
      appendDimension(dimension);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}



void DataSpace::eraseDimension(
         size_t index)
{
  assert(index < _dimensions.size());
  _dimensions.erase(_dimensions.begin() + index);

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}



void DataSpace::clear()
{
  _dimensions.clear();

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}



void DataSpace::replaceDimension(
         size_t index,
         Dimension const& dimension)
{
  eraseDimension(index);
  insertDimension(index, dimension);
}



//! Returns the number of dimensions of the data space.
/*!
  \return    Number of dimensions.
*/
size_t DataSpace::rank() const
{
  return _dimensions.size();
}



size_t DataSpace::size() const
{
  return _dimensions.size();
}



//! Returns wheter the data space has no dimensions or whether all dimensions are empty.
/*!
  \return    True or false

  An empty data space has no dimensions or only dimensions with no values. Such
  a data space has no valid data space addresses and no data space iterator can
  be created to iterate through the space.
*/
bool DataSpace::isEmpty() const
{
  return _dimensions.empty();

  // for(size_t i = 0; i < size(); ++i) {
  //   if(!dimension(i).isEmpty()) {
  //     return false;
  //   }
  // }

  // return true;
}



//! Returns the number of dimensions with more than one value set.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
size_t DataSpace::nrWideDimensions() const
{
  size_t result = 0;

  for(size_t i = 0; i < size(); ++i) {
    if(dimension(i).isWide()) {
      ++result;
    }
  }

  return result;
}



//!
/*!
  \param     .
  \return    Index of first wide dimension.
  \exception .
  \warning   .
  \sa        .

  This function returns rank() if no wide dimension is present.
*/
size_t DataSpace::indexOfWideDimension() const
{
  size_t result;

  for(result = 0; result < size(); ++result) {
    if(dimension(result).isWide()) {
      break;
    }
  }

  return result;
}



bool DataSpace::isCompatible(
         DataSpace const& space) const
{
  if(size() == space.size()) {
    for(size_t i = 0; i < size(); ++i) {
      if(dimension(i).coordinateType() != space.dimension(i).coordinateType() ||
         dimension(i).meaning() != space.dimension(i).meaning()) {
        return false;
      }
    }

    return true;
  }

  return false;
}



//! Returns dimension at position \a index.
/*!
  \param     index Index of dimension to return.
  \return    Dimension.
  \warning   \a index must be smaller than the number of dimensions.
*/
Dimension& DataSpace::dimension(
         size_t index)
{
  assert(index < _dimensions.size());

  return _dimensions[index];
}



//! Returns dimension at position \a index.
/*!
  \param     index Index of dimension to return.
  \return    Dimension.
  \warning   \a index must be smaller than the number of dimensions.
*/
Dimension const& DataSpace::dimension(
         size_t index) const
{
  assert(index < _dimensions.size());

  return _dimensions[index];
}



//! Returns the first dimension with meaning \a meaning.
/*!
  \param     meaning Meaning of dimension to search for.
  \return    Dimension.
  \warning   A dimension with meaning \a meaning must be present.
*/
Dimension const& DataSpace::dimension(
         Meaning meaning) const
{
  assert(indexOf(meaning) < _dimensions.size());

  return dimension(indexOf(meaning));
}



//! Returns the index of the first occurence of a dimension with meaning \a meaning.
/*!
  \param     meaning Meaning of dimension to search for.
  \return    Index or the number of dimensions when a relevant dimension could not be found.
*/
size_t DataSpace::indexOf(
         Meaning meaning) const
{
  for(size_t i = 0; i < _dimensions.size(); ++i) {
    if(_dimensions[i].meaning() == meaning) {
      return i;
    }
  }

  return _dimensions.size();
}



//! Returns index of dimension with same address as \a dimension.
/*!
  \param     dimension Dimension to check.
  \return    index or rank() if not found
*/
size_t DataSpace::indexOf(
         Dimension const* dimension) const
{
  size_t result = _dimensions.size();

  for(size_t i = 0; i < _dimensions.size(); ++i) {
    if(&_dimensions[i] == dimension) {
      result = i;
      break;
    }
  }

  return result;
}



//! Returns index of dimension which is compatible with \a dimension.
/*!
  \param     dimension Dimension to check.
  \return    index or rank() if not found
*/
size_t DataSpace::indexOf(
         Dimension const& dimension) const
{
  size_t result = _dimensions.size();

  for(size_t i = 0; i < _dimensions.size(); ++i) {
    if(_dimensions[i].isCompatible(dimension)) {
      result = i;
      break;
    }
  }

  return result;
}



DataSpaceIterator DataSpace::begin() const
{
  return DataSpaceIterator(*this);
}



DataSpaceIterator DataSpace::rbegin() const
{
  DataSpaceIterator result(*this);
  result.setToRBegin();

  return result;
}



DataSpaceIterator DataSpace::end() const
{
  DataSpaceIterator result(*this);
  result.setToEnd();

  return result;
}



DataSpaceIterator DataSpace::rend() const
{
  DataSpaceIterator result(*this);
  result.setToREnd();

  return result;
}



//! Returns whether the data space contains at least one spatial dimension.
/*!
  \return    True or false
*/
bool DataSpace::isSpatial() const
{
  return std::find_if(_dimensions.begin(), _dimensions.end(),
         boost::bind(std::equal_to<Meaning>(),
           boost::bind(&Dimension::meaning, _1), Space)) != _dimensions.end();
}



//! Returns whether the data space constains a scenarios dimension.
/*!
  \return    True or false
*/
bool DataSpace::hasScenarios() const
{
  return indexOf(Scenarios) != _dimensions.size();
}



bool DataSpace::hasCumProbabilities() const
{
  return indexOf(CumulativeProbabilities) != _dimensions.size();
}



//! Returns whether the data space constains a samples dimension.
/*!
  \return    True or false
*/
bool DataSpace::hasSamples() const
{
  return indexOf(Samples) != _dimensions.size();
}



//! Returns whether the data space constains a time dimension.
/*!
  \return    True or false
*/
bool DataSpace::hasTime() const
{
  return indexOf(Time) != _dimensions.size();
}



bool DataSpace::hasSpace() const
{
  return indexOf(Space) != _dimensions.size();
}



//! Returns whether the data space contains raster dimensions.
/*!
  \return    True or false

  This function returns true when the data space has two spatial dimensions,
  both with regular discretisations.
*/
bool DataSpace::hasRaster() const
{
  return std::count_if(_dimensions.begin(), _dimensions.end(),
         boost::bind(
              std::logical_and<bool>(),
              boost::bind(
                   std::equal_to<Meaning>(),
                   boost::bind(&Dimension::meaning, _1),
                   Space),
              boost::bind(
                   std::equal_to<DiscretisationType>(),
                   boost::bind(&Dimension::discretisation, _1),
                   RegularDiscretisation))
         ) == 1;
}



//! Returns whether the data space contains feature dimensions.
/*!
  \return    True or false

  This function returns true when the data space has two spatial dimensions,
  both with bordered discretisations.
*/
bool DataSpace::hasFeatures() const
{
  return std::count_if(_dimensions.begin(), _dimensions.end(),
         boost::bind(
              std::logical_and<bool>(),
              boost::bind(
                   std::equal_to<Meaning>(),
                   boost::bind(&Dimension::meaning, _1),
                   Space),
              boost::bind(
                   std::equal_to<DiscretisationType>(),
                   boost::bind(&Dimension::discretisation, _1),
                   BorderedDiscretisation))
         ) == 1;
}



//! Returns whether \a space equals this data space.
/*!
  \return    True or false
*/
bool DataSpace::equals(
         DataSpace const& rhs) const
{
  if(rank() == rhs.rank()) {
    for(size_t i = 0; i < rank(); ++i) {
      if(dimension(i) != rhs.dimension(i)) {
        return false;
      }
    }

    return true;
  }

  return false;
}



//! Returns whether \a lhs and \a rhs are equal given the configuration of this data space.
/*!
  \param     lhs Data space address to test.
  \param     rhs Data space address to test.
  \return    True or false
  \warning   It is assumed that \a lhs and \a rhs are valid addresses in this
             data space.
*/
bool DataSpace::equal(
         DataSpaceAddress const& lhs,
         DataSpaceAddress const& rhs) const
{
  assert(lhs.size() == rank());
  assert(rhs.size() == rank());

  for(size_t i = 0; i < rank(); ++i) {
    if(( lhs.isValid(i) && !rhs.isValid(i)) ||
       (!lhs.isValid(i) &&  rhs.isValid(i))) {
      return false;
    }
    else if(lhs.isValid(i) && rhs.isValid(i)) {
      switch(dimension(i).meaning()) {
        case Scenarios: {
          if(lhs.coordinate<std::string>(i) != rhs.coordinate<std::string>(i)) {
            return false;
          }
          break;
        }
        case CumulativeProbabilities: {
          if(!comparable<float>(lhs.coordinate<float>(i),
              rhs.coordinate<float>(i))) {
            return false;
          }
          break;
        }
        case Samples: {
          if(lhs.coordinate<size_t>(i) != rhs.coordinate<size_t>(i)) {
            return false;
          }
          break;
        }
        case Time: {
          if(lhs.coordinate<size_t>(i) != rhs.coordinate<size_t>(i)) {
            return false;
          }
          break;
        }
        case Space: {
          if(lhs.coordinate<SpatialCoordinate>(i) !=
              rhs.coordinate<SpatialCoordinate>(i)) {
            return false;
          }
          break;
        }
        default: {
          assert(false);
          break;
        }
      }
    }
  }

  return true;
}



//! Returns \a address with irrelevant coordinates removed.
/*!
  \param     address Address to trim.
  \return    Reference to the \a address passed in.
  \exception .
  \warning   The \a address passed in must contain at least coordinates for
             all dimensions of the data space.

  The address passed in is relevant in \a space but an address is needed which
  is relevant in *this. The *this space must be a subset of \a space.
*/
DataSpaceAddress DataSpace::trim(
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(space.size() >= size());
  assert(space.size() == address.size());

  DataSpaceAddress result(address);

  size_t i = 0;    // Index in result address.
  size_t t = 0;    // Index in target space.

  // Loop over all dimensions in the source space.
  for(size_t s = 0; s < space.size() && t < size(); ++s) {
    // Check wether current dimension in source space is compatible with
    // current dimension in target space.
    if(!space.dimension(s).isCompatible(dimension(t))) {
      // No, get rid of coordinate.
      result.eraseCoordinate(i);
    }
    else {
      // Yes, keep coordinate.
      ++i;
      ++t;
    }
  }

  // Remove any excess coordinates.
  result.resize(size());

  assert(result.size() == size());

  return result;
}



DataSpaceAddress DataSpace::initialiseInvalidCoordinates(
         DataSpaceAddress const& address) const
{
  assert(size() == address.size());

  DataSpaceAddress result(address);

  for(size_t i = 0; i < size(); ++i) {
    if(!result.isValid(i)) {
      Dimension const& dimension(this->dimension(i));

      switch(dimension.meaning()) {
        case Scenarios: {
          // if(!dimension.isEmpty()) {
            result.setCoordinate<std::string>(i,
                   dimension.value<std::string>(0));
          // }

          break;
        }
        case CumulativeProbabilities: {
          // if(!dimension.isEmpty()) {
            // Analyse the values. Quantiles range from 0.0 to 1.0. The user
            // probably wants the analyse the whole distribution or just the
            // tail. In both cases a good starting point is the quantile which
            // is nearest or equal to the median.
            result.setCoordinate<float>(i, dimension.clamp<float>(0.5));
          // }

          break;
        }
        case Samples: {
          // if(!dimension.isEmpty()) {
            result.setCoordinate<size_t>(i, dimension.value<size_t>(0));
          // }

          break;
        }
        case Time: {
          // if(!dimension.isEmpty()) {
            result.setCoordinate<size_t>(i, dimension.value<size_t>(0));
          // }

          break;
        }
        case Space: {
          // if(!dimension.isEmpty()) {
            // Initialise address with nort west corner of the space.
            SpaceDimensions const* dimensions = 0;

            switch(dimension.discretisation()) {
              case RegularDiscretisation: {
                dimensions = &dimension.value<RasterDimensions>(0);
                break;
              }
              case BorderedDiscretisation: {
                dimensions = &dimension.value<SpaceDimensions>(0);
                break;
              }
              default: {
                assert(false);
                break;
              }
            }

            result.setCoordinate<SpatialCoordinate>(i, SpatialCoordinate(
              dimensions->west(), dimensions->north()));
          // }

          break;
        }
        default: {
          assert(false);
          break;
        }
      }
    }
  }

  return result;
}



//! Returns an address with the same rank as this data space.
/*!
  \return    Data space address.
*/
DataSpaceAddress DataSpace::address() const
{
  return DataSpaceAddress(rank());
}



//! Returns whether \a address is available in the data space.
/*!
  \param     address Address to check.
  \return    True or false.
  \warning   If \a address contains invalid coordinates false is returned.

*/
bool DataSpace::contains(
         DataSpaceAddress const& address) const
{
  if(rank() == address.size()) {
    for(size_t i = 0; i < rank(); ++i) {
      if(!address.isValid(i)) {
        return false;
      }

      Dimension const& dimension = this->dimension(i);

      // if(dimension.isEmpty()) {
      //   // Dimension is empty but address coordinate is valid.
      //   return false;
      // }
      // else {
        if(!dimension.contains(address.coordinate(i))) {
          return false;
        }
      // }
    }

    return true;
  }

  return false;
}



//! Returns whether \a address is a valid address.
/*!
  \param     address Address to check.
  \return    true or false
  \warning   It is assumed that address has the same properties as this
             space object (size, type of the coordinates).

  This function returns true when the space is empty.
*/
bool DataSpace::isValid(
         DataSpaceAddress const& address) const
{
  assert(size() == address.size());

  for(size_t i = 0; i < size(); ++i) {
    if(!address.isValid(i)) {
      return false;
    }
  }

  return true;
}



//! Erases all dimensions with meaning \a meaning.
/*!
  \param     meaning Meaning of dimensions to erase from the data space.
*/
void DataSpace::eraseDimension(
         Meaning meaning)
{
  for(int i = size() - 1; i >= 0; --i) {
    if(dimension(i).meaning() == meaning) {
      eraseDimension(i);
    }
  }
}



//! Erases the coordinates from \a address which have meaning \a meaning and returns the result.
/*!
  \param     address Address to copy and change.
  \param     meaning Meaning of dimensions to remove coordinates of.
  \return    New address, based on \a address passed in, but with certain
             coordinates removed.

  If *this does not contain dimensions with meaning \a meaning, the returned
  address will be a copy of the address passed in.
*/
DataSpaceAddress DataSpace::eraseCoordinates(
         DataSpaceAddress const& address,
         Meaning meaning) const
{
  assert(size() == address.size());

  DataSpaceAddress result(address);

  for(int i = size() - 1; i >= 0; --i) {
    if(dimension(i).meaning() == meaning) {
      result.eraseCoordinate(i);
    }
  }

  return result;
}



DataSpaceAddress DataSpace::unsetCoordinates(
         DataSpaceAddress const& address,
         Meaning meaning) const
{
  assert(size() == address.size());

  DataSpaceAddress result(address);

  for(size_t i = 0; i < size(); ++i) {
    if(dimension(i).meaning() == meaning) {
      result.unsetCoordinate(i);
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

PCR_DAL_DECL bool operator==(
         DataSpace const& lhs,
         DataSpace const& rhs)
{
  return lhs.equals(rhs);
}



PCR_DAL_DECL bool operator!=(
         DataSpace const& lhs,
         DataSpace const& rhs)
{
  return !lhs.equals(rhs);
}



DataSpace operator|(
         DataSpace const& lhs,
         DataSpace const& rhs)
{
  return DataSpace(lhs) |= rhs;
}



DataSpace operator&(
         DataSpace const& lhs,
         DataSpace const& rhs)
{
  return DataSpace(lhs) &= rhs;
}



DataSpace operator+(
         DataSpace const& lhs,
         DataSpace const& rhs)
{
  return DataSpace(lhs) += rhs;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

