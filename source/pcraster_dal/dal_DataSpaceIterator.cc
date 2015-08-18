#ifndef INCLUDED_DAL_DATASPACEITERATOR
#include "dal_DataSpaceIterator.h"
#define INCLUDED_DAL_DATASPACEITERATOR
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif



/*!
  \file
  This file contains the implementation of the DataSpaceIterator class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASPACEITERATOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATASPACEITERATOR MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \warning   Don't manipulate or dereference the iterator.

  Creates an invalid iterator.
*/
DataSpaceIterator::DataSpaceIterator()

  : d_space(0),
    d_address(),
    d_setIndices(),
    d_endReached(true),
    d_rendReached(true)

{
}



//! Constructor.
/*!
  \param     space Data space to iterate over.

  The iterator is positioned at the begin of the data space.
*/
DataSpaceIterator::DataSpaceIterator(
         DataSpace const& space)

  : d_space(new DataSpace(space)),
    d_address(d_space->address()),
    d_setIndices(d_address.size())

{
  setToBegin();
}



DataSpaceIterator::DataSpaceIterator(
         DataSpace const& space,
         DataSpaceAddress const& address)

  : d_space(new DataSpace(space)),
    d_address(d_space->address()),
    d_setIndices(d_address.size())

{
  setToAddress(address);
}



//! Copy constructor.
/*!
  \param     rhs Object to copy from.
*/
DataSpaceIterator::DataSpaceIterator(
         DataSpaceIterator const& rhs)

  : d_space(new DataSpace(*rhs.d_space)),
    d_address(rhs.d_address),
    d_setIndices(rhs.d_setIndices),
    d_endReached(rhs.d_endReached),
    d_rendReached(rhs.d_rendReached)

{
}



//! Destructor.
/*!
*/
DataSpaceIterator::~DataSpaceIterator()
{
  if(d_space) {
    delete d_space;
  }
}



//! Assignment operator.
/*!
  \param     rhs Object to assign from.
  \return    *this
*/
DataSpaceIterator& DataSpaceIterator::operator=(
         DataSpaceIterator const& rhs)
{
  if(this != &rhs) {
    if(!rhs.d_space) {
      if(d_space) {
        delete d_space;
        d_space = 0;
      }
    }
    else {
      if(!d_space) {
        d_space = new DataSpace(*rhs.d_space);
      }
      else {
        *d_space = *rhs.d_space;
      }
    }

    d_address = rhs.d_address;
    d_setIndices = rhs.d_setIndices;
    d_endReached = rhs.d_endReached;
    d_rendReached = rhs.d_rendReached;
  }

  return *this;
}



void DataSpaceIterator::setToBegin()
{
  assert(d_space);

  if(d_space->isEmpty()) {
    d_endReached = true;
    d_rendReached = true;
  }
  else {
    initialiseOnFirst();
    d_endReached = false;
    d_rendReached = false;
  }
}



void DataSpaceIterator::setToEnd()
{
  assert(d_space);

  d_endReached = true;

  if(d_space->isEmpty()) {
    d_rendReached = true;
  }
  else {
    d_rendReached = false;
  }

  assert(endReached());
}



void DataSpaceIterator::setToRBegin()
{
  assert(d_space);

  if(d_space->isEmpty()) {
    d_endReached = true;
    d_rendReached = true;
  }
  else {
    initialiseOnLast();
    d_endReached = false;
    d_rendReached = false;
  }
}



void DataSpaceIterator::setToREnd()
{
  assert(d_space);

  if(d_space->isEmpty()) {
    d_endReached = true;
  }
  else {
    d_endReached = false;
  }

  d_rendReached = true;

  assert(rEndReached());
}



void DataSpaceIterator::setToAddress(
         DataSpaceAddress const& address)
{
  assert(d_space);

  initialiseOnAddress(address);
  d_endReached = false;
  d_rendReached = false;
}



bool DataSpaceIterator::endReached() const
{
  return d_endReached;
}



bool DataSpaceIterator::rEndReached() const
{
  return d_rendReached;
}



//! Initialises the iterator starting with the dimension with index \a index.
/*!
  \param     index Index of dimension to start with.
  \exception .
  \warning   .
  \sa        .

  Initialisation means that the coordinates of the address the iterator points
  at are initialised to the first value of the dimensions with an index equal
  or greater than \a index. Dimensions with indices smaller than \a index are
  not initialised.
*/
void DataSpaceIterator::initialiseOnFirst(
         size_t index)
{
  assert(d_space);

  DataSpace const& space(*d_space);

  for(size_t i = index; i < space.size(); ++i) {
    Dimension const& dimension(space.dimension(i));

    // if(dimension.isEmpty()) {
    //   d_address.unsetCoordinate(i);
    // }
    // else {
      d_setIndices[i] = 0;

      switch(dimension.meaning()) {
        case Scenarios: {
          d_address.setCoordinate<std::string>(i,
                   dimension.value<std::string>(0));
          break;
        }
        case CumulativeProbabilities: {
          d_address.setCoordinate<float>(i, dimension.value<float>(0));
          break;
        }
        case Samples:
        case Time: {
          assert(dimension.discretisation() == RegularDiscretisation);
          d_address.setCoordinate<size_t>(i, dimension.value<size_t>(0));
          break;
        }
        case Space: {
          switch(dimension.discretisation()) {
            case RegularDiscretisation: {
              // Initialize on center of upper left cell.
              RasterDimensions const& rasterDimensions(
                   dimension.value<RasterDimensions>(0));
              double x, y;
              rasterDimensions.coordinates(0.5, 0.5, x, y);
              d_address.setCoordinate<SpatialCoordinate>(i,
                   SpatialCoordinate(x, y));
              break;
            }
            case BorderedDiscretisation: {
              // Initialize on north west corner.
              SpaceDimensions const& spaceDimensions(
                   dimension.value<SpaceDimensions>(0));
              double x = spaceDimensions.west();
              double y = spaceDimensions.north();
              d_address.setCoordinate<SpatialCoordinate>(i,
                   SpatialCoordinate(x, y));
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
    // }
  }
}



void DataSpaceIterator::initialiseOnLast(
         size_t index)
{
  assert(d_space);

  DataSpace const& space(*d_space);

  for(size_t i = index; i < space.size(); ++i) {
    Dimension const& dimension(space.dimension(i));

    // if(dimension.isEmpty()) {
    //   d_address.unsetCoordinate(i);
    // }
    // else {
      if(dimension.discretisation() == ExactDiscretisation) {
        d_setIndices[i] = dimension.nrValues() - 1;
      }

      switch(dimension.meaning()) {
        case Scenarios: {
          d_address.setCoordinate<std::string>(i,
                   dimension.value<std::string>(dimension.nrValues() - 1));
          break;
        }
        case CumulativeProbabilities: {
          assert(dimension.discretisation() == RegularDiscretisation);

          float first = dimension.value<float>(0);
          float last = dimension.value<float>(1);
          float interval = dimension.value<float>(2);
          d_setIndices[i] = round<float, size_t>((last - first) / interval);
          d_address.setCoordinate<float>(i, first + d_setIndices[i] * interval);
          // FEATURE replace above code by something like this:
          // d_address.setCoordinate<float>(i,
          //   round<float, size_t>((last - first) / interval));

          break;
        }
        case Samples:
        case Time: {
          assert(dimension.discretisation() == RegularDiscretisation);

          size_t first = dimension.value<size_t>(0);
          size_t last = dimension.value<size_t>(1);
          size_t interval = dimension.value<size_t>(2);
          d_address.setCoordinate<size_t>(i,
              last - ((last - first) % interval));

          break;
        }
        case Space: {
          switch(dimension.discretisation()) {
            case RegularDiscretisation: {
              // Initialize on center of lower right cell.
              RasterDimensions const& rasterDimensions(
                   dimension.value<RasterDimensions>(0));
              double x, y;
              rasterDimensions.coordinates(
                   static_cast<double>(rasterDimensions.nrRows()) - 0.5,
                   static_cast<double>(rasterDimensions.nrCols()) - 0.5, x, y);
              d_address.setCoordinate<SpatialCoordinate>(i,
                   SpatialCoordinate(x, y));
              break;
            }
            case BorderedDiscretisation: {
              // Initialize on south east corner.
              SpaceDimensions const& spaceDimensions(
                   dimension.value<SpaceDimensions>(0));
              double x = spaceDimensions.east();
              double y = spaceDimensions.south();
              d_address.setCoordinate<SpatialCoordinate>(i,
                   SpatialCoordinate(x, y));
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
    // }
  }
}



void DataSpaceIterator::initialiseOnAddress(
         DataSpaceAddress const& address)
{
  assert(d_space);
  assert(d_space->contains(address));

  DataSpace const& space(*d_space);

  for(size_t i = 0; i < space.size(); ++i) {
    Dimension const& dimension(space.dimension(i));

    // if(!dimension.isEmpty()) {
      if(dimension.discretisation() == ExactDiscretisation) {
        d_setIndices[i] = dimension.indexOf<std::string>(
              address.coordinate<std::string>(i));
      }

      switch(dimension.meaning()) {
        case Scenarios: {
          break;
        }
        case CumulativeProbabilities: {
          float value = address.coordinate<float>(i);
          d_setIndices[i] = dimension.indexOf<float>(value);

          break;
        }
        case Samples:
        case Time:
        case Space: {
          // FEATURE TODO update coordinate to center of raster cell?
          break;
        }
        default: {
          assert(false);
          break;
        }
      }
    // }
  }

  d_address = address;
}



//! Increments the iterator to the first address in the layered data space after the current one.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void DataSpaceIterator::increment()
{
  assert(d_space && !d_endReached);

  if(rEndReached()) {
    setToBegin();
    return;
  }

  DataSpace const& space(*d_space);

  for(size_t i = space.size(); i > 0; --i) {
    Dimension const& dimension(space.dimension(i - 1));

    // if(!dimension.isEmpty()) {
      switch(dimension.meaning()) {
        case Scenarios: {
          if(d_setIndices[i - 1] < dimension.nrValues() - 1) {
            ++d_setIndices[i - 1];
            d_address.setCoordinate<std::string>(i - 1,
                     dimension.value<std::string>(d_setIndices[i - 1]));
            initialiseOnFirst(i);
            return;
          }

          break;
        }
        case CumulativeProbabilities: {
          float first = dimension.value<float>(0);
          float last = dimension.value<float>(1);
          float interval = dimension.value<float>(2);
          // FEATURE get rid of d_setIndices code here...
          float value = first + (d_setIndices[i - 1] + 1) * interval;
          if(value < last || comparable<float>(value, last)) {
            ++d_setIndices[i - 1];
            d_address.setCoordinate<float>(i - 1, value);
            initialiseOnFirst(i);
            return;
          }

          break;
        }
        case Samples:
        case Time: {
          size_t last = dimension.value<size_t>(1);
          size_t interval = dimension.value<size_t>(2);
          if(d_address.coordinate<size_t>(i - 1) + interval <= last) {
            d_address.setCoordinate<size_t>(i - 1,
              d_address.coordinate<size_t>(i - 1) + interval);
            initialiseOnFirst(i);
            return;
          }

          break;
        }
        case Space: {
          switch(dimension.discretisation()) {
            case RegularDiscretisation: {
              // Set address to next cell.
              RasterDimensions rasterDimensions =
                   dimension.value<RasterDimensions>(0);

              // Current spatial address.
              SpatialCoordinate spatialCoordinate =
                   d_address.coordinate<SpatialCoordinate>(i - 1);

              // Linear index of next cell.
              size_t index = rasterDimensions.index(spatialCoordinate.x(),
                   spatialCoordinate.y()) + 1;

              if(index < rasterDimensions.nrCells()) {
                // Valid new cell, set coordinates.
                double x, y;
                rasterDimensions.coordinates(index, x, y);

                d_address.setCoordinate<SpatialCoordinate>(i - 1,
                   SpatialCoordinate(x, y));

                // We moved, initialise folowing dimensions to first
                // coordinates and stop.
                initialiseOnFirst(i);
                return;
              }

              break;
            }
            case BorderedDiscretisation: {
              // This is only supported if the north west and south west
              // corners are the same.
              assert(comparable(dimension.value<SpaceDimensions>(0).area(),
                   0.0));
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
    // }
  }

  // No valid coordinates anymore.
  setToEnd();
}



void DataSpaceIterator::decrement()
{
  assert(d_space && !d_rendReached);

  if(endReached()) {
    setToRBegin();
    return;
  }

  DataSpace const& space(*d_space);

  for(size_t i = space.size(); i > 0; --i) {
    Dimension const& dimension(space.dimension(i - 1));

    // if(!dimension.isEmpty()) {
      switch(dimension.meaning()) {
        case Scenarios: {
          if(d_setIndices[i - 1] > 0) {
            --d_setIndices[i - 1];
            d_address.setCoordinate<std::string>(i - 1,
                     dimension.value<std::string>(d_setIndices[i - 1]));
            initialiseOnLast(i);
            return;
          }

          break;
        }
        case CumulativeProbabilities: {
          float first = dimension.value<float>(0);
          float interval = dimension.value<float>(2);
          // FEATURE get rid of d_setIndices code here.
          if(d_setIndices[i - 1] > 0) {
          // if(d_address.coordinate<float>(i - 1) >= first + interval) {
            --d_setIndices[i - 1];
            d_address.setCoordinate<float>(i - 1,
               first + d_setIndices[i - 1] * interval);
            // d_address.setCoordinate<float>(i - 1,
            //    d_address.coordinate<float>(i - 1) - interval);
            initialiseOnLast(i);
            return;
          }

          break;
        }
        case Samples:
        case Time: {
          size_t first = dimension.value<size_t>(0);
          size_t interval = dimension.value<size_t>(2);
          if(d_address.coordinate<size_t>(i - 1) >= first + interval) {
            d_address.setCoordinate<size_t>(i - 1,
               d_address.coordinate<size_t>(i - 1) - interval);
            initialiseOnLast(i);
            return;
          }

          break;
        }
        case Space: {
          switch(dimension.discretisation()) {
            case RegularDiscretisation: {
              // Set address to previous cell.
              RasterDimensions rasterDimensions =
                   dimension.value<RasterDimensions>(0);

              // Current spatial address.
              SpatialCoordinate spatialCoordinate =
                   d_address.coordinate<SpatialCoordinate>(i - 1);

              // Linear index of current cell.
              size_t index = rasterDimensions.index(spatialCoordinate.x(),
                   spatialCoordinate.y());

              if(index > 0) {
                // Index of previous cell.
                --index;

                // Set coordinates.
                double x, y;
                rasterDimensions.coordinates(index, x, y);

                d_address.setCoordinate<SpatialCoordinate>(i - 1,
                   SpatialCoordinate(x, y));

                // We moved, initialise folowing dimensions to first
                // coordinates and stop.
                initialiseOnFirst(i);
                return;
              }

              break;
            }
            case BorderedDiscretisation: {
              // This is only supported if the north west and south west
              // corners are the same.
              assert(comparable(dimension.value<SpaceDimensions>(0).area(),
                   0.0));
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
  // }

  setToREnd();
}



//! Dereference operator, returns the address in the data space the iterator currently points at.
/*!
  \return    Address
  \warning   The returned address is overwritten as soon as the iterator
             is incremented or decremented. It is a temporary object generated
             by the iterator.
*/
DataSpaceAddress const& DataSpaceIterator::operator*() const
{
  assert(d_space && !d_endReached && !d_rendReached);
  return d_address;
}



//! Dereference operator, returns the address in the data space the iterator currently points at.
/*!
  \return    Address
*/
/*
DataSpaceAddress& DataSpaceIterator::operator*()
{
  assert(d_space);
  return d_address;
}
*/



//! Increment operator.
/*!
  \return    Updated data space iterator.
*/
DataSpaceIterator& DataSpaceIterator::operator++()
{
  increment();

  return *this;
}



//! Decrement operator.
/*!
  \return    Updated data space iterator.
*/
DataSpaceIterator& DataSpaceIterator::operator--()
{
  decrement();

  return *this;
}



//! Returns whether the iterator is equal to \a rhs.
/*!
  \param     rhs Iterator to compare to.
  \return    true or false
*/
bool DataSpaceIterator::equals(
         DataSpaceIterator const& rhs) const
{
  return (d_endReached == true && rhs.d_endReached == true) ||
         (d_rendReached == true && rhs.d_rendReached == true) ||
         (d_endReached == rhs.d_endReached &&
         d_rendReached == rhs.d_rendReached &&
         d_space && rhs.d_space && *d_space == *rhs.d_space &&
         d_space->equal(d_address, rhs.d_address) &&
         d_setIndices == rhs.d_setIndices);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Returns whether \a lhs and \a rhs are equal to each other.
/*!
  \param     lhs Iterator.
  \param     rhs Iterator.
  \return    true or false
*/
PCR_DAL_DECL bool operator==(
         DataSpaceIterator const& lhs,
         DataSpaceIterator const& rhs)
{
  return lhs.equals(rhs);
}



//! Returns whether \a lhs and \a rhs are not equal to each other.
/*!
  \param     lhs Iterator.
  \param     rhs Iterator.
  \return    true or false
*/
PCR_DAL_DECL bool operator!=(
         DataSpaceIterator const& lhs,
         DataSpaceIterator const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

