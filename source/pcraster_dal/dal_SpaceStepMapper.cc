#ifndef INCLUDED_DAL_SPACESTEPMAPPER
#include "dal_SpaceStepMapper.h"
#define INCLUDED_DAL_SPACESTEPMAPPER
#endif

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif



/*!
  \file
  This file contains the implementation of the SpaceStepMapper class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPACESTEPMAPPER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SPACESTEPMAPPER MEMBERS
//------------------------------------------------------------------------------

SpaceStepMapper::SpaceStepMapper()

  : d_index(),
    d_coordinate(),
    d_cellSize()

{
  assert(!isValid());
}



SpaceStepMapper::SpaceStepMapper(
         size_t index,
         double coordinate,
         double cellSize)

  : d_index(index),
    d_coordinate(coordinate),
    d_cellSize(cellSize)

{
  assert(isValid());
}



SpaceStepMapper::~SpaceStepMapper()
{
}



SpaceStepMapper& SpaceStepMapper::operator|=(
         SpaceStepMapper const& rhs)
{
  assert(isValid());
  assert(rhs.isValid());

  d_index = std::min<size_t>(d_index, rhs.d_index);
  d_cellSize = std::min<double>(d_cellSize, rhs.d_cellSize);
  d_coordinate = std::min<double>(d_coordinate, rhs.d_coordinate);

  assert(isValid());

  return *this;
}



bool SpaceStepMapper::isValid() const
{
  return !comparable(d_cellSize, 0.0);
}



size_t SpaceStepMapper::index() const
{
  return d_index;
}



double SpaceStepMapper::coordinate() const
{
  return d_coordinate;
}



double SpaceStepMapper::cellSize() const
{
  return d_cellSize;
}



double SpaceStepMapper::destination(
         double index) const
{
  assert(isValid());

  return d_coordinate + d_cellSize * (double(index) - d_index);
}



double SpaceStepMapper::source(
         double coordinate) const
{
  assert(isValid());

  return double(d_index) + (coordinate - d_coordinate) / d_cellSize;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

#ifdef DEBUG_DEVELOP
std::ostream& operator<<(
         std::ostream& stream,
         SpaceStepMapper const& mapper)
{
  stream
    << mapper.index() << ", " << mapper.coordinate()
    << ", " << mapper.cellSize();

  return stream;
}
#endif



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

