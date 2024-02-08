#ifndef INCLUDED_DAL_VECTOR
#include "dal_Vector.h"
#define INCLUDED_DAL_VECTOR
#endif

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Vector class.
*/

namespace {

} // Anonymous namespace



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VECTOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VECTOR MEMBERS
//------------------------------------------------------------------------------

Vector::Vector(
         RasterDimensions const& dimensions,
         TypeId typeId)

  : Dataset(VECTOR),
    _dimensions(dimensions),
    _typeId(typeId),
    _x(new Matrix(dimensions.nrRows(), dimensions.nrCols(), typeId)),
    _y(new Matrix(dimensions.nrRows(), dimensions.nrCols(), typeId))

{
}



Vector::Vector(
         Vector const& rhs)

  : Dataset(rhs),
    _dimensions(rhs._dimensions),
    _x(new Matrix(*rhs._x)),
    _y(new Matrix(*rhs._y))

{
}



Vector::~Vector()
{
}



Vector& Vector::operator=(
         Vector const& rhs)
{
  if(this != &rhs) {
    dynamic_cast<Dataset&>(*this) = rhs;
    _dimensions = rhs._dimensions;
    _x.reset(new Matrix(*rhs._x));
    _y.reset(new Matrix(*rhs._y));
  }

  return *this;
}



void Vector::createCells()
{
  _x->createCells();
  _y->createCells();
}



void Vector::transfer(
         Matrix& x,
         Matrix& y)
{
  assert(x.cellsAreCreated());
  assert(y.cellsAreCreated());

  _x->transfer(x.release());
  _y->transfer(y.release());
}



TypeId Vector::typeId() const
{
  return _typeId;
}



bool Vector::cellsAreCreated() const
{
  return _x->cellsAreCreated() && _y->cellsAreCreated();
}



RasterDimensions const& Vector::dimensions() const
{
  return _dimensions;
}



size_t Vector::nrRows() const
{
  return _dimensions.nrRows();
}



size_t Vector::nrCols() const
{
  return _dimensions.nrCols();
}



size_t Vector::nrCells() const
{
  return _dimensions.nrCells();
}



//! Returns the cell size.
/*!
  \return    Cell size.
*/
double Vector::cellSize() const
{
  return _dimensions.cellSize();
}



//! Returns the northern-most y-coordinate.
/*!
  \return    Northern-most y-coordinate.

  Top side of the top cell, the cell with a row index of 0.
*/
double Vector::north() const
{
  return _dimensions.north();
}



//! Returns the southern-most y-coordinate.
/*!
  \return    Southern-most y-coordinate.

  Bottom side of the bottom cell, the cell with a row index of nrRows()-1.
*/
double Vector::south() const
{
  return _dimensions.south();
}



//! Returns the western-most x-coordinate.
/*!
  \return    Western-most x-coordinate.

  Left side of the left cell, the cell with a col index of 0.
*/
double Vector::west() const
{
  return _dimensions.west();
}



//! Returns the eastern-most x-coordinate.
/*!
  \return    Eastern-most x-coordinate.

  Right side of the right cell, the cell with a col index of nrCols()-1.
*/
double Vector::east() const
{
  return _dimensions.east();
}



void const* Vector::xCells() const
{
  return _x->cells();
}



void* Vector::xCells()
{
  return _x->cells();
}



void const* Vector::yCells() const
{
  return _y->cells();
}



void* Vector::yCells()
{
  return _y->cells();
}



//! Sets the type id of the cell values to \a typeId.
/*!
  \param     typeId New type id.
  \warning   The array's for cell values must not already be created.
*/
void Vector::setTypeId(
         TypeId typeId)
{
  assert(!(_typeId != typeId && cellsAreCreated() && hasExtremes()));
  _typeId = typeId;
}



void Vector::setAllMV()
{
  assert(_typeId == TI_REAL4 || _typeId == TI_REAL8);

  _x->setAllMV();
  _y->setAllMV();
  _min = boost::any();
  _max = boost::any();
}



bool Vector::hasExtremes() const
{
  return !_min.empty() && !_max.empty();
}



// bool Vector::allMV() const
// {
//   return _min.empty() || _max.empty();
// }



template<typename T>
void Vector::calculateExtremes()
{
  assert(cellsAreCreated());

  _min = boost::any();
  _max = boost::any();

  size_t nrCells = this->nrCells();
  T value;
  T min = 0; // Shut up compiler.
  T max = 0; // Shut up compiler.
  T minX = 0; // Shut up compiler.
  T minY = 0; // Shut up compiler.
  T maxX = 0; // Shut up compiler.
  T maxY = 0; // Shut up compiler.
  bool initialized = false;
  T const*x = _x->cells<T>();
  T const*y = _y->cells<T>();

  for(size_t i = 0; i < nrCells; ++i) {
    if(!pcr::isMV(x[i]) && !pcr::isMV(y[i])) {
      // Don't take the sqrt because it is not necessary.
      value = x[i] * x[i] + y[i] * y[i];

      if(!initialized) {
        min = value;
        minX = x[i];
        minY = y[i];

        max = value;
        maxX = x[i];
        maxY = y[i];
        initialized = true;
      }
      else {
        if(value < min) {
          min = value;
          minX = x[i];
          minY = y[i];
        }
        else if(value > max) {
          max = value;
          maxX = x[i];
          maxY = y[i];
        }
      }
    }
  }

  if(initialized) {
    _min = std::sqrt(minX * minX + minY * minY);
    _max = std::sqrt(maxX * maxX + maxY * maxY);
  }
}



void Vector::calculateExtremes()
{
  assert(cellsAreCreated());

  switch(_typeId) {
    case(TI_REAL4): {
      calculateExtremes<REAL4>();
      break;
    }
    case(TI_REAL8): {
      calculateExtremes<REAL8>();
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

