#ifndef INCLUDED_DAL_MATRIXDIMENSIONS
#include "dal_MatrixDimensions.h"
#define INCLUDED_DAL_MATRIXDIMENSIONS
#endif

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the MatrixDimensions class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATRIXDIMENSIONS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MATRIXDIMENSIONS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     nrRows Number of rows.
  \param     nrCols Number of columns.
*/
MatrixDimensions::MatrixDimensions(
         size_t nrRows,
         size_t nrCols)

  : _nrRows(nrRows),
    _nrCols(nrCols)

{
}



//! Copy constructor.
/*!
  \param     rhs Object to copy from.
*/
MatrixDimensions::MatrixDimensions(
         MatrixDimensions const& rhs)

  : _nrRows(rhs._nrRows),
    _nrCols(rhs._nrCols)

{
}



//! Assignment operator.
/*!
  \param     rhs Object to copy from.
  \return    Reference to *this.
*/
MatrixDimensions& MatrixDimensions::operator=(
         MatrixDimensions const& rhs)
{
  if(this != &rhs) {
    _nrRows = rhs._nrRows;
    _nrCols = rhs._nrCols;
  }

  return *this;
}



//! Destructor.
/*!
*/
MatrixDimensions::~MatrixDimensions()
{
}



//! Returns whether \a rhs equals *this.
/*!
  \param     rhs Object to compare with.
  \return    true or false
*/
bool MatrixDimensions::equals(
         MatrixDimensions const& rhs) const
{
  return _nrRows == rhs._nrRows && _nrCols == rhs._nrCols;
}



void MatrixDimensions::setNrRows(
         size_t nrRows)
{
  _nrRows = nrRows;
}



void MatrixDimensions::setNrCols(
         size_t nrCols)
{
  _nrCols = nrCols;
}



//! Returns the number of rows.
/*!
  \return    Number of rows.
*/
size_t MatrixDimensions::nrRows() const
{
  return _nrRows;
}



//! Returns the number of columns.
/*!
  \return    Number of columns.
*/
size_t MatrixDimensions::nrCols() const
{
  return _nrCols;
}



//! Returns the number of cells.
/*!
  \return    Number of cells.
*/
size_t MatrixDimensions::nrCells() const
{
  return _nrRows * _nrCols;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Equality operator.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
bool operator==(
         MatrixDimensions const& lhs,
         MatrixDimensions const& rhs)
{
  return lhs.equals(rhs);
}



//! Inequality operator.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
bool operator!=(
         MatrixDimensions const& lhs,
         MatrixDimensions const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

