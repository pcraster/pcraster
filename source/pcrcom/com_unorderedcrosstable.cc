#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_UNORDEREDCROSSTABLE
#include "com_unorderedcrosstable.h"
#define INCLUDED_COM_UNORDEREDCROSSTABLE
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#include "com_countedobject.cc"



/*!
  \file
  This file contains the implementation of the UnOrderedCrossTable class.
*/



template class com::CountedObject<com::UnOrderedCrossTable>;



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CROSSTABLE MEMBERS
//------------------------------------------------------------------------------

std::map<size_t, size_t**> com::UnOrderedCrossTable::d_indicesMap;



//! Generates a 2D array with indices.
/*!
  \param     size Size of the cross table.
  \exception .
  \warning   You only need to create one 2D indices array for a cross table
             with a certain size.

  To translate a position in a 2D cross table to a position in the 1D array
  used in the UnOrderedCrossTable class, we create a 2D array with indices to
  the 1D array.

  \todo
   This will go into the static data struct but what about deletion?
   Solution: make a singleton static class object private to
    this file
*/
void com::UnOrderedCrossTable::createIndices(size_t size)
{
/*
#ifdef DEBUG_DEVELOP
  PRECOND(d_indicesMap.find(size) == d_indicesMap.end());
#endif
*/

  if(d_indicesMap.find(size) == d_indicesMap.end()) {

    size_t ** indices = new size_t*[size];

    for(size_t i = 0; i < size; ++i) {
      indices[i] = new size_t[size];
    }

    for(size_t r = 0; r < size; ++r) {
      for(size_t c = 0; c < size; ++c) {
        indices[r][c] = calculateIndex(size, r, c);
      }
    }

    d_indicesMap[size] = indices;
    //const_cast<size_t**>(d_indicesMap[size]) = indices;
    //d_indicesMap[size] = indices;
  }
}



//! Calculates an index into a 1D array from indices of a 2D array.
/*!
  \param     size Size/dimension of 2D array.
  \param     row Row index.
  \param     col Column index.
  \return    Index.
*/
size_t com::UnOrderedCrossTable::calculateIndex(size_t size, size_t row,
                   size_t col)
{
#ifdef DEBUG_DEVELOP
  PRECOND(row < size && col < size);
#endif

  if(row > col) {
    std::swap(row, col);
  }

  size_t i1 = row * size + col;
  size_t i2 = i1 / size;
  size_t i3 = 0;
  for(size_t i = 1; i <= i2; ++i) {
    i3 += i;
  }
  return i1 - i3;
}



//! Looks up an index into a 1D array from indices of a 2D array.
/*!
  \param     size Size/dimension of 2D array.
  \param     row Row index.
  \param     col Column index.
  \return    Index.
*/
size_t com::UnOrderedCrossTable::index(size_t size, size_t row, size_t col)
{
#ifdef DEBUG_DEVELOP
  PRECOND(row < size && col < size);
  PRECOND(d_indicesMap.find(size) != d_indicesMap.end());
#endif

  return d_indicesMap[size][row][col];
}



//! Calculates the length of the array needed to store all values.
/*!
  \param     size Size of matrix.
  \return    Length of the array.
*/
inline size_t com::UnOrderedCrossTable::length(size_t size) const
{
  // PRECOND(d_size % 2 == 1);  // Make sure that d_size is not even.

  return size * size - (size * size - size) / 2;
}



//------------------------------------------------------------------------------
// DEFINITION OF CROSSTABLE MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     size Dimension of the table.
*/
com::UnOrderedCrossTable::UnOrderedCrossTable(size_t size)

  : CountedObject<UnOrderedCrossTable>(),
    d_size(size), d_length(length(size)), d_cells(new size_t[d_length])

{
  std::fill(d_cells, d_cells + d_length, 0);

  createIndices(size);

#ifdef DEBUG_DEVELOP
  PRECOND(d_indicesMap.find(size) != d_indicesMap.end());
#endif

  d_indices = d_indicesMap[size];
}



//! Copy constructor.
/*!
  \param     aTable Cross table to copy from.
*/
com::UnOrderedCrossTable::UnOrderedCrossTable(const UnOrderedCrossTable& aTable)

  : CountedObject<UnOrderedCrossTable>(),
    d_size(aTable.d_size), d_length(aTable.d_length),
    d_cells(new size_t[d_length]), d_indices(aTable.d_indices)

{
  std::copy(aTable.d_cells, aTable.d_cells + d_length, d_cells);
}



//! Destructor.
/*!
*/
com::UnOrderedCrossTable::~UnOrderedCrossTable()
{
  delete[] d_cells;

  if(nrObjectsCreated() == 1) {
    for(std::map<size_t, size_t**>::iterator it = d_indicesMap.begin();
         it != d_indicesMap.end(); ++it) {

      size_t size = it->first;
      size_t** indices = it->second;

      for(size_t i = 0; i < size; ++i) {
        delete[] indices[i];
      }

      delete[] indices;
    }

    d_indicesMap.clear();
  }
}



//! Assignment operator.
/*!
  \param     aTable Cross table to assign from.
  \return    A reference to *this.
  \exception .
  \warning   The dimension of \a aTable must match the dimension of *this.
*/
com::UnOrderedCrossTable& com::UnOrderedCrossTable::operator=(
                   const UnOrderedCrossTable& aTable)
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_size == aTable.d_size);
#endif

  if(this != &aTable) {
    std::copy(aTable.d_cells, aTable.d_cells + d_length, d_cells);
  }

  return *this;
}



//! Returns the number of cells in the cross table.
/*!
  \return    Number of cells.
  \sa        size()
*/
size_t com::UnOrderedCrossTable::nrCells() const
{
  return d_size * d_size;
}



//! Returns the size/dimension of the cross table.
/*!
  \return    Size.
  \sa        nrCells()
*/
size_t com::UnOrderedCrossTable::size() const
{
  return d_size;
}



//! Returns a pointer to the first cell.
/*!
  \return    Pointer to first cell.
  \warning   Make sure you know the internals of this class before using the
             pointer.
  \sa        begin(), end()
*/
const size_t* com::UnOrderedCrossTable::cells() const
{
  return d_cells;
}



//! Returns an iterator to the first cell.
/*!
  \return    iterator.
  \warning   Make sure you know the internals of this class before using the
             iterator.
  \sa        cells(), end()
*/
com::UnOrderedCrossTable::iterator com::UnOrderedCrossTable::begin()
{
  return d_cells;
}



//! Returns an iterator to the one-past-the-last cell.
/*!
  \return    iterator.
  \warning   Make sure you know the internals of this class before using the
             iterator.
  \sa        cells(), begin()
*/
com::UnOrderedCrossTable::iterator com::UnOrderedCrossTable::end()
{
  return d_cells + d_length;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Writes a table to a stream.
/*!
  \param     stream Stream for table information.
  \param     table Table to write.
  \return    The output stream.
*/
std::ostream& com::operator<<(std::ostream& stream,
                   const UnOrderedCrossTable& table)
{
  for(size_t r = 0; r < table.size(); ++r) {
    for(size_t c = r; c < table.size() - 1; ++c) {
      stream << table.cell(r, c) << ' ';
    }

    stream << table.cell(r, table.size() - 1) << '\n';
  }

  return stream;
}



//! Read a table from a stream.
/*!
  \param     stream Stream with table information.
  \param     table Table to format.
  \return    The input stream.
  \exception com::FileFormatError If the table in \a stream is badly formatted.
  \warning   \a table must have the right size for the table in \a stream.
  \bug       Don't skip the newline. Check if the right amount of information is present on each line.
*/
std::istream& com::operator>>(std::istream& stream, UnOrderedCrossTable& table)
{
  for(size_t i = 0; i < table.d_length; ++i) {
    stream >> table.d_cells[i];
  }

  if(!stream)
    throw com::Exception("Cell value: Wrong format");

  return stream;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



