#ifndef INCLUDED_COM_UNORDEREDCROSSTABLE
#define INCLUDED_COM_UNORDEREDCROSSTABLE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_COUNTEDOBJECT
#include "com_countedobject.h"
#define INCLUDED_COM_COUNTEDOBJECT
#endif



namespace com {
  // UnOrderedCrossTable declarations.
}



namespace com {



//! The UnOrderedCrossTable class is for cross table objects.
/*!
  A normal cross table represents a square matrix with values for each
  combination of cell indices. For example, a 3x3 cross table is a 2D data
  structure with 9 cells (9 combinations of cell indices).

  The UnOrderedCrossTable class represents a cross table in case the order of
  the cell indices doesn't matter. So cell 1, 2 is equivalent to cell 2, 1 so
  part of the matrix can be discarded.

  Because of this feature of unordered cross tables, the whole table can be
  represented by a 1D array of cell values. This is more memory efficient.
  Downside of this implementation is that a translation needs to be made from
  2D cross table coordinates (row and column indices) to 1D array coordinates.
  For each cross table with a different size this is done only once though.

  As a user of this class you probably want to use the cell(size_t, size_t)
  member to access the cell values of the table. If you are interested in cell
  values only (not in their position in the table) you can use the cell(size_t)
  member which doesn't need to translate from 2D to 1D cell indices.
*/
class UnOrderedCrossTable: public CountedObject<UnOrderedCrossTable>
{

private:

  //! Iterator to cell values.
  typedef size_t* iterator;

  //! Map which relates table sizes to indices for the d_cells array.
  static std::map<size_t, size_t**> d_indicesMap;

  static size_t    calculateIndex      (size_t size,
                                        size_t r,
                                        size_t c);

  size_t           length              (size_t size) const;

  //! Size of the cross table.
  size_t           d_size;

  //! Length of array to store the values.
  size_t           d_length;

  //! Values in the cross table.
  size_t*          d_cells;

  //! Indices to d_cells array.
  size_t**        d_indices;

  iterator         begin               ();

  iterator         end                 ();

protected:

  const size_t*    cells               () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   UnOrderedCrossTable (size_t size);

                   UnOrderedCrossTable (const UnOrderedCrossTable& aTable);

  virtual          ~UnOrderedCrossTable();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  UnOrderedCrossTable& operator=       (const UnOrderedCrossTable& aTable);

  friend std::istream& operator>>      (std::istream& stream,
                                        UnOrderedCrossTable& table);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  static void      createIndices       (size_t size);

  static size_t    index               (size_t size,
                                        size_t r,
                                        size_t c);

  size_t           nrCells             () const;

  size_t           size                () const;

  const size_t&    cell                (size_t i) const;

  size_t&          cell                (size_t i);

  const size_t&    cell                (size_t r,
                                        size_t c) const;

  size_t&          cell                (size_t r,
                                        size_t c);

  size_t           length              () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

//! Returns the cell value at position \a i.
/*!
  \param     i Index.
  \return    cell value.
  \warning   i < length().
  \sa        index(size_t, size_t)
*/
inline const size_t& UnOrderedCrossTable::cell(size_t i) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_indices);
  PRECOND(i < length());
#endif

  return d_cells[i];
}

//! Returns the cell value at position \a i.
/*!
  \param     i Index.
  \return    cell value.
  \warning   i < length().
  \sa        index(size_t, size_t)
*/
inline size_t& UnOrderedCrossTable::cell(size_t i)
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_indices);
  PRECOND(i < length());
#endif

  return d_cells[i];
}

//! Returns the cell value at position \a r, \a c.
/*!
  \param     r Row index.
  \param     c Column index.
  \warning   r < size() && c <, size().
  \return    cell value.
*/
inline const size_t& UnOrderedCrossTable::cell(size_t r, size_t c) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_indices);
#endif

  return d_cells[d_indices[r][c]];
}

//! Returns the cell value at position \a r, \a c.
/*!
  \param     r Row index.
  \param     c Column index.
  \warning   r < size() && c <, size().
  \return    cell value.
*/
inline size_t& UnOrderedCrossTable::cell(size_t r, size_t c)
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_indices);
#endif

  return d_cells[d_indices[r][c]];
}

//! Returns the length of the array which used to represent the cross table.
/*!
  \return    Length.
  \sa        size()
*/
inline size_t UnOrderedCrossTable::length() const
{
  return d_length;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

std::ostream&      operator<<          (std::ostream& stream,
                                        const UnOrderedCrossTable& table);

std::istream&      operator>>          (std::istream& stream,
                                        UnOrderedCrossTable& table);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
