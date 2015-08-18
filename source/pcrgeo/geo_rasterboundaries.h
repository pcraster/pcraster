#ifndef INCLUDED_GEO_RASTERBOUNDARIES
#define INCLUDED_GEO_RASTERBOUNDARIES



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_BINARYOPERATORS
#include "com_binaryoperators.h"
#define INCLUDED_COM_BINARYOPERATORS
#endif

#ifndef INCLUDED_COM_FUNCTIONS
#include "com_functions.h"
#define INCLUDED_COM_FUNCTIONS
#endif

// Module headers.
#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif



namespace geo {
  // RasterBoundaries declarations.
  template<class ValueType>
    class RasterBoundaries;
  template<class ValueType>
    RasterBoundaries<ValueType> operator+(
         const RasterBoundaries<ValueType>& lhs,
         const RasterBoundaries<ValueType>& rhs);
  template<class ValueType>
    RasterBoundaries<ValueType> operator-(
         const RasterBoundaries<ValueType>& lhs,
         const RasterBoundaries<ValueType>& rhs);
  template<class ValueType>
    RasterBoundaries<ValueType> operator/(
         const RasterBoundaries<ValueType>& lhs,
         const RasterBoundaries<ValueType>& rhs);
  template<class ValueType>
    RasterBoundaries<ValueType> operator/(
         const RasterBoundaries<ValueType>& lhs,
         const ValueType& rhs);
  template<class ValueType>
    RasterBoundaries<ValueType> operator*(
         const RasterBoundaries<ValueType>& lhs,
         const RasterBoundaries<ValueType>& rhs);
}



namespace geo {



//! Class for storing values which are positioned on raster cell boundaries.
/*!
  Normally raster cell values are positioned in the raster cell. This class
  is for rasters with values on the boundaries between adjacent raster
  cells.

  As an example of a situation where this class might be useful consider
  a field with velocity values in x-direction in each raster cell. When
  velocities are needed at the boundary of cells a RasterBoundaries object
  might be used to store the values. This is much more (memory) efficient
  than creating 4 normal rasters for storing values for each boundary of the
  raster cells. This is so because for example the value of the right
  boundary of a cell equals the value of the left boundary of the adjacent
  cell in the next column. This class takes that into account.

  Row and column indices: 0 <= row < nrRows and 0 <= col < nrCols.
*/
template<typename ValueType>
class RasterBoundaries
{

private:

  friend class RasterBoundariesTest;

  //! Iterator type.
  typedef ValueType* iterator;

  //! Number of rows.
  size_t           d_nrRows;

  //! Number of cols.
  size_t           d_nrCols;

  //! Number of values.
  size_t           d_nrValues;

  //! Raster values.
  ValueType*       d_values;

  void             copyValues          (const RasterBoundaries& rhs);

  size_t           indexLeft           (size_t row,
                                        size_t col) const;

  size_t           indexTop            (size_t row,
                                        size_t col) const;

  size_t           indexRight          (size_t row,
                                        size_t col) const;

  size_t           indexBottom         (size_t row,
                                        size_t col) const;

  size_t           nrValues            () const;

  iterator         begin               () const;

  iterator         end                 () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RasterBoundaries    (size_t nrRows,
                                        size_t nrCols);

                   RasterBoundaries    (const RasterBoundaries& rhs);

  virtual          ~RasterBoundaries   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  RasterBoundaries& operator=          (const RasterBoundaries& rhs);

  RasterBoundaries& operator*=         (const RasterBoundaries& rhs);

  RasterBoundaries& operator/=         (const RasterBoundaries& rhs);

  RasterBoundaries& operator/=         (const ValueType& rhs);

  RasterBoundaries& operator+=         (const RasterBoundaries& rhs);

  RasterBoundaries& operator-=         (const RasterBoundaries& rhs);

  friend RasterBoundaries operator*<>  (const RasterBoundaries& lhs,
                                        const RasterBoundaries& rhs);

  friend RasterBoundaries operator/<>  (const RasterBoundaries& lhs,
                                        const RasterBoundaries& rhs);

  friend RasterBoundaries operator/<>  (const RasterBoundaries& lhs,
                                        const ValueType& rhs);

  friend RasterBoundaries operator+<>  (const RasterBoundaries& lhs,
                                        const RasterBoundaries& rhs);

  friend RasterBoundaries operator-<>  (const RasterBoundaries& lhs,
                                        const RasterBoundaries& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  ValueType&       leftBoundary        (size_t row,
                                        size_t col);

  const ValueType& leftBoundary        (size_t row,
                                        size_t col) const;

  const ValueType& leftBoundary        (const CellLoc& loc) const;

  ValueType&       topBoundary         (size_t row,
                                        size_t col);

  const ValueType& topBoundary         (size_t row,
                                        size_t col) const;

  const ValueType& topBoundary         (const CellLoc& loc) const;

  ValueType&       rightBoundary       (size_t row,
                                        size_t col);

  const ValueType& rightBoundary       (size_t row,
                                        size_t col) const;

  const ValueType& rightBoundary       (const CellLoc& loc) const;

  ValueType&       bottomBoundary      (size_t row,
                                        size_t col);

  const ValueType& bottomBoundary      (size_t row,
                                        size_t col) const;

  const ValueType& bottomBoundary      (const CellLoc& loc) const;

  void             rightBoundary       (SimpleRaster<ValueType>& raster) const;

  void             bottomBoundary      (SimpleRaster<ValueType>& raster) const;

  ValueType        minimum             () const;

  ValueType        maximum             () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     nrRows Number of rows.
  \param     nrCols Number of cols.
*/
template<typename ValueType>
inline RasterBoundaries<ValueType>::RasterBoundaries(
         size_t nrRows, size_t nrCols)
  : d_nrRows(nrRows), d_nrCols(nrCols)
{
  PRECOND(nrRows && nrCols);

  d_nrValues = 2 * d_nrRows * d_nrCols + d_nrRows + d_nrCols;
  d_values = new ValueType[nrValues()];
}

//! Copy constructor.
template<typename ValueType>
inline RasterBoundaries<ValueType>::RasterBoundaries(
         const RasterBoundaries& rhs)
  : d_nrRows(rhs.nrRows()), d_nrCols(rhs.nrCols()), d_nrValues(rhs.nrValues())
{
  d_values = new ValueType[nrValues()];
  copyValues(rhs);
}



//! Destructor.
/*!
*/
template<typename ValueType>
inline RasterBoundaries<ValueType>::~RasterBoundaries()
{
  delete[] d_values;
}



template<typename ValueType>
inline void RasterBoundaries<ValueType>::copyValues(
         const RasterBoundaries& boundaries)
{
  PRECOND(nrRows() == boundaries.nrRows());
  PRECOND(nrCols() == boundaries.nrCols());

  std::memcpy(static_cast<void*>(d_values),
         static_cast<const void*>(boundaries.d_values),
         nrValues() * sizeof(ValueType));
}



template<typename ValueType>
inline RasterBoundaries<ValueType>& RasterBoundaries<ValueType>::operator=(
         const RasterBoundaries& rhs)
{
  PRECOND(nrRows() == rhs.nrRows());
  PRECOND(nrCols() == rhs.nrCols());

  if(this != &rhs) {
    copyValues(rhs);
  }

  return *this;
}



template<typename ValueType>
inline RasterBoundaries<ValueType>& RasterBoundaries<ValueType>::operator*=(
         const RasterBoundaries& rhs)
{
  com::multiplyByRange(begin(), end(), rhs.begin());
  return *this;
}



template<typename ValueType>
inline RasterBoundaries<ValueType>& RasterBoundaries<ValueType>::operator/=(
         const RasterBoundaries& rhs)
{
  com::divideByRange(begin(), end(), rhs.begin());
  return *this;
}



template<typename ValueType>
inline RasterBoundaries<ValueType>& RasterBoundaries<ValueType>::operator/=(
         const ValueType& rhs)
{
  com::divideByValue(begin(), end(), rhs);
  return *this;
}



template<typename ValueType>
inline RasterBoundaries<ValueType>& RasterBoundaries<ValueType>::operator+=(
         const RasterBoundaries& rhs)
{
  com::addRange(begin(), end(), rhs.begin());
  return *this;
}



template<typename ValueType>
inline RasterBoundaries<ValueType>& RasterBoundaries<ValueType>::operator-=(
         const RasterBoundaries& rhs)
{
  com::substractRange(begin(), end(), rhs.begin());
  return *this;
}



//! Return the number of rows of the raster.
/*!
  \return    Number of rows.
  \sa        nrCols()
*/
template<typename ValueType>
inline size_t RasterBoundaries<ValueType>::nrRows() const
{
  return d_nrRows;
}



//! Return the number of cols of the raster.
/*!
  \return    Number of rows.
  \sa        nrRows()
*/
template<typename ValueType>
inline size_t RasterBoundaries<ValueType>::nrCols() const
{
  return d_nrCols;
}



//! Returns the index of the left boundary value of cell \a row, \a col.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Index of boundary value.
  \sa        indexTop(size_t, size_t), indexRight(size_t, size_t),
             indexBottom(size_t, size_t)

  The index can be used to access the cell boundary value of cell \a row,
  \a col.
*/
template<typename ValueType>
size_t RasterBoundaries<ValueType>::indexLeft(size_t row, size_t col) const
{
  return 2 * row * d_nrCols + d_nrCols + row + col;
}



//! Returns the index of the top boundary value of cell \a row, \a col.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Index of boundary value.
  \sa        indexLeft(size_t, size_t), indexRight(size_t, size_t),
             indexBottom(size_t, size_t)

  The index can be used to access the cell boundary value of cell \a row,
  \a col.
*/
template<typename ValueType>
size_t RasterBoundaries<ValueType>::indexTop(size_t row, size_t col) const
{
  return 2 * row * d_nrCols + row + col;
}



//! Returns the index of the right boundary value of cell \a row, \a col.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Index of boundary value.
  \sa        indexLeft(size_t, size_t), indexTop(size_t, size_t),
             indexBottom(size_t, size_t)

  The index can be used to access the cell boundary value of cell \a row,
  \a col.
*/
template<typename ValueType>
size_t RasterBoundaries<ValueType>::indexRight(size_t row, size_t col) const
{
  return indexLeft(row, col) + 1;
}



//! Returns the index of the bottom boundary value of cell \a row, \a col.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Index of boundary value.
  \sa        indexLeft(size_t, size_t), indexTop(size_t, size_t),
             indexRight(size_t, size_t)

  The index can be used to access the cell boundary value of cell \a row,
  \a col.
*/
template<typename ValueType>
size_t RasterBoundaries<ValueType>::indexBottom(size_t row, size_t col) const
{
  return indexTop(row, col) + 2 * d_nrCols + 1;
}



template<typename ValueType>
size_t RasterBoundaries<ValueType>::nrValues() const
{
  return d_nrValues;
}



template<typename ValueType>
typename RasterBoundaries<ValueType>::iterator
         RasterBoundaries<ValueType>::begin() const
{
  return d_values;
}



template<typename ValueType>
typename RasterBoundaries<ValueType>::iterator
         RasterBoundaries<ValueType>::end() const
{
  return d_values + nrValues();
}



//! Returns the left cell boundary value.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Boundary value.
  \sa        topBoundary(size_t, size_t), rightBoundary(size_t, size_t),
             bottomBoundary(size_t, size_t)
*/
template<typename ValueType>
inline ValueType& RasterBoundaries<ValueType>::leftBoundary(
         size_t row, size_t col)
{
  return d_values[indexLeft(row, col)];
}



//! Returns the top cell boundary value.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Boundary value.
  \sa        leftBoundary(size_t, size_t), rightBoundary(size_t, size_t),
             bottomBoundary(size_t, size_t)
*/
template<typename ValueType>
inline ValueType& RasterBoundaries<ValueType>::topBoundary(
         size_t row, size_t col)
{
  return d_values[indexTop(row, col)];
}



//! Returns the right cell boundary value.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Boundary value.
  \sa        topBoundary(size_t, size_t), topBoundary(size_t, size_t),
             bottomBoundary(size_t, size_t)
*/
template<typename ValueType>
inline ValueType& RasterBoundaries<ValueType>::rightBoundary(
         size_t row, size_t col)
{
  return d_values[indexRight(row, col)];
}



//! Returns the bottom cell boundary value.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Boundary value.
  \sa        topBoundary(size_t, size_t), rightBoundary(size_t, size_t),
             leftBoundary(size_t, size_t)
*/
template<typename ValueType>
inline ValueType& RasterBoundaries<ValueType>::bottomBoundary(
         size_t row, size_t col)
{
  return d_values[indexBottom(row, col)];
}



//! Returns the left cell boundary value.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Boundary value.
  \sa        topBoundary(size_t, size_t), rightBoundary(size_t, size_t),
             bottomBoundary(size_t, size_t)
*/
template<typename ValueType>
inline const ValueType& RasterBoundaries<ValueType>::leftBoundary(
         size_t row, size_t col) const
{
  return d_values[indexLeft(row, col)];
}



template<typename ValueType>
inline const ValueType& RasterBoundaries<ValueType>::leftBoundary(
         const CellLoc& loc) const
{
  return d_values[indexLeft(loc.row(), loc.col())];
}



//! Returns the top cell boundary value.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Boundary value.
  \sa        leftBoundary(size_t, size_t), rightBoundary(size_t, size_t),
             bottomBoundary(size_t, size_t)
*/
template<typename ValueType>
inline const ValueType& RasterBoundaries<ValueType>::topBoundary(
         size_t row, size_t col) const
{
  return d_values[indexTop(row, col)];
}



template<typename ValueType>
inline const ValueType& RasterBoundaries<ValueType>::topBoundary(
         const CellLoc& loc) const
{
  return d_values[indexTop(loc.row(), loc.col())];
}



//! Returns the right cell boundary value.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Boundary value.
  \sa        topBoundary(size_t, size_t), topBoundary(size_t, size_t),
             bottomBoundary(size_t, size_t)
*/
template<typename ValueType>
inline const ValueType& RasterBoundaries<ValueType>::rightBoundary(
         size_t row, size_t col) const
{
  return d_values[indexRight(row, col)];
}



template<typename ValueType>
inline const ValueType& RasterBoundaries<ValueType>::rightBoundary(
         const CellLoc& loc) const
{
  return d_values[indexRight(loc.row(), loc.col())];
}



//! Returns the bottom cell boundary value.
/*!
  \param     row Row index.
  \param     col Col index.
  \return    Boundary value.
  \sa        topBoundary(size_t, size_t), rightBoundary(size_t, size_t),
             leftBoundary(size_t, size_t)
*/
template<typename ValueType>
inline const ValueType& RasterBoundaries<ValueType>::bottomBoundary(
         size_t row, size_t col) const
{
  return d_values[indexBottom(row, col)];
}



template<typename ValueType>
inline const ValueType& RasterBoundaries<ValueType>::bottomBoundary(
         const CellLoc& loc) const
{
  return d_values[indexBottom(loc.row(), loc.col())];
}



template<typename ValueType>
inline void RasterBoundaries<ValueType>::rightBoundary(
         SimpleRaster<ValueType>& raster) const
{
  for(CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    raster.cell(*loc) = rightBoundary(*loc);
  }
}



template<typename ValueType>
inline void RasterBoundaries<ValueType>::bottomBoundary(
         SimpleRaster<ValueType>& raster) const
{
  for(CellLocVisitor loc(nrRows(), nrCols()); loc.valid(); ++loc) {
    raster.cell(*loc) = bottomBoundary(*loc);
  }
}



template<typename ValueType>
inline ValueType RasterBoundaries<ValueType>::minimum() const
{
  return com::minimum(begin(), end());
}



template<typename ValueType>
inline ValueType RasterBoundaries<ValueType>::maximum() const
{
  return com::maximum(begin(), end());
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

template<typename ValueType>
inline RasterBoundaries<ValueType> operator*(
         const RasterBoundaries<ValueType>& lhs,
         const RasterBoundaries<ValueType>& rhs)
{
  PRECOND(lhs.nrRows() == rhs.nrRows());
  PRECOND(lhs.nrCols() == rhs.nrCols());

  RasterBoundaries<ValueType> boundaries(lhs);
  boundaries *= rhs;
  return boundaries;
}



template<typename ValueType>
inline RasterBoundaries<ValueType> operator/(
         const RasterBoundaries<ValueType>& lhs,
         const RasterBoundaries<ValueType>& rhs)
{
  PRECOND(lhs.nrRows() == rhs.nrRows());
  PRECOND(lhs.nrCols() == rhs.nrCols());

  RasterBoundaries<ValueType> boundaries(lhs);
  boundaries /= rhs;
  return boundaries;
}



template<typename ValueType>
inline RasterBoundaries<ValueType> operator/(
         const RasterBoundaries<ValueType>& lhs, const ValueType& rhs)
{
  RasterBoundaries<ValueType> boundaries(lhs);
  boundaries /= rhs;
  return boundaries;
}



template<typename ValueType>
inline RasterBoundaries<ValueType> operator+(
         const RasterBoundaries<ValueType>& lhs,
         const RasterBoundaries<ValueType>& rhs)
{
  PRECOND(lhs.nrRows() == rhs.nrRows());
  PRECOND(lhs.nrCols() == rhs.nrCols());

  RasterBoundaries<ValueType> boundaries(lhs);
  boundaries += rhs;
  return boundaries;
}



template<typename ValueType>
inline RasterBoundaries<ValueType> operator-(
         const RasterBoundaries<ValueType>& lhs,
         const RasterBoundaries<ValueType>& rhs)
{
  PRECOND(lhs.nrRows() == rhs.nrRows());
  PRECOND(lhs.nrCols() == rhs.nrCols());

  RasterBoundaries<ValueType> boundaries(lhs);
  boundaries -= rhs;
  return boundaries;
}



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
