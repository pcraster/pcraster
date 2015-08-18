#ifndef INCLUDED_GEO_RASTERDIM
#define INCLUDED_GEO_RASTERDIM



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

// Module headers.



namespace geo {
  // RasterDim declarations.
}



namespace geo {



//! Raster dimension expressed as number of rows and columns
class RasterDim
{
  size_t           d_nrRows;
  size_t           d_nrCols;


private:

  // Assignment operator. DEFAULT
  // RasterDim&           operator=           (const RasterDim& rhs);

  // Copy constructor. DEFAULT
  //               RasterDim               (const RasterDim& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RasterDim               ();

                   RasterDim               (size_t nrRows, size_t nrCols);

  /* virtual */    ~RasterDim              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setNrRows           (size_t nrRows);
  void             setNrCols           (size_t nrCols);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool             valid               () const;
  size_t           row                 (LinearLoc index) const;
  size_t           col                 (LinearLoc index) const;
  size_t           nrRows              () const;
  size_t           nrCols              () const;
  size_t           nrCells             () const;
  bool             contains            (size_t r, size_t c) const;
  bool             contains            (LinearLoc        l) const;
  bool             contains            (const CellLoc&   c) const;
  bool             intContains         (int    r, int    c) const;

  CellLoc          convert             (LinearLoc      l) const;
  LinearLoc        convert             (const CellLoc& c) const;

  LinearLoc        convert             (size_t row,
                                        size_t col) const;

 //! transform a raster-linear nr to a row and col nrr
 /*! a raster-linear nr is the cell nr if all rows put after each other
  */
  void linear2RowCol( size_t linear, size_t &row, size_t &col) const;

  bool             operator==          (const RasterDim& rhs) const;
  bool             operator!=          (const RasterDim& rhs) const;

  template<typename T>
   LinearLoc       target              (LinearLoc from, typename T::Code dir) const;
  template<typename T>
   CellLoc         target              (const CellLoc& from, typename T::Code dir) const;
};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! default ctor leaving object in a invalid state (0,0), see valid()
inline geo::RasterDim::RasterDim():
  d_nrRows(0),d_nrCols(0)
{
}

//! ctor
/*!
 * \todo
 *  do we want this as precondition:
 *   nrRows != 0, nrCols != 0
 */
inline geo::RasterDim::RasterDim(size_t nrRows, size_t nrCols):
  d_nrRows(nrRows),d_nrCols(nrCols)
{
//  DEVELOP_POSTCOND(nrRows && nrCols);
}


inline RasterDim::~RasterDim()
{
}

//! set value of nrRows
inline void RasterDim::setNrRows(size_t nrRows)
{
  d_nrRows=nrRows;
}

//! set value of nrCols
inline void RasterDim::setNrCols(size_t nrCols)
{
  d_nrCols=nrCols;
}

//! are both nrRows and nrCols non zero?
inline bool RasterDim::valid() const
{
  return d_nrRows!=0 && d_nrCols!=0;
}

inline size_t RasterDim::nrCols() const
{ return d_nrCols; }

inline size_t RasterDim::nrRows() const
{ return d_nrRows; }

inline size_t RasterDim::nrCells() const
{ return d_nrCols * d_nrRows; }

//! Returns the row corresponding to index \a index.
/*!
  \param     index Location in 1 dimensional array of values.
  \return    row.
  \sa        col(LinearLoc)
*/
inline size_t RasterDim::row(LinearLoc index) const
{
  return index / nrCols();
}

//! Returns the col corresponding to index \a index.
/*!
  \param     index Location in 1 dimensional array of values.
  \return    col.
  \sa        row(LinearLoc)
*/
inline size_t RasterDim::col(LinearLoc index) const
{
  return index % nrCols();
}


inline bool RasterDim::contains(size_t r, size_t c) const
{
  return c < d_nrCols && r < d_nrRows;
}

inline bool RasterDim::contains(const CellLoc&   c) const
{
  return contains(c.row(),c.col());
}


inline bool RasterDim::contains(LinearLoc   l) const
{
  return l < nrCells();
}

inline bool RasterDim::intContains(int r, int c) const
{
  return r >= 0 && c >= 0 && contains(r,c);
}

//! No check on out of bound, since l may model (past-)end iterator
inline CellLoc RasterDim::convert(LinearLoc l) const
{
  return CellLoc(row(l),col(l));
}

inline LinearLoc RasterDim::convert(const CellLoc& c) const
{
  return convert(c.row(), c.col());
}

inline LinearLoc RasterDim::convert(size_t row, size_t col) const
{
  return row * d_nrCols + col;
}

/*!
 * \todo
 *   not according to style guide! dest,src
 */
inline void geo::RasterDim::linear2RowCol(
   size_t linear,
  size_t &row, size_t &col) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(linear < nrCells());
#endif
  CellLoc c = convert(linear);
  row = c.row();
  col = c.col();
}

inline bool RasterDim::operator==(const RasterDim& rhs) const
{
  return d_nrRows==rhs.d_nrRows
      && d_nrCols==rhs.d_nrCols;
}

inline bool RasterDim::operator!=(const RasterDim& rhs) const
{
  return d_nrRows!=rhs.d_nrRows
      || d_nrCols!=rhs.d_nrCols;
}

template<typename T>
 LinearLoc RasterDim::target(LinearLoc from, typename T::Code dir ) const
{
  return T::target(from,dir,nrCells(),nrCols());
}

template<typename T>
 CellLoc RasterDim::target(const CellLoc& from, typename T::Code dir) const
{
  return T::target(from,dir);
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
