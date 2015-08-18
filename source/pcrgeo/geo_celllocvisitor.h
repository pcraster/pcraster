#ifndef INCLUDED_GEO_CELLLOCVISITOR
#define INCLUDED_GEO_CELLLOCVISITOR

#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif

namespace geo {


//! Sort of iterator to visit all cells of a raster
/*! The iterator visit all cells in the same order
    as the following code would do
    \code
     for(size_t row=0; row < nrRows; row++)
      for(size_t col=0; col < nrCols; col++)
        do something
    \endcode
    With CellLocVisitor we may write:
    \code
    for(CellLocVisitor c(nrRows,nrCols); c.valid(); ++c)
        do something
    \endcode
   Design ideas:
   - Contains a lot of inline members; do not generate
     overhead over classic double for-loop.
   - Passing CellLoc as a const ref will spare stack space;
     1 pointer instead of two int's for row and col

 */
class CellLocVisitor {
  RasterDim  d_rd;
  CellLoc    d_currentCell;

public:
  //! Initialize with raster dimension to first cell
  /*! Note that an empty raster is not allowed; both \a
      nrRows and \a nrCols must be > 0
  */
  CellLocVisitor(size_t nrRows, size_t nrCols);

  CellLocVisitor(const RasterDim& rd);

  //! advance to next cell
  void operator++() {
   if (d_currentCell.nextCol() == d_rd.nrCols()) {
    d_currentCell.setCol(0);
    d_currentCell.nextRow();
   }
  };

  //! is current cell valid
  /*! if false then all cells are done
   */
  bool valid() const {
    return d_currentCell.row() < d_rd.nrRows();
  };

  //! return the current cell
  const CellLoc& operator*() const {
    return d_currentCell;
  };

  //! return the row id of the current cell
  size_t row() const {
    return d_currentCell.row();
  };
  //! return the col id of the current cell
  size_t col() const {
    return d_currentCell.col();
  };

  CellLoc downstream(unsigned int lddVal) const;

  bool    downstream(CellLoc& cl, unsigned int lddVal) const;
};

} // namespace geo

#endif
