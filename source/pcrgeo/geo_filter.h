#ifndef INCLUDED_GEO_FILTER
#define INCLUDED_GEO_FILTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif



namespace geo {
  // Filter declarations.
}



namespace geo {



//! Filter base class.
/*!
  A filter calculates an new value for a cell in a raster based on surrounding
  cell. Examples of filters are the high and low pass filters, min and max
  filters, etc. Filters are also called kernels.

  This class encapsulates the logic needed to select valid cells from the
  source raster in case the kernel is positioned in the corners or sides of
  the raster. In these cases part of the kernel lies outside the raster.

  For simple kernels, only the result(const SimpleRaster<SrcType>&, size_t,
  size_t, size_t, size_t) function needs to be implemented. The arguments of
  this function mark the area with valid cells from the source raster.

  The calc.. functions garantee that if the center value of the kernel has a
  missing value in the source raster, a missing value is returned for the
  destination raster. They also garantee that the result() function is only
  called when this center value in the source raster doesn't have a missing
  value.

  \sa FilterEngine
*/
template<class SrcType, class DstType>
class Filter: public SimpleRaster<double>
{

private:

  //! Radius.
  size_t           d_radius;

  //! Missing value.
  DstType          d_mv;

protected:

  inline           Filter              (const SimpleRaster<double>& weights);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~Filter             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  inline size_t    radius              () const;

  inline DstType   mv                  () const;

  virtual DstType  result              (const SimpleRaster<SrcType>& source,
                                        size_t rowSrc,
                                        size_t colSrc,
                                        size_t rowFlt,
                                        size_t colFlt,
                                        size_t nrRows,
                                        size_t nrCols) const;

  virtual DstType  calcUL              (const SimpleRaster<SrcType>& source,
                                        size_t row,
                                        size_t col) const;

  virtual DstType  calcUR              (const SimpleRaster<SrcType>& source,
                                        size_t row,
                                        size_t col) const;

  virtual DstType  calcLR              (const SimpleRaster<SrcType>& source,
                                        size_t row,
                                        size_t col) const;

  virtual DstType  calcLL              (const SimpleRaster<SrcType>& source,
                                        size_t row,
                                        size_t col) const;

  virtual DstType  calcTop             (const SimpleRaster<SrcType>& source,
                                        size_t row,
                                        size_t col) const;

  virtual DstType  calcBottom          (const SimpleRaster<SrcType>& source,
                                        size_t row,
                                        size_t col) const;

  virtual DstType  calcLeft            (const SimpleRaster<SrcType>& source,
                                        size_t row,
                                        size_t col) const;

  virtual DstType  calcRight           (const SimpleRaster<SrcType>& source,
                                        size_t row,
                                        size_t col) const;

  virtual DstType  calcInterior        (const SimpleRaster<SrcType>& source,
                                        size_t row,
                                        size_t col) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     weights Raster with weights to use.
  \warning   The width and height of the filter (size of \a weights) must be
             odd. The width and height of the filter must be equal. Don't
             destroy \a weights while we have it.
*/
template<class SrcType, class DstType>
Filter<SrcType, DstType>::Filter(const SimpleRaster<double>& weights)

  : SimpleRaster<double>(weights)

{
  DEVELOP_PRECOND(nrRows() % 2 == 1);
  DEVELOP_PRECOND(nrCols() % 2 == 1);
  DEVELOP_PRECOND(nrRows() == nrCols());

#ifdef DEBUG_DEVELOP
  // Should be possible with find_if but it has trouble finding the right isMV.
  // PRECOND(std::find_if(begin(), end(), pcr::isMV) == end());
  for(SimpleRaster<double>::const_iterator it = begin(); it != end(); ++it) {
    PRECOND(!pcr::isMV(*it));
  }
#endif

  d_radius = (nrCols() - 1) / 2;
  pcr::setMV(d_mv);
}

//! Destructor.
/*!
*/
template<class SrcType, class DstType>
Filter<SrcType, DstType>::~Filter()
{
}

//! Returns the radius of the filter.
template<class SrcType, class DstType>
size_t Filter<SrcType, DstType>::radius() const
{
  return d_radius;
}

//! Returns a missing value in the destination's value type.
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::mv() const
{
  return d_mv;
}

//! Calculates a result value for a group of cells from \a source.
/*!
  \param     source Raster to pick values from.
  \param     rowSrc Upper row in source raster of group.
  \param     colSrc Left column in source raster of group.
  \param     rowFlt Upper row in filter of group.
  \param     colFlt Left column in filter of group.
  \param     nrRows Number of rows in group.
  \param     nrCols Number of cols in group.
  \return    Result of calculation.
  \warning   It is assumed that the cell for which a value is computed has not
             a missing value in the source raster. So at least one cell in the
             group of cells to analyse is not a missing value.

  Override this function for your filter.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::result(const SimpleRaster<SrcType>& source,
         size_t rowSrc, size_t colSrc, size_t /* rowFlt */, size_t /* colFlt */,
         size_t /* nrRows */, size_t /* nrCols */) const
{
  return static_cast<DstType>(source.cell(rowSrc, colSrc));
}

//! Calculates a result for a cell in the upper left corner of the source.
/*!
  \param     source Source raster.
  \return    Result value.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::calcUL(
         const SimpleRaster<SrcType>& source,
         size_t row,
         size_t col) const
{
  if(pcr::isMV(source.cell(row, col))) {
    return d_mv;
  }
  else {
    size_t nrRows = std::min(row + 1 + radius(), source.nrRows());
    size_t nrCols = std::min(col + 1 + radius(), source.nrCols());

    size_t rowFlt = radius() - row;
    size_t colFlt = radius() - col;

    row = 0;
    col = 0;

    DEVELOP_POSTCOND(row < source.nrRows());
    DEVELOP_POSTCOND(col < source.nrCols());
    DEVELOP_POSTCOND(row + nrRows <= source.nrRows());
    DEVELOP_POSTCOND(col + nrCols <= source.nrCols());
    DEVELOP_POSTCOND(rowFlt + nrRows <= this->nrRows());
    DEVELOP_POSTCOND(colFlt + nrCols <= this->nrRows());

    return result(source, row, col, rowFlt, colFlt, nrRows, nrCols);
  }
}

//! Calculates a result for a cell in the upper right corner of the source.
/*!
  \param     source Source raster.
  \return    Result value.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::calcUR(const SimpleRaster<SrcType>& source,
         size_t row, size_t col) const
{
  if(pcr::isMV(source.cell(row, col))) {
    return d_mv;
  }
  else {
    size_t nrRows = std::min(row + 1 + radius(), source.nrRows());
    size_t nrCols = std::min(radius() + source.nrCols() - col, source.nrCols());

    size_t rowFlt = radius() - row;
    size_t colFlt = 0;

    row = 0;
    col = col > radius() ? col - radius() : 0;

    DEVELOP_POSTCOND(row < source.nrRows());
    DEVELOP_POSTCOND(col < source.nrCols());
    DEVELOP_POSTCOND(row + nrRows <= source.nrRows());
    DEVELOP_POSTCOND(col + nrCols <= source.nrCols());
    DEVELOP_POSTCOND(rowFlt + nrRows <= this->nrRows());
    DEVELOP_POSTCOND(colFlt + nrCols <= this->nrRows());

    return result(source, row, col, rowFlt, colFlt, nrRows, nrCols);
  }
}

//! Calculates a result for a cell in the lower right corner of the source.
/*!
  \param     source Source raster.
  \return    Result value.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::calcLR(const SimpleRaster<SrcType>& source,
         size_t row, size_t col) const
{
  if(pcr::isMV(source.cell(row, col))) {
    return d_mv;
  }
  else {
    size_t nrRows = std::min(radius() + source.nrRows() - row, source.nrRows());
    size_t nrCols = std::min(radius() + source.nrCols() - col, source.nrCols());

    size_t rowFlt = 0;
    size_t colFlt = 0;

    row = row > radius() ? row - radius() : 0;
    col = col > radius() ? col - radius() : 0;

    DEVELOP_POSTCOND(row < source.nrRows());
    DEVELOP_POSTCOND(col < source.nrCols());
    DEVELOP_POSTCOND(row + nrRows <= source.nrRows());
    DEVELOP_POSTCOND(col + nrCols <= source.nrCols());
    DEVELOP_POSTCOND(rowFlt + nrRows <= this->nrRows());
    DEVELOP_POSTCOND(colFlt + nrCols <= this->nrRows());

    return result(source, row, col, rowFlt, colFlt, nrRows, nrCols);
  }
}

//! Calculates a result for a cell in the lower left corner of the source.
/*!
  \param     source Source raster.
  \return    Result value.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::calcLL(const SimpleRaster<SrcType>& source,
         size_t row, size_t col) const
{
  if(pcr::isMV(source.cell(row, col))) {
    return d_mv;
  }
  else {
    size_t nrRows = std::min(radius() + source.nrRows() - row, source.nrRows());
    size_t nrCols = std::min(col + 1 + radius(), source.nrCols());

    size_t rowFlt = 0;
    size_t colFlt = radius() - col;

    row = row > radius() ? row - radius() : 0;
    col = 0;

    DEVELOP_POSTCOND(row < source.nrRows());
    DEVELOP_POSTCOND(col < source.nrCols());
    DEVELOP_POSTCOND(row + nrRows <= source.nrRows());
    DEVELOP_POSTCOND(col + nrCols <= source.nrCols());
    DEVELOP_POSTCOND(rowFlt + nrRows <= this->nrRows());
    DEVELOP_POSTCOND(colFlt + nrCols <= this->nrRows());

    return result(source, row, col, rowFlt, colFlt, nrRows, nrCols);
  }
}

//! Calculates a result for a cell in the top row of the source.
/*!
  \param     source Source raster.
  \return    Result value.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::calcTop(const SimpleRaster<SrcType>& source,
         size_t row, size_t col) const
{
  if(pcr::isMV(source.cell(row, col))) {
    return d_mv;
  }
  else {
    size_t nrRows = std::min(row + 1 + radius(), source.nrRows());
    size_t nrCols = this->nrCols();

    size_t rowFlt = radius() - row;
    size_t colFlt = 0;

    row = 0;
    col = col > radius() ? col - radius() : 0;

    DEVELOP_POSTCOND(row < source.nrRows());
    DEVELOP_POSTCOND(col < source.nrCols());
    DEVELOP_POSTCOND(row + nrRows <= source.nrRows());
    DEVELOP_POSTCOND(col + nrCols <= source.nrCols());
    DEVELOP_POSTCOND(rowFlt + nrRows <= this->nrRows());
    DEVELOP_POSTCOND(colFlt + nrCols <= this->nrRows());

    return result(source, row, col, rowFlt, colFlt, nrRows, nrCols);
  }
}

//! Calculates a result for a cell in the bottom row of the source.
/*!
  \param     source Source raster.
  \return    Result value.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::calcBottom(const SimpleRaster<SrcType>&
         source, size_t row, size_t col) const
{
  if(pcr::isMV(source.cell(row, col))) {
    return d_mv;
  }
  else {
    size_t nrRows = std::min(radius() + source.nrRows() - row, source.nrRows());
    size_t nrCols = this->nrCols();

    size_t rowFlt = 0;
    size_t colFlt = 0;

    row = row > radius() ? row - radius() : 0;
    col = col > radius() ? col - radius() : 0;

    DEVELOP_POSTCOND(row < source.nrRows());
    DEVELOP_POSTCOND(col < source.nrCols());
    DEVELOP_POSTCOND(row + nrRows <= source.nrRows());
    DEVELOP_POSTCOND(col + nrCols <= source.nrCols());
    DEVELOP_POSTCOND(rowFlt + nrRows <= this->nrRows());
    DEVELOP_POSTCOND(colFlt + nrCols <= this->nrRows());

    return result(source, row, col, rowFlt, colFlt, nrRows, nrCols);
  }
}

//! Calculates a result for a cell in the left collumn of the source.
/*!
  \param     source Source raster.
  \return    Result value.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::calcLeft(const SimpleRaster<SrcType>& source,
         size_t row, size_t col) const
{
  if(pcr::isMV(source.cell(row, col))) {
    return d_mv;
  }
  else {
    size_t nrRows = this->nrRows();
    size_t nrCols = std::min(col + 1 + radius(), source.nrCols());

    size_t colFlt = radius() - col;
    size_t rowFlt = 0;

    row = row > radius() ? row - radius() : 0;
    col = 0;

    DEVELOP_POSTCOND(row < source.nrRows());
    DEVELOP_POSTCOND(col < source.nrCols());
    DEVELOP_POSTCOND(row + nrRows <= source.nrRows());
    DEVELOP_POSTCOND(col + nrCols <= source.nrCols());
    DEVELOP_POSTCOND(rowFlt + nrRows <= this->nrRows());
    DEVELOP_POSTCOND(colFlt + nrCols <= this->nrRows());

    return result(source, row, col, rowFlt, colFlt, nrRows, nrCols);
  }
}

//! Calculates a result for a cell in the right collumn of the source.
/*!
  \param     source Source raster.
  \return    Result value.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::calcRight(const SimpleRaster<SrcType>& source,
         size_t row, size_t col) const
{
  if(pcr::isMV(source.cell(row, col))) {
    return d_mv;
  }
  else {
    size_t nrRows = this->nrRows();
    size_t nrCols = std::min(radius() + source.nrCols() - col, source.nrCols());

    size_t rowFlt = 0;
    size_t colFlt = 0;

    row = row > radius() ? row - radius() : 0;
    col = col > radius() ? col - radius() : 0;

    DEVELOP_POSTCOND(row < source.nrRows());
    DEVELOP_POSTCOND(col < source.nrCols());
    DEVELOP_POSTCOND(row + nrRows <= source.nrRows());
    DEVELOP_POSTCOND(col + nrCols <= source.nrCols());
    DEVELOP_POSTCOND(rowFlt + nrRows <= this->nrRows());
    DEVELOP_POSTCOND(colFlt + nrCols <= this->nrRows());

    return result(source, row, col, rowFlt, colFlt, nrRows, nrCols);
  }
}

//! Calculates a result for a cell in the interior of the source.
/*!
  \param     source Source raster.
  \return    Result value.
*/
template<class SrcType, class DstType>
DstType Filter<SrcType, DstType>::calcInterior(
         const SimpleRaster<SrcType>& source, size_t row, size_t col) const
{
  if(pcr::isMV(source.cell(row, col))) {
    return d_mv;
  }
  else {
    size_t nrRows = this->nrRows();
    size_t nrCols = this->nrCols();

    size_t rowFlt = 0;
    size_t colFlt = 0;

    row = row > radius() ? row - radius() : 0;
    col = col > radius() ? col - radius() : 0;

    DEVELOP_POSTCOND(row < source.nrRows());
    DEVELOP_POSTCOND(col < source.nrCols());
    DEVELOP_POSTCOND(row + nrRows <= source.nrRows());
    DEVELOP_POSTCOND(col + nrCols <= source.nrCols());
    DEVELOP_POSTCOND(rowFlt + nrRows <= this->nrRows());
    DEVELOP_POSTCOND(colFlt + nrCols <= this->nrRows());

    return result(source, row, col, rowFlt, colFlt, nrRows, nrCols);
  }
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
