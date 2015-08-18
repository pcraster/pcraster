#ifndef INCLUDED_DISCR_RASTERDATA
#define INCLUDED_DISCR_RASTERDATA



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_BOOST_TYPE_TRAITS
#include <boost/type_traits.hpp>
#define INCLUDED_BOOST_TYPE_TRAITS
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.
#ifndef INCLUDED_DISCR_RASTER
#include "discr_raster.h"
#define INCLUDED_DISCR_RASTER
#endif



namespace discr {
  // RasterData declarations.
}



namespace discr {



//! Class for raster data objects.
/*!
  ValueType can be a class type instead of a fundamental type.

  \warning   Data objects rely on discretisation information for their spatial
             properties. This information is passed into the constructors
             as a pointer. Make sure the object pointed to exists as long as
             there are data objects.
*/
template<class ValueType>
class RasterData
{

  friend class RasterDataTest;

private:

  //! Iterator type.
  typedef ValueType* iterator;

  //! Discretisation information.
  Raster const*    d_raster;

  //! Array with cell values.
  ValueType*       d_cells;

  iterator         begin               ();

  iterator         end                 ();

  bool             isMV                (size_t index,
                                        boost::false_type const&) const;

  bool             isMV                (size_t index,
                                        boost::true_type const&) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RasterData          (Raster const* raster);

                   RasterData          (Raster const* raster,
                                        ValueType const& value);

                   RasterData          (Raster const* raster,
                                        ValueType const* values);

                   RasterData          (RasterData const& rhs);

  /* virtual */    ~RasterData         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  RasterData&      operator=           (ValueType rhs);

  RasterData&      operator=           (RasterData const& rhs);

  void             setAllMV            ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             equals              (RasterData<ValueType> const& rhs) const;

  Raster const*    raster              () const;

  bool             isMV                (size_t index) const;

  ValueType const& cell                (size_t index) const;

  ValueType&       cell                (size_t index);

  ValueType const* cells               () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     raster Discretisation object.

  The cells are initialised by a default constructed ValueType value.
*/
template<class ValueType>
inline RasterData<ValueType>::RasterData(
         Raster const* raster)
  : d_raster(raster),
    d_cells(new ValueType[d_raster->nrCells()])
{
  std::fill(begin(), end(), ValueType());
}

//! Constructor.
/*!
  \param     raster Discretisation object.
  \param     value Initial value to use for the cells.
*/
template<class ValueType>
inline RasterData<ValueType>::RasterData(
         Raster const* raster,
         ValueType const& value)
  : d_raster(raster),
    d_cells(new ValueType[d_raster->nrCells()])
{
  std::fill(begin(), end(), value);
}

template<class ValueType>
inline RasterData<ValueType>::RasterData(
         Raster const* raster,
         ValueType const* values)
  : d_raster(raster),
    d_cells(new ValueType[d_raster->nrCells()])
{
  // func::assign(d_cells, value, d_raster->nrCells());
  std::memcpy(static_cast<void*>(d_cells),
         static_cast<void const*>(values),
         d_raster->nrCells() * sizeof(ValueType));
}

template<class ValueType>
inline RasterData<ValueType>::RasterData(
         RasterData const& rhs)
  : d_raster(rhs.d_raster),
    d_cells(new ValueType[d_raster->nrCells()])
{
  std::memcpy(static_cast<void*>(d_cells),
         static_cast<void const*>(rhs.d_cells),
         d_raster->nrCells() * sizeof(ValueType));
}

//! Destructor.
/*!
  The discretisation information is for use only.

  The cells are deleted here.
*/
template<class ValueType>
inline RasterData<ValueType>::~RasterData()
{
  delete[] d_cells;
}

template<class ValueType>
inline RasterData<ValueType>& RasterData<ValueType>::operator=(
         ValueType rhs)
{
  if(pcr::isMV(rhs)) {
    setAllMV();
  }
  else {
    std::fill(begin(), end(), rhs);
  }

  return *this;
}

template<class ValueType>
inline RasterData<ValueType>& RasterData<ValueType>::operator=(
         RasterData const& rhs)
{
  if(this != &rhs) {
    std::memcpy(static_cast<void*>(d_cells),
           static_cast<void const*>(rhs.d_cells),
           d_raster->nrCells() * sizeof(ValueType));
  }

  return *this;
}

template<class ValueType>
inline void RasterData<ValueType>::setAllMV()
{
  pcr::setMV(d_cells, d_raster->nrCells());
}

//! Returns whether rhs equals *this.
/*!
  \param     rhs Object to compare with.
  \return    true or false
*/
template<class ValueType>
bool RasterData<ValueType>::equals(
         RasterData<ValueType> const& rhs) const
{
  bool result = false;

  if(d_raster == rhs.d_raster || *d_raster == *rhs.d_raster) {
    size_t i;

    for(i = 0; i < d_raster->nrCells(); ++i) {
      if(isMV(i)) {
        // Both MV's?
        if(!rhs.isMV(i)) {
          // No.
          break;
        }
      }
      else {
        // Both values equal?
        if(rhs.isMV(i) || cell(i) != rhs.cell(i)) {
          // No.
          break;
        }
      }
    }

    // True when loop finished completely.
    result = i == d_raster->nrCells();
  }

  return result;
}

//! Returns the layered discretisation information.
/*!
  \return    discretisation
*/
template<class ValueType>
Raster const* RasterData<ValueType>::raster() const
{
  return d_raster;
}

//! Returns an iterator to the first cell.
/*!
  \return    iterator

  The first cell is the most north western cell (row 0, col 0, index 0).
*/
template<class ValueType>
inline typename RasterData<ValueType>::iterator RasterData<ValueType>::begin()
{
  return d_cells;
}

//! Returns the iterator to the one-past-the-last cell.
/*!
  \return    iterator

  The last cell is the most south eastern cell (row nrRows - 1, col nrCols - 1,
  index nrCells - 1).
*/
template<class ValueType>
inline typename RasterData<ValueType>::iterator RasterData<ValueType>::end()
{
  return d_cells + d_raster->nrCells();
}

/*!
  \overload

  Returns the result of calling cell(index).isMV().
*/
template<class ValueType>
inline bool RasterData<ValueType>::isMV(
         size_t index,
         boost::false_type const&) const
//          boost::integral_constant<bool, b> const&) const
{
  return cell(index).isMV();
}

/*!
  \overload

  Returns the result of calling pcr::isMV(ValueType const&).
*/
template<class ValueType>
inline bool RasterData<ValueType>::isMV(
         size_t index,
         boost::true_type const&) const
{
  return pcr::isMV(cell(index));
}

//! Returns whether cell \a index contains a missing value.
/*!
  \param     index Index of cell to check.
  \return    true or false

  For arithmetic values isMV(size_t, boost::true_type) is called,
  isMV(size_t, boost::false_type) in all other cases.
*/
template<class ValueType>
inline bool RasterData<ValueType>::isMV(
         size_t index) const
{
  return isMV(index, boost::is_arithmetic<ValueType>());
}

//! Returns the value of cell \a index.
/*!
  \param     index Index of cell to return value for.
  \return    value
*/
template<class ValueType>
inline ValueType const& RasterData<ValueType>::cell(
         size_t index) const
{
  DEVELOP_PRECOND(index < d_raster->nrCells());

  return d_cells[index];
}

//! Returns the value of cell \a index.
/*!
  \param     index Index of cell to return value for.
  \return    value
*/
template<class ValueType>
inline ValueType& RasterData<ValueType>::cell(
         size_t index)
{
  DEVELOP_PRECOND(index < d_raster->nrCells());

  return d_cells[index];
}



//! Returns the array of cells.
/*!
  \return    array
*/
template<class ValueType>
inline ValueType const* RasterData<ValueType>::cells() const
{
  return d_cells;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

//! Returns whether \a lhs is equal to \a rhs.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
template<class ValueType>
inline bool operator==(
         RasterData<ValueType> const& lhs,
         RasterData<ValueType> const& rhs)
{
  return lhs.equals(rhs);
}



//! Returns whether \a lhs is not equal to \a rhs.
/*!
  \param     lhs Object to compare.
  \param     rhs Object to compare.
  \return    true or false
*/
template<class ValueType>
inline bool operator!=(
         RasterData<ValueType> const& lhs,
         RasterData<ValueType> const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace discr

#endif
