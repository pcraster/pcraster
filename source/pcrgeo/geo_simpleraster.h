#ifndef INCLUDED_GEO_SIMPLERASTER
#define INCLUDED_GEO_SIMPLERASTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_LIST
#include <list>
#define INCLUDED_LIST
#endif

#ifndef INCLUDED_COM_BINARYOPERATORS
#include "com_binaryoperators.h"
#define INCLUDED_COM_BINARYOPERATORS
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_FUNCTIONS
#include "com_functions.h"
#define INCLUDED_COM_FUNCTIONS
#endif

#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif

#ifndef INCLUDED_GEO_DEF
#include "geo_def.h"
#define INCLUDED_GEO_DEF
#endif



namespace geo {
  template<class T>
    class SimpleRaster;
  template<class T>
    std::ostream& operator<<(std::ostream& s, const SimpleRaster<T>& r);
  template<class T>
    std::istream& operator>>(std::istream& s, SimpleRaster<T>& r);
}



namespace geo {

//! thrown if something can not be computed to an input missing value
struct InputMV {
};


//! The SimpleRaster class is a simple data structure for data in raster format.
/*!
  Data in raster format is referenced by a row and column number. The raster
  cell at the upper left position has row number 0 and column number 0. Row
  and column number range from [0, nr) and [0, nc) respectively.
*/
template<class T> class SimpleRaster : public RasterDim
{

private:
  //! Data values.
  T*               d_values;

  void             copy                (SimpleRaster<T> const& r);

  void             memCopy             (SimpleRaster<T> const& r);

public:

  //! Iterator type for the cell values.
  typedef T* iterator;

  //! Const iterator type for the cell values.
  typedef const T* const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SimpleRaster        (size_t nr,
                                        size_t nc);
                   SimpleRaster        (const RasterDim& rd);

                   SimpleRaster        (const RasterDim& rd,
                                        const T& v);
                   SimpleRaster        (size_t nr,
                                        size_t nc,
                                        const T& v);

                   SimpleRaster        (size_t nr,
                                        size_t nc,
                                        T* values);

                   SimpleRaster        (const RasterDim& rd,
                                        T* values);

                   SimpleRaster        (const SimpleRaster& r);

  virtual          ~SimpleRaster       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  SimpleRaster&    operator=           (const SimpleRaster& r);

  SimpleRaster&    operator*=          (const SimpleRaster& rhs);

  SimpleRaster&    operator*=          (const T& rhs);

  SimpleRaster&    operator/=          (const SimpleRaster& rhs);

  SimpleRaster&    operator/=          (const T& rhs);

  T*               release             ();

  SimpleRaster<T>& divide              (const T& value);

  SimpleRaster<T>& divide              (const SimpleRaster<T>& raster);

  SimpleRaster<T>& absolute            ();

  void             fill                (const T&v);

  void             fillMV              ();

  void             setMV               (size_t row,
                                        size_t col);

  void             setMV               (LinearLoc loc);

  void             setMV               (const CellLoc& loc);

  void             setMV               (SimpleRaster<bool> const& missingValues);

  void             takeMV              (SimpleRaster const& raster);

/*
  friend SimpleRaster operator*<>      (const SimpleRaster& lhs,
                                        const SimpleRaster& rhs);

  friend SimpleRaster operator*<>      (const SimpleRaster& lhs,
                                        const T& rhs);

  friend SimpleRaster operator*<>      (const T& lhs,
                                        const SimpleRaster& rhs);

  friend SimpleRaster operator/<>      (const SimpleRaster& lhs,
                                        const SimpleRaster& rhs);

  friend SimpleRaster operator/<>      (const SimpleRaster& lhs,
                                        const T& rhs);
                                        */

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const RasterDim& rasterDim           () const;

  iterator         begin               ();

  const_iterator   begin               () const;

  iterator         end                 ();

  const_iterator   end                 () const;

  T&               cell                (size_t index);

  const T&         cell                (size_t index) const;

  T&               cell                (size_t r,
                                        size_t c);

  const T&         cell                (size_t r,
                                        size_t c) const;

  T&               cell                (const CellLoc& loc);

  const T&         cell                (const CellLoc& loc) const;

  T*               cells               ();

  const T*         cells               () const;

  T&               operator[]          (size_t index);

  const T&         operator[]          (size_t index) const;

  T&               operator[]          (const CellLoc& loc);

  const T&         operator[]          (const CellLoc& loc) const;

  bool             isMV                (const CellLoc& loc) const;

  bool             isMV                (size_t row,
                                        size_t col) const;

  bool             isMV                (size_t index) const;

  bool             mv                  (const CellLoc& loc) const;

  bool             equals              (const SimpleRaster& aRaster) const;

  /*
  T                minimum             () const;

  T                maximum             () const;
  */

  friend std::ostream& operator<< <>   (std::ostream& s,
                                        const SimpleRaster& r);

  friend std::istream& operator>> <>   (std::istream& s,
                                        SimpleRaster& r);

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Constructs a SimpleRaster object.
/*!
  \param     nr Number of rows.
  \param     nc Number of columns.

  Enough room is allocated for nr * nc values.

  The values are undefined.
*/
template<class T>
inline SimpleRaster<T>::SimpleRaster(size_t nr, size_t nc)
  : RasterDim(nr,nc)
{
  d_values = new T[nrCells()];
}

template<class T>
inline SimpleRaster<T>::SimpleRaster(const RasterDim& rd)
  : RasterDim(rd)
{
  d_values = new T[nrCells()];
}


//! Constructs a SimpleRaster object.
/*!
  \param     nr Number of rows.
  \param     nc Number of columns.
  \param     v Initial value.

  Enough room is allocated for nr * nc values. All values are 
  initialized with \a v.
*/
template<class T>
inline SimpleRaster<T>::SimpleRaster(size_t nr, size_t nc, const T& v)
  : RasterDim(nr,nc)
{
  d_values = new T[nrCells()];
  fill(v);
}

//! Constructs a SimpleRaster object.
/*!
  \param     rd Raster dimension
  \param     v Initial value.

  All values are initialized with \a v.
*/
template<class T>
inline SimpleRaster<T>::SimpleRaster(const RasterDim& rd, const T& v)
  : RasterDim(rd)
{
  d_values = new T[nrCells()];
  fill(v);
}

//! Constructor.
/*!
  \param     nr Number of rows.
  \param     nc Number of columns.
  \param     values Pointer to values.
  \warning   This object will take ownership of the \a values pointer. It will
             be deleted (using delete[]) by the destructor. You must not
             delete it yourself or use the pointer after this object went out
             of scope.
  \sa        release()
*/
template<class T>
inline SimpleRaster<T>::SimpleRaster(size_t nr, size_t nc, T* values)
  : RasterDim(nr,nc)
{
  d_values = values;
}

template<class T>
inline SimpleRaster<T>::SimpleRaster(const RasterDim& rd, T* values)
  : RasterDim(rd)
{
  d_values = values;
}

template<class T>
inline void SimpleRaster<T>::copy(SimpleRaster<T> const& r)
{
  std::copy(r.d_values, r.d_values + nrCells(), d_values);
}

template<class T>
inline void SimpleRaster<T>::memCopy(SimpleRaster<T> const& r)
{
  std::memcpy(static_cast<void*>(d_values),
         static_cast<const void*>(r.d_values), nrCells() * sizeof(T));
}

// Copy constructor.
/*
  \param     r SimpleRaster object to copy from.
  \warning   memcpy is used to copy values. This function assumes that all values are stored in a linear array. This won't do in case the raster contains objects which reference memory outside this array. Use the assignment operator in case you have a raster with list object for example. This function is faster than the assignment operator.
*/

template<>
inline SimpleRaster<UINT1>::SimpleRaster(const SimpleRaster<UINT1>& r)
  : RasterDim(r)
{
  d_values = new UINT1[nrCells()];
  memCopy(r);
}

template<>
inline SimpleRaster<INT4>::SimpleRaster(const SimpleRaster<INT4>& r)
  : RasterDim(r)
{
  d_values = new INT4[nrCells()];
  memCopy(r);
}

template<>
inline SimpleRaster<REAL4>::SimpleRaster(const SimpleRaster<REAL4>& r)
  : RasterDim(r)
{
  d_values = new REAL4[nrCells()];
  memCopy(r);
}

template<class T>
inline SimpleRaster<T>::SimpleRaster(const SimpleRaster<T>& r)
  : RasterDim(r)
{
  d_values = new T[nrCells()];
  copy(r);
}

//! Destructs a SimpleRaster object.
/*!
*/
template<class T>
inline SimpleRaster<T>::~SimpleRaster()
{
  delete[] d_values;
}

// Assignment operator.
/*
  \param     r SimpleRaster to assign from.
  \return    A reference to *this.
  \warning   The number of rows and columns of both rasters must be equal.
  \warning   This function is slower than the copy constructor since each value is copy constructed explicitly while the copy constructor just copies a piece of memory.
*/

template<>
inline SimpleRaster<UINT1>& SimpleRaster<UINT1>::operator=(
         const SimpleRaster<UINT1>& r)
{
  DEVELOP_PRECOND(nrRows() == r.nrRows() && nrCols() == r.nrCols());

  if(this != &r) {
    memCopy(r);
  }

  return *this;
}

template<>
inline SimpleRaster<INT4>& SimpleRaster<INT4>::operator=(
         const SimpleRaster<INT4>& r)
{
  DEVELOP_PRECOND(nrRows() == r.nrRows() && nrCols() == r.nrCols());

  if(this != &r) {
    memCopy(r);
  }

  return *this;
}

template<>
inline SimpleRaster<REAL4>& SimpleRaster<REAL4>::operator=(
         const SimpleRaster<REAL4>& r)
{
  DEVELOP_PRECOND(nrRows() == r.nrRows() && nrCols() == r.nrCols());

  if(this != &r) {
    memCopy(r);
  }

  return *this;
}

template<class T>
inline SimpleRaster<T>& SimpleRaster<T>::operator=(const SimpleRaster<T>& r)
{
  DEVELOP_PRECOND(nrRows() == r.nrRows() && nrCols() == r.nrCols());

  if(this != &r) {
    copy(r);
  }

  return *this;
}



template<class ValueType>
inline SimpleRaster<ValueType>& SimpleRaster<ValueType>::operator*=(
         const SimpleRaster& rhs)
{
  com::multiplyByRange(begin(), end(), rhs.begin());
  return *this;
}



template<class ValueType>
inline SimpleRaster<ValueType>& SimpleRaster<ValueType>::operator*=(
         const ValueType& rhs)
{
  com::multiplyByValue(begin(), end(), rhs);
  return *this;
}



template<class ValueType>
inline SimpleRaster<ValueType>& SimpleRaster<ValueType>::operator/=(
         const SimpleRaster& rhs)
{
  com::divideByRange(begin(), end(), rhs.begin());
  return *this;
}



template<class ValueType>
inline SimpleRaster<ValueType>& SimpleRaster<ValueType>::operator/=(
         const ValueType& rhs)
{
  com::divideByValue(begin(), end(), rhs);
  return *this;
}



//! Releases the ownership of the layered values pointer from the object.
/*!
  \return    Values pointer.
  \warning   The layered values pointer is set to 0. After calling this
             function you can't call member functions which need this pointer.
             Normally, you call this function just before the object goes out
             of scope.
  \sa        SimpleRaster(size_t, size_t, T*)

  This function is handy only if you need the underlying pointer to pass into
  functions/objects which take ownership of the pointer. In 99% of all cases
  you don't need this function and if you do you're probably hacking your way
  out of a nasty situation!

  CW TODO soms will je dit echt, net zoals je een ctor met copy semantics
  will hebben?
*/
template<class T>
T* SimpleRaster<T>::release()
{
  T* tmp = d_values;
  d_values = 0;
  return tmp;
}

template<class T>
const geo::RasterDim& SimpleRaster<T>::rasterDim() const
{
  return *this;
}

//! Returns an iterator to the first cell value.
/*!
  \return    Iterator to the first cell value.
*/
template<class T>
inline typename SimpleRaster<T>::iterator SimpleRaster<T>::begin()
{
  return d_values;
}

//! Returns a const iterator to the first cell value.
/*!
  \return    Const iterator to the first cell value.
*/
template<class T>
inline typename SimpleRaster<T>::const_iterator SimpleRaster<T>::begin() const
{
  return d_values;
}

//! Returns an iterator to the one-past-the-last cell value.
/*!
  \return    Iterator to the one-past-the-last cell value.
*/
template<class T>
inline typename SimpleRaster<T>::iterator SimpleRaster<T>::end()
{
  return d_values + nrCells();
}

//! Returns a const iterator to the one-past-the-last cell value.
/*!
  \return    Const iterator to the one-past-the-last cell value.
*/
template<class T>
inline typename SimpleRaster<T>::const_iterator SimpleRaster<T>::end() const
{
  return d_values + nrCells();
}

//! Access cell values as a 1 dimensional array.
/*!
   \param index index in range [0 , nrCells() >
*/
template<class T>
T& SimpleRaster<T>::cell(size_t index)
{
#ifdef DEBUG_DEVELOP
  PRECOND(index < nrCells());
#endif

  return d_values[index];
}

//! Access cell values as a 1 dimensional array.
/*!
   \param index index in range [0 , nrCells() >
*/
template<class T>
const T& SimpleRaster<T>::cell(size_t index) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(index < nrCells());
#endif

  return d_values[index];
}

//! Returns a writable reference to a cell value.
/*!
  \param     r Row number.
  \param     c Column number.
  \return    Reference to a cell value.
  \warning   Row numbers range from [0, nr) and column number range from
             [0, nc).
*/
template<class T>
inline T& SimpleRaster<T>::cell(size_t r, size_t c)
{
#ifdef DEBUG_DEVELOP
  if (!(r < nrRows() && c < nrCols()))
    PRECOND(r < nrRows() && c < nrCols());
#endif

  return d_values[r * nrCols() + c];
}

//! Returns a read-only reference to a cell value.
/*!
  \param     r Row number.
  \param     c Column number.
  \return    Reference to a cell value.
  \warning   Row numbers range from [0, nr) and column number range from
             [0, nc).
*/
template<class T>
inline const T& SimpleRaster<T>::cell(size_t r, size_t c) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(r < nrRows() && c < nrCols());
#endif

  return d_values[r * nrCols() + c];
}

/*!
  \overload
*/
template<class T>
T& SimpleRaster<T>::cell(const CellLoc& loc)
{
  return cell(loc.row(), loc.col());
}

template<class T>
bool SimpleRaster<T>::isMV(const CellLoc& loc) const
{
  return pcr::isMV(cell(loc));
}

template<class T>
bool SimpleRaster<T>::isMV(size_t row, size_t col) const
{
  return pcr::isMV(cell(row, col));
}

template<class T>
inline bool SimpleRaster<T>::isMV(size_t index) const
{
  return pcr::isMV(cell(index));
}

template<class T>
 bool SimpleRaster<T>::mv(const CellLoc& loc) const
{
  return pcr::isMV(cell(loc));
}

/*!
  \overload
*/
template<class T>
const T& SimpleRaster<T>::cell(const CellLoc& loc) const 
{
  return cell(loc.row(), loc.col());
}

//! Returns a pointer to the array with cell values.
/*!
  \return    Array with cell values.
  \warning   Careful!
  \sa        cell(size_t,size_t)
*/
template<class T>
inline T* SimpleRaster<T>::cells()
{
  return d_values;
}

//! Returns a pointer to the array with cell values.
/*!
  \return    Array with cell values.
  \sa        cell(size_t,size_t)
*/
template<class T>
inline const T* SimpleRaster<T>::cells() const
{
  return d_values;
}

//! Access cell values as a 1 dimensional array.
/*!
   \param index index in range [0 , nrCells() >
*/
template<class T>
inline T& SimpleRaster<T>::operator[](size_t index)
{
#ifdef DEBUG_DEVELOP
  PRECOND(index < nrCells());
#endif

  return d_values[index];
}

//! Access cell values as a 1 dimensional array.
/*!
   \param index index in range [0 , nrCells() >
*/
template<class T>
inline const T& SimpleRaster<T>::operator[](size_t index) const
{
#ifdef DEBUG_DEVELOP
  PRECOND(index < nrCells());
#endif

  return d_values[index];
}

template<class T>
inline const T& SimpleRaster<T>::operator[](const CellLoc& loc) const
{
  return cell(loc.row(),loc.col());
}

template<class T>
inline       T& SimpleRaster<T>::operator[](const CellLoc& loc)
{
  return cell(loc.row(),loc.col());
}


/*
template<typename ValueType>
inline ValueType SimpleRaster<ValueType>::minimum() const
{
  return com::minimum(begin(), end());
}

template<typename ValueType>
inline ValueType SimpleRaster<ValueType>::maximum() const
{
  return com::maximum(begin(), end());
}
*/

template<class T>
bool SimpleRaster<T>::equals(const SimpleRaster<T>& aRaster) const
{
  return nrRows() == aRaster.nrRows() && nrCols() == aRaster.nrCols() &&
              std::equal(d_values, d_values + nrCells(), aRaster.d_values);
}

//! set all values to \a v
template<class T>
inline void SimpleRaster<T>::fill(const T&v)
{
  if (v==0)
   std::memset(static_cast<void*>(d_values),0,nrCells()*sizeof(T));
  else
   std::fill(d_values, d_values + nrCells(), static_cast<T>(v));
}

//! set all values to \a v
template<class T>
inline void SimpleRaster<T>::fillMV()
{
  pcr::setMV(d_values,nrCells());
}

//! Sets value at \a row, \a col to missing value.
template<class T>
void SimpleRaster<T>::setMV(
         size_t row,
         size_t col)
{
  pcr::setMV(cell(row, col));
}

//! Set value at \a loc to missing value.
template<class T>
void SimpleRaster<T>::setMV(
         LinearLoc loc)
{
  pcr::setMV(cell(loc));
}

//! Set value at \a loc to missing value.
template<class T>
void SimpleRaster<T>::setMV(const CellLoc& loc)
{
  pcr::setMV(cell(loc));
}

//! Sets cells to missing value where \a missingValues is true.
/*!
  \param     missingValues Boolean raster.
  \warning   Only true cells on \a missingValues are considered, other cells are untouched (not set to a non-MV value for example).
*/
template<class T>
void SimpleRaster<T>::setMV(
         SimpleRaster<bool> const& missingValues)
{
  PRECOND(nrCells() == missingValues.nrCells());

  for(size_t i = 0; i < nrCells(); ++i) {
    if(missingValues.cell(i)) {
      setMV(i);
    }
  }
}

//! Assigns a missing value to each cell \a raster has a missing value in.
/*!
  \param     raster Raster to look for missing value in.

  Cells for which \a raster has no missing value are not touched.
*/
template<class T>
inline void SimpleRaster<T>::takeMV(
         SimpleRaster const& raster)
{
  PRECOND(nrCells() == raster.nrCells());

  for(size_t i = 0; i < nrCells(); ++i) {
    if(pcr::isMV(raster.d_values[i])) {
      pcr::setMV(d_values[i]);
    }
  }
}

template<class T>
inline SimpleRaster<T>& SimpleRaster<T>::divide(const T& value)
{
  PRECOND(!pcr::isMV(value));

  if(value == T(0))
    fillMV();
  else {
    for(size_t i = 0; i < nrCells(); ++i) {
      if(!pcr::isMV(d_values[i])) {
        d_values[i] /= value;
      }
    }
  }

  return *this;
}



template<class T>
inline SimpleRaster<T>& SimpleRaster<T>::divide(const SimpleRaster<T>& raster)
{
  for(size_t i = 0; i < nrCells(); ++i) {
    if(!pcr::isMV(d_values[i])) {
      if(pcr::isMV(raster.d_values[i]) || raster.d_values[i] == 0) {
        pcr::setMV(d_values[i]);
      }
      else {
        d_values[i] /= raster.d_values[i];
      }
    }
  }

  return *this;
}



template<class T>
inline SimpleRaster<T>& SimpleRaster<T>::absolute()
{
  for(size_t i = 0; i < nrCells(); ++i) {
    if(!pcr::isMV(d_values[i])) {
      d_values[i] = ABS(d_values[i]);
    }
  }

  return *this;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

template<class T>
std::ostream& operator<<(std::ostream& s, const SimpleRaster<T>& raster)
{
  for(size_t r = 0; r < raster.nrRows(); ++r) {

    for(size_t c = 0; c < raster.nrCols(); ++c) {
      s << raster.cell(r, c) << ' ';
    }

    s << '\n';

  }

  return s;
}



template<class T>
std::istream& operator>>(std::istream& s, SimpleRaster<T>& raster)
{
  for(size_t r = 0; r < raster.nrRows(); ++r) {

    for(size_t c = 0; c < raster.nrCols(); ++c) {
      s >> raster.cell(r, c);
    }

  }

  if(!s.good()) {
    throw com::BadStreamFormat("SimpleRaster: Bad format");
  }

  return s;
}



template<class T>
bool operator==(const SimpleRaster<T>& lhs, const SimpleRaster<T>& rhs)
{
  return lhs.equals(rhs);
}



template<class ValueType>
inline SimpleRaster<ValueType> operator*(
         const SimpleRaster<ValueType>& lhs, const SimpleRaster<ValueType>& rhs)
{
  PRECOND(lhs.nrRows() == rhs.nrRows());
  PRECOND(lhs.nrCols() == rhs.nrCols());

  SimpleRaster<ValueType> raster(lhs);
  raster *= rhs;
  return raster;
}



template<class ValueType>
inline SimpleRaster<ValueType> operator*(
         const SimpleRaster<ValueType>& lhs, const ValueType& rhs)
{
  SimpleRaster<ValueType> raster(lhs);
  raster *= rhs;
  return raster;
}



template<class ValueType>
inline SimpleRaster<ValueType> operator*(
         const ValueType& lhs, const SimpleRaster<ValueType>& rhs)
{
  SimpleRaster<ValueType> raster(rhs);
  raster *= lhs;
  return raster;
}



template<class ValueType>
inline SimpleRaster<ValueType> operator/(
         const SimpleRaster<ValueType>& lhs, const SimpleRaster<ValueType>& rhs)
{
  PRECOND(lhs.nrRows() == rhs.nrRows());
  PRECOND(lhs.nrCols() == rhs.nrCols());

  SimpleRaster<ValueType> raster(lhs);
  raster /= rhs;
  return raster;
}



template<class ValueType>
inline SimpleRaster<ValueType> operator/(
         const SimpleRaster<ValueType>& lhs, const ValueType& rhs)
{
  SimpleRaster<ValueType> raster(lhs);
  raster /= rhs;
  return raster;
}



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------

typedef SimpleRaster<UINT1> BooleanSimpleRaster;
typedef SimpleRaster<INT4> NominalSimpleRaster;
typedef SimpleRaster<INT4> OrdinalSimpleRaster;
typedef SimpleRaster<REAL4> ScalarSimpleRaster;
typedef SimpleRaster<REAL4> DirectionalSimpleRaster;
typedef SimpleRaster<UINT1> LddSimpleRaster;



} // namespace geo

#endif
