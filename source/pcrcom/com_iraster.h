#ifndef INCLUDED_COM_IRASTER
#define INCLUDED_COM_IRASTER



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



namespace com {
  // IRaster declarations.
}



namespace com {

//! Interface class for Rasters.
/*!
*/
template<typename T>
class IRaster
{

private:

  const size_t     d_nrRows;

  const size_t     d_nrCols;

  //! Assignment operator. NOT IMPLEMENTED.
  IRaster&         operator=           (const IRaster&);

  //! Copy constructor. NOT IMPLEMENTED.
                   IRaster             (const IRaster&);

protected:

                   IRaster             (size_t nrRows,
                                        size_t nrCols);

   size_t          index(size_t r, size_t c) const;

   //! Access cell values as a 1 dimensional array.
   /*!
       \param index index in range [0 , nrCells() >
   */
   virtual T&      operator[]          (size_t index)=0;

    //! Access cell values as a 1 dimensional array.
    /*!
       \param index index in range [0 , nrCells() >
    */
   virtual const T&operator[]          (size_t index) const=0;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~IRaster            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setMV               (size_t row,
                                        size_t col);

/*
  IRaster<T>&      operator+=          (const T& rhs)
  { return (*this).add(rhs); }

  IRaster<T>&      operator+=          (const IRaster<T>& rhs)
  { return (*this).add(rhs); }

  // virtual IRaster<T>& add              (const T& rhs) = 0;
  */

  virtual IRaster<T>& add              (const IRaster<T>& rhs) = 0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             isMV                (size_t row,
                                        size_t col) const;

  T&               element             (size_t index);

  const T&         element             (size_t index) const;

  T&               cell                (size_t r,
                                        size_t c);

  const T&         cell                (size_t r,
                                        size_t c) const;


  bool             cell                (T& value, size_t r, size_t c) const;

  bool             cell                (T& value, int r, int c) const;

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  size_t           nrCells             () const;

  bool             isOutside           (int row,
                                        int col) const;

  /*!
    \return true if raster is represented by different values, false if
            represented by a single value.
   */
  virtual bool isMultiValued()const=0;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class T>
IRaster<T>::IRaster(size_t nrRows, size_t nrCols)
  : d_nrRows(nrRows), d_nrCols(nrCols)
{
}

template<class T>
inline IRaster<T>::~IRaster ()
{
}

template<class T>
inline T& IRaster<T>::element(size_t i)
{
  return operator[](i);
}

template<class T>
inline const T& IRaster<T>::element(size_t i) const
{
  return operator[](i);
}

template<class T>
inline T& IRaster<T>::cell(size_t r, size_t c)
{
  return operator[](index(r,c));
}

template<class T>
inline const T& IRaster<T>::cell(size_t r, size_t c) const
{
  return operator[](index(r,c));
}

/*!
 *   \param value  a value if return value is true, \a value is untouched
 *                 if return value is false. A MV is never set as value.
 *  \returns true if \a value is not missing value, false
 *          otherwise
 */
template<class T>
inline bool IRaster<T>::cell(T& value, size_t r, size_t c) const
{
  DEVELOP_PRECOND(!isOutside(r,c));
  if (isMV(r,c))
    return false;
  value=cell(r,c);
  return true;
}

/*!
 *   \param value  a value if return value is true, \a value is untouched
 *                 if return value is false. A MV is never set as value.
 *   \returns true if \a value is not missing value and \a r and
 *           \c c are inside the raster dimensions, false
 *            otherwise
 */
template<class T>
inline bool IRaster<T>::cell(T& value, int r, int c) const
{
  if(isOutside(r, c))
    return false;
  return cell(value,static_cast<size_t>(r),
                    static_cast<size_t>(c));
}

template<class T>
inline size_t IRaster<T>::nrRows() const
{
  return d_nrRows;
}

template<class T>
inline size_t IRaster<T>::nrCols() const
{
  return d_nrCols;
}

template<class T>
inline size_t IRaster<T>::nrCells() const
{
  return d_nrRows * d_nrCols;
}

template<class T>
inline bool IRaster<T>::isOutside(int row, int col) const
{
  return row < 0 || row >= (int)nrRows() || col < 0 || col >= (int)nrCols();
}

template<class T>
inline size_t IRaster<T>::index(size_t row, size_t col) const
{
  return row*nrCols()+col;
}

template<class T>
inline bool IRaster<T>::isMV(size_t row, size_t col) const
{
  return pcr::isMV(cell(row,col));
}

template<class T>
inline void IRaster<T>::setMV(size_t row, size_t col)
{
  pcr::setMV(cell(row,col));
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
