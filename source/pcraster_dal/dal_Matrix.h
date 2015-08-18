#ifndef INCLUDED_DAL_MATRIX
#define INCLUDED_DAL_MATRIX



// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_BOOST_ANY
#include <boost/any.hpp>
#define INCLUDED_BOOST_ANY
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_COMPILER
#include "dev_Compiler.h"
#define INCLUDED_DEV_COMPILER
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASET
#include "dal_Dataset.h"
#define INCLUDED_DAL_DATASET
#endif

#ifndef INCLUDED_DAL_TYPE
#include "dal_Type.h"
#define INCLUDED_DAL_TYPE
#endif



namespace dal {
  // Matrix declarations.
}



namespace dal {



//! This is the Dataset class for matrix data.
/*!
  In a matrix all cell values have the same type. The dimensions of the matrix
  are nrRows() * nrCols().

  Values are kept in a 1 dimensional array, with the 2 dimensional grid/raster 
  simulated in row-major (Ansi-C style).
  The indexing equation for a row-major array with 0-based indexing as
  in C is:
  \code
              index = (row * d_nrCols) + column
  \endcode


  This class is also a base class for the spatial Raster Dataset class.

  The template functions in the class will throw boost::bad_any_cast if the
  type with which the functions are called doesn't correspond with the type
  of the cell values in the layered array.

  The extremes min() and max() may be set
  setExtremes(boost::any min, boost::any max) explicitly or computed by
  setExtremes(). In both cases hasExtremes() will be true. Note that min(),
  max() and allMV() is not updated when cells are modified at any time.

  \todo KDJ Get rid of allMV stuff. I don't think it is necessary. Discuss
        with CW.
  \todo KDJ Use MatrixDimensions as a base. Can we do without the setNrRows()?

  \warning Keep this class as small and simple as possible. Do not make this
           class a full fledged matrix class with all kinds of handies. Bare
           minimum for I/O.
*/
class PCR_DAL_DECL Matrix: public Dataset
{

  friend class MatrixTest;

public:

  //! for transfer methods
  enum Ownership {
    //! transfer cells arg must be created with new T[]
    TakeOwnership,
    //! transfer will only refer to the cells arg pointer
    DoNotTakeOwnerShip
  };

private:

  //! Number of rows.
  size_t           d_nrRows;

  //! Number of columns.
  size_t           d_nrCols;

  //! Type id of cell values.
  TypeId           d_typeId;

  //! Cell values.
  boost::any       d_cells;

  Ownership        d_ownership;

  //! minimum value, if empty() then !hasExtremes() or allMV()
  boost::any       d_min;

  //! maximum value, if empty() then !hasExtremes() or allMV()
  boost::any       d_max;

  //! is all data MV?, unspecified if !hasExtremes()
  bool             d_allMV;

  //! are the min,max and allMV known?
  bool             d_hasExtremes;

  void             copyCells           (void const* cells);

  template<typename T>
  T*               createCells         ();

  template<typename T>
  void             copyCells           (T const* cells);

  void             eraseCells          ();

protected:

                   Matrix              (DatasetType datasetType,
                                        size_t nrRows,
                                        size_t nrCols,
                                        TypeId typeId);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Matrix              ();

                   Matrix              (size_t nrCols,
                                        TypeId typeId);

                   Matrix              (size_t nrRows,
                                        size_t nrCols,
                                        TypeId typeId);

                   Matrix              (Matrix const& rhs);

  virtual          ~Matrix             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Matrix&          operator=           (Matrix const& rhs);

  void             createCells         ();

  void             transfer            (void* cells,
                                        Ownership ownership=TakeOwnership);

  void             setCellsReference   (void* cells);

  template<typename T>
  PCR_DAL_DECL void  transfer          (T* cells,
                                        Ownership ownership=TakeOwnership);

  void*            release             ();

  template<typename T>
  T*               release             ();

  void             setTypeId           (TypeId typeId);

  void             setNrRows           (size_t nrRows);

  template<typename T>
  void             eraseCells          ();

  void             setAllMV            ();

  template<typename T>
  void             setAllMV            ();

  template<typename T>
  void             takeMV              (Matrix const& matrix);

  template<typename T>
  PCR_DAL_DECL void fill               (T const& value);

  void             setExtremes         (boost::any min,
                                        boost::any max);

  void             setExtremes         ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  size_t           nrCells             () const;

  TypeId           typeId              () const;

  bool             cellsAreCreated     () const;

  void*            cells               ();

  void const*      cells               () const;


  template<typename T>
  T const*         cells               () const;

  template<typename T>
  PCR_DAL_DECL T*  cells               ();

  template<typename T>
  PCR_DAL_DECL T const& cell           (size_t index) const;

  template<typename T>
  PCR_DAL_DECL T&  cell                (size_t index);

  template<typename T>
  PCR_DAL_DECL T const&  cell          (size_t row,
                                        size_t col) const;

  template<typename T>
  PCR_DAL_DECL T&  cell                (size_t row,
                                        size_t col);

  bool             hasExtremes         () const;

  bool             allMV               () const;

  boost::any       min                 () const;

  boost::any       max                 () const;

  template<typename T>
  PCR_DAL_DECL T   min                 () const;

  template<typename T>
  PCR_DAL_DECL T   max                 () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

// //! Creates the datastructure for holding cell values.
// /*!
//   \return    Reference to the array.
//   \warning   The datastructure must not already be created.
//   \sa        eraseCells()
// 
//   The array will contain nrCells() values. Useful if the call assigns to the
//   cells in the array.
// */
// template<typename T>
// inline T* Matrix::createCells()
// {
//   std::cout << "createCells<T>" << '\t' << TypeTraits<T>::typeId << std::endl;
//   assert(!cellsAreCreated());
// 
//   T* pointer = new T[nrCells()];
//   d_cells = pointer;
//   std::cout << this << '\t' << d_cells.type().name() << std::endl;
// 
//   assert(cells<T>()); 
// 
//   std::cout << "/createCells<T>" << std::endl;
//   return pointer;
// }
// 
// //! Accepts the ownership of \a cells.
// /*!
//   \param     cells Array to adopt, if ownership==TakeOwnership then cells
//              must be allocated with new T[]
//   \param     ownership Determines whether \a cells is ours to delete upon
//              destruction or not.
//   \warning   The current cells will be erased first.
//   \sa        release()
// */
// template<typename T>
// inline void Matrix::transfer(
//          T* cells,
//          Ownership ownership)
// {
//   eraseCells<T>();
//   d_cells = cells;
//   d_ownership = ownership;
// }
// 
// //! Releases ownership of the layered array of cell values.
// /*!
//   \return    Pointer to the array of cell values.
//   \warning   An array of cell values must have been created.
//   \sa        transfer(T*)
// */
// template<typename T>
// inline T* Matrix::release()
// {
//   assert(cellsAreCreated());
// 
//   T* pointer = boost::any_cast<T*>(d_cells);
//   d_cells = boost::any();
// 
//   return pointer;
// }
// 
// //! Returns a reference to the layered array with cell values.
// /*!
//   \return    Array with cell values.
//   \warning   The array for cell values must have been created.
// 
//   This function assumes that there are cell values to return the array for.
//   Use void const* dal::Matrix::cells() const to test whether this is the case.
// */
// template<typename T>
// inline T const* Matrix::cells() const
// {
//   assert(cellsAreCreated());
// 
//   T* pointer = boost::any_cast<T*>(d_cells);
// 
//   return pointer;
// }
// 
// template<typename T>
// inline T* Matrix::cells()
// {
//   std::cout << "cells" << std::endl;
//   assert(cellsAreCreated());
// 
//   std::cout << "cast..." << std::endl;
//   std::cout << this << '\t' << d_cells.type().name() << std::endl;
//   T* pointer = boost::any_cast<T*>(d_cells);
//   std::cout << "/cast..." << std::endl;
// 
//   std::cout << "/cells" << std::endl;
//   return pointer;
// }
// 
// template<typename T>
// inline T const& Matrix::cell(size_t index) const
// {
//   assert(cellsAreCreated());
// 
//   return cells<T>()[index];
// }
// 
// template<typename T>
// inline T& Matrix::cell(size_t index)
// {
//   assert(cellsAreCreated());
// 
//   return cells<T>()[index];
// }
// 
// //! Deletes the layered array for cell values.
// /*!
//   \sa        createCells()
// 
//   Nothing happens if the array has not been created.
// */
// template<typename T>
// inline void Matrix::eraseCells()
// {
//   if(cellsAreCreated() && d_ownership == TakeOwnership) {
//     delete[] cells<T>();
//   }
// 
//   d_cells = boost::any();
// }
// 
// //! Fills the matrix with MV values.
// /*!
//   \warning   The cells must have been created already.
// */
// template<typename T>
// inline void Matrix::setAllMV()
// {
//   assert(cellsAreCreated());
//   T* pointer = cells<T>();
// 
//   for(size_t i = 0; i < nrCells(); ++i) {
//     pcr::setMV(pointer[i]);
//   }
// }
// 
// //! Fills the matrix with \a value values.
// /*!
//   \warning   The cells must have been created already.
// */
// template<typename T>
// inline void Matrix::fill(T const& value)
// {
//   assert(cellsAreCreated());
// 
//   T* pointer = cells<T>();
// 
//   for(size_t i = 0; i < nrCells(); ++i) {
//     pointer[i] = value;
//   }
// }
// 
// template<typename T>
// inline T Matrix::min() const
// {
//   return boost::any_cast<T>(d_min);
// }
// 
// template<typename T>
// inline T Matrix::max() const
// {
//   return boost::any_cast<T>(d_max);
// }
// 
// template<typename T>
// inline void Matrix::copyCells(T const* cells)
// {
//   assert(cellsAreCreated());
// 
//   d_cells = copyNrCells(this->cells<T>(), cells, nrCells());
// }



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
T* copyNrCells( T* destination,
         T const* source,
         size_t size) {
  std::memcpy(static_cast<void*>(destination),
         static_cast<void const*>(source), size * sizeof(T));

  return destination;
}



template<typename T>
T* newCopy(
         T const* source,
         size_t size) {
  T* result = new T[size];

  return copyNrCells(result, source, size);
}

} // namespace dal

#endif
