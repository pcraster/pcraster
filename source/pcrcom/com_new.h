#ifndef INCLUDED_COM_NEW
#define INCLUDED_COM_NEW

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

namespace com {
  //! Allocate 2 dimensional grid in from existing linear memory
  /*! Allocation is done such that array[0] is also a
   *  pointer to linear a piece of memory with nrCols *nrRows
   *  elements: \a linear.
   */
  template<class CellType> CellType **new2dFromLinear(
   CellType *linear,
   size_t nrRows,
   size_t nrCols)
  {
    CellType **ptrVal = new CellType *[nrRows];
    ptrVal[0] = linear;
    for (size_t i = 1; i < nrRows; i++)
      ptrVal[i] = ptrVal[i-1]+nrCols;

    return ptrVal;
  }

  //! Allocate 2 dimensional grid in linear memory
  /*! Allocation is done such that array[0] is also a
   *  pointer to linear a piece of memory with nrCols *nrRows
   *  elements.
   */
  template<class CellType> CellType **new2d(
   size_t nrRows,
   size_t nrCols)
  {
    CellType *linear = new CellType[nrRows*nrCols];
    return new2dFromLinear(linear, nrRows, nrCols);
  }
  //! as com::new2d but does 0 the values
  template<class CellType> CellType **new02d(
   size_t nrRows,
   size_t nrCols)
  {
    CellType *linear = new CellType[nrRows*nrCols];
    std::memset(linear,0,sizeof(CellType)*nrRows*nrCols);
    return new2dFromLinear(linear, nrRows, nrCols);
  }
  //! Allocate 2 dimensional grid in linear memory
  /*! Allocation is done such that array[0] is also a
   *  pointer to linear a piece of memory with nrCols *nrRows
   *  elements. As new2dFromLinear but  with optional linear memory
   *  argument
   */
  template<class CellType> CellType **new2d(
   size_t nrRows,
   size_t nrCols,
   CellType *linear)
  {
    if (!linear)
      linear = new CellType[nrRows*nrCols];
    return new2dFromLinear(linear, nrRows, nrCols);
  }

  template<class CellType> void delete2d(
   CellType **data,
   bool deleteLinear=true)
  {
    if (deleteLinear)
     delete [] data[0];
   delete [] data;
  }

} // namespace com

#endif
