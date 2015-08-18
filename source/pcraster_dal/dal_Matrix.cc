#ifndef INCLUDED_DAL_MATRIX
#include "dal_Matrix.h"
#define INCLUDED_DAL_MATRIX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Matrix class.
*/



//------------------------------------------------------------------------------

namespace dal {

template<typename T>
static void findExtremes(
         T const* data,
         size_t size,
         boost::any& min,
         boost::any& max,
         bool& allMV)
{
  allMV=true;
  T minT= T();
  T maxT= T();

  for(size_t i=0; i < size; ++i) {
    if (!pcr::isMV(data[i])) {
      if (allMV)
        minT=maxT=data[i];
      allMV=false;
      minT = std::min<T>(minT,data[i]);
      maxT = std::max<T>(maxT,data[i]);
    }
  }
  if (allMV)
    min=max=boost::any();
  else {
    min=minT;
    max=maxT;
  }
}

} // namespace dal



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATRIX MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MATRIX MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  \warning   The type id is set to an invalid value (TI_NR_TYPES).
  \sa        createCells(), transfer(void*), transfer(T*), setTypeId(TypeId),
             setNrRows(size_t)

  Creates an empty matrix with 0-sized rows and columns. You can configure and/or
  create the matrix later.
*/
dal::Matrix::Matrix()

  : Dataset(MATRIX),
    d_nrRows(0), d_nrCols(0), d_typeId(TI_NR_TYPES),
    d_ownership(TakeOwnership),
    d_allMV(true), d_hasExtremes(false)

{
}



//! Constructor.
/*!
  \param     nrCols Number of cols.
  \param     typeId Type id of cell values.
  \sa        createCells(), transfer(void*), transfer(T*), setNrRows(size_t)

  Creates an empty matrix with 0-sized rows and columns. You can configure and/or
  create the matrix later.
*/
dal::Matrix::Matrix(size_t nrCols, TypeId typeId)

  : Dataset(MATRIX),
    d_nrRows(0), d_nrCols(nrCols), d_typeId(typeId),
    d_ownership(TakeOwnership),
    d_allMV(true), d_hasExtremes(false)

{
}



//! Constructor.
/*!
  \param     datasetType Dataset type.
  \param     nrRows Number of rows.
  \param     nrCols Number of cols.
  \param     typeId Type id of cell values.
  \sa        createCells(), transfer(void*), transfer(T*)

  Creates an empty matrix with zero rows and columns. You can configure and/or
  create the matrix later.
*/
dal::Matrix::Matrix(
         DatasetType datasetType,
         size_t nrRows,
         size_t nrCols,
         TypeId typeId)

  : Dataset(datasetType),
    d_nrRows(nrRows), d_nrCols(nrCols), d_typeId(typeId),
    d_ownership(TakeOwnership),
    d_allMV(true), d_hasExtremes(false)

{
}


//! Constructor.
/*!
  \param     nrRows Number of rows.
  \param     nrCols Number of cols.
  \param     typeId Type id of cell values.
  \sa        createCells(), transfer(void*), transfer(T*)

  Creates an empty matrix with zero rows and columns. You can configure and/or
  create the matrix later.
*/
dal::Matrix::Matrix(
         size_t nrRows,
         size_t nrCols,
         TypeId typeId)

  : Dataset(MATRIX),
    d_nrRows(nrRows), d_nrCols(nrCols), d_typeId(typeId),
    d_ownership(TakeOwnership),
    d_allMV(true),d_hasExtremes(false)
{
}



//! Copy constructor.
dal::Matrix::Matrix(
         Matrix const& rhs)

  : Dataset(rhs),
    d_nrRows(rhs.d_nrRows), d_nrCols(rhs.d_nrCols),
    d_typeId(rhs.d_typeId), d_ownership(TakeOwnership),
    d_min(rhs.d_min), d_max(rhs.d_max), d_allMV(rhs.d_allMV),
    d_hasExtremes(rhs.d_hasExtremes)

{
  if(rhs.cellsAreCreated()) {
    createCells();
    copyCells(rhs.cells());
  }
}



//! Destructor.
/*!
  \warning   The layered array with cell values is deleted. If you want to
             keep hold of the data call release() first.
  \sa        release()
*/
dal::Matrix::~Matrix()
{
  if(cellsAreCreated()) {
    eraseCells();
  }
}



//! Assignment operator.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Refactor copy / assignment switch to copyCells() member.
*/
dal::Matrix& dal::Matrix::operator=(Matrix const& rhs)
{
  if(this != &rhs) {
    static_cast<Dataset&>(*this) = rhs;
    d_nrRows = rhs.d_nrRows;
    d_nrCols = rhs.d_nrCols;
    d_typeId = rhs.d_typeId;
    d_ownership = TakeOwnership;
    d_min = rhs.d_min;
    d_max = rhs.d_max;
    d_allMV = rhs.d_allMV;
    d_hasExtremes = rhs.d_hasExtremes;

    if(!rhs.cellsAreCreated()) {
      eraseCells();
    }
    else {
      if(!cellsAreCreated()) {
        createCells();
      }

      copyCells(rhs.cells());
    }

  }

  return *this;
}



//! Sets the type id of the cell values to \a typeId.
/*!
  \param     typeId New type id.
  \warning   The array for cell values must not already be created.
*/
void dal::Matrix::setTypeId(TypeId typeId)
{
#ifdef DEBUG_DEVELOP
  if(d_typeId != typeId) {
    assert(!cellsAreCreated());
  }
#endif

  d_typeId = typeId;
}



//! Set number of rows to \a nrRows.
/*!
  \param     nrRows New number of rows.
  \pre       The number of rows must not already be set, it must be still 0.
  \sa        .
*/
void dal::Matrix::setNrRows(size_t nrRows)
{
  assert(d_nrRows == 0);

  d_nrRows = nrRows;
}

//! shorthand for transfer(cells,DoNotTakeOwnerShip);
void dal::Matrix::setCellsReference(void* cells)
{
  transfer(cells,DoNotTakeOwnerShip);
}

/*!
  \overload
*/
void dal::Matrix::transfer(
         void* cells,
         Ownership ownership)
{
  assert(d_typeId != TI_NR_TYPES);

  switch(d_typeId) {
    case TI_INT1 : transfer<INT1>(static_cast<INT1*>(cells), ownership); break;
    case TI_INT2 : transfer<INT2>(static_cast<INT2*>(cells), ownership); break;
    case TI_INT4 : transfer<INT4>(static_cast<INT4*>(cells), ownership); break;
    case TI_UINT1: transfer<UINT1>(static_cast<UINT1*>(cells), ownership); break;
    case TI_UINT2: transfer<UINT2>(static_cast<UINT2*>(cells), ownership); break;
    case TI_UINT4: transfer<UINT4>(static_cast<UINT4*>(cells), ownership); break;
    case TI_REAL4: transfer<REAL4>(static_cast<REAL4*>(cells), ownership); break;
    case TI_REAL8: transfer<REAL8>(static_cast<REAL8*>(cells), ownership); break;
    case TI_UINT1_VECTOR: transfer<UINT1_VECTOR>(static_cast<UINT1_VECTOR*>(cells), ownership); break;
    case TI_INT4_VECTOR: transfer<INT4_VECTOR>(static_cast<INT4_VECTOR*>(cells), ownership); break;
    case TI_REAL4_VECTOR: transfer<REAL4_VECTOR>(static_cast<REAL4_VECTOR*>(cells), ownership); break;
    default: assert(false); break;
  }
}



/*!
  \overload
*/
void* dal::Matrix::release()
{
  assert(cellsAreCreated());
  assert(d_typeId != TI_NR_TYPES);

  void* result = 0;

  switch(d_typeId) {
    case TI_INT1 : result = release<INT1>(); break;
    case TI_INT2 : result = release<INT2>(); break;
    case TI_INT4 : result = release<INT4>(); break;
    case TI_UINT1: result = release<UINT1>(); break;
    case TI_UINT2: result = release<UINT2>(); break;
    case TI_UINT4: result = release<UINT4>(); break;
    case TI_REAL4: result = release<REAL4>(); break;
    case TI_REAL8: result = release<REAL8>(); break;
    case TI_UINT1_VECTOR: result = release<UINT1_VECTOR>(); break;
    case TI_INT4_VECTOR: result = release<INT4_VECTOR>(); break;
    case TI_REAL4_VECTOR: result = release<REAL4_VECTOR>(); break;
    case TI_STRING: result = release<std::string>(); break;
    case TI_NR_TYPES: assert(d_typeId != TI_NR_TYPES); break;
  }

  return result;
}



//! Returns the number or rows.
/*!
  \return    Number of rows.
*/
size_t dal::Matrix::nrRows() const
{
  return d_nrRows;
}



//! Returns the number or cols.
/*!
  \return    Number of cols.
*/
size_t dal::Matrix::nrCols() const
{
  return d_nrCols;
}



//! Returns the number or cells.
/*!
  \return    Number of cells.
*/
size_t dal::Matrix::nrCells() const
{
  return nrRows() * nrCols();
}



//! Returns the type id.
/*!
  \return    Type id.
*/
dal::TypeId dal::Matrix::typeId() const
{
  return d_typeId;
}



bool dal::Matrix::cellsAreCreated() const
{
  return !d_cells.empty();
}



//! Returns an opaque pointer to the cell values.
/*!
  \return    Opaque pointer to cell values. If no cell values are
             present 0 is returned.
*/
void const* dal::Matrix::cells() const
{
  assert(d_typeId != TI_NR_TYPES);

  void const* result = 0;

  if(cellsAreCreated()) {
    switch(d_typeId) {
      case TI_INT1 : result = cells<INT1>(); break;
      case TI_INT2 : result = cells<INT2>(); break;
      case TI_INT4 : result = cells<INT4>(); break;
      case TI_UINT1: result = cells<UINT1>(); break;
      case TI_UINT2: result = cells<UINT2>(); break;
      case TI_UINT4: result = cells<UINT4>(); break;
      case TI_REAL4: result = cells<REAL4>(); break;
      case TI_REAL8: result = cells<REAL8>(); break;
      case TI_UINT1_VECTOR: result = cells<UINT1_VECTOR>(); break;
      case TI_INT4_VECTOR: result = cells<INT4_VECTOR>(); break;
      case TI_REAL4_VECTOR: result = cells<REAL4_VECTOR>(); break;
      case TI_STRING: result = cells<std::string>(); break;
      case TI_NR_TYPES: assert(d_typeId != TI_NR_TYPES); break;
    }
  }

  return result;
}

//! Returns an opaque pointer to modify the cell values.
/*!
  \return    Opaque pointer to cell values. If no cell values are
             present 0 is returned.
*/
void* dal::Matrix::cells()
{
  assert(d_typeId != TI_NR_TYPES);

  void* result = 0;

  if(cellsAreCreated()) {
    switch(d_typeId) {
      case TI_INT1 : result = cells<INT1>(); break;
      case TI_INT2 : result = cells<INT2>(); break;
      case TI_INT4 : result = cells<INT4>(); break;
      case TI_UINT1: result = cells<UINT1>(); break;
      case TI_UINT2: result = cells<UINT2>(); break;
      case TI_UINT4: result = cells<UINT4>(); break;
      case TI_REAL4: result = cells<REAL4>(); break;
      case TI_REAL8: result = cells<REAL8>(); break;
      case TI_UINT1_VECTOR: result = cells<UINT1_VECTOR>(); break;
      case TI_INT4_VECTOR: result = cells<INT4_VECTOR>(); break;
      case TI_REAL4_VECTOR: result = cells<REAL4_VECTOR>(); break;
      case TI_STRING: result = cells<std::string>(); break;
      case TI_NR_TYPES: assert(d_typeId != TI_NR_TYPES); break;
    }
  }

  return result;
}



void dal::Matrix::setAllMV()
{
  assert(d_typeId != TI_NR_TYPES);

  if(cellsAreCreated()) {
    switch(d_typeId) {
      case TI_INT1 : { setAllMV<INT1>() ; break; }
      case TI_INT2 : { setAllMV<INT2>() ; break; }
      case TI_INT4 : { setAllMV<INT4>() ; break; }
      case TI_UINT1: { setAllMV<UINT1>(); break; }
      case TI_UINT2: { setAllMV<UINT2>(); break; }
      case TI_UINT4: { setAllMV<UINT4>(); break; }
      case TI_REAL4: { setAllMV<REAL4>(); break; }
      case TI_REAL8: { setAllMV<REAL8>(); break; }
      case TI_UINT1_VECTOR: { assert(false) /* setAllMV<UINT1_VECTOR>() */; break; }
      case TI_INT4_VECTOR: { assert(false) /* setAllMV<INT4_VECTOR>() */; break; }
      case TI_REAL4_VECTOR: { assert(false) /* setAllMV<REAL4_VECTOR>() */; break; }
      default: assert(false); break;
    }
  }
}


void dal::Matrix::createCells()
{
  switch(typeId()) {
    case TI_UINT1: {
      createCells<UINT1>();
      break;
    }
    case TI_UINT2: {
      createCells<UINT2>();
      break;
    }
    case TI_UINT4: {
      createCells<UINT4>();
      break;
    }
    case TI_INT1: {
      createCells<INT1>();
      break;
    }
    case TI_INT2: {
      createCells<INT2>();
      break;
    }
    case TI_INT4: {
      createCells<INT4>();
      break;
    }
    case TI_REAL4: {
      createCells<REAL4>();
      break;
    }
    case TI_REAL8: {
      createCells<REAL8>();
      break;
    }
    case TI_UINT1_VECTOR: {
      createCells<UINT1_VECTOR>();
      break;
    }
    case TI_INT4_VECTOR: {
      createCells<INT4_VECTOR>();
      break;
    }
    case TI_REAL4_VECTOR: {
      createCells<REAL4_VECTOR>();
      break;
    }
    default: { assert(false); break; }
  }
}



/*! Set the extremes
 *  Should set the extremes in accordance with the data
 */
void dal::Matrix::setExtremes(
         boost::any min,
         boost::any max)
{
  assert(min.empty() == max.empty());

  d_min = min;
  d_max = max;
  d_allMV = min.empty();
  d_hasExtremes = true;
}



void dal::Matrix::setExtremes()
{
  if(!cellsAreCreated()) {
    return;
  }

  d_hasExtremes = true;

  switch(typeId()) {
    case TI_UINT1: {
      findExtremes(cells<UINT1>(), nrCells(), d_min,d_max,d_allMV);
      break;
    }
    case TI_UINT2: {
      findExtremes(cells<UINT2>(), nrCells(), d_min,d_max,d_allMV);
      break;
    }
    case TI_UINT4: {
      findExtremes(cells<UINT4>(), nrCells(), d_min,d_max,d_allMV);
      break;
    }
    case TI_INT1: {
      findExtremes(cells<INT1>(), nrCells(), d_min,d_max,d_allMV);
      break;
    }
    case TI_INT2: {
      findExtremes(cells<INT2>(), nrCells(), d_min,d_max,d_allMV);
      break;
    }
    case TI_INT4: {
      findExtremes(cells<INT4>(), nrCells(), d_min,d_max,d_allMV);
      break;
    }
    case TI_REAL4: {
      findExtremes(cells<REAL4>(), nrCells(), d_min,d_max,d_allMV);
      break;
    }
    case TI_REAL8: {
      findExtremes(cells<REAL8>(), nrCells(), d_min,d_max,d_allMV);
      break;
    }
    case TI_UINT1_VECTOR: {
      assert(false);
      break;
    }
    case TI_INT4_VECTOR: {
      assert(false);
      break;
    }
    case TI_REAL4_VECTOR: {
      assert(false);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



//! get value of d_allMV
bool dal::Matrix::allMV() const
{
  return d_allMV;
}

bool dal::Matrix::hasExtremes() const
{
  return d_hasExtremes;
}



boost::any dal::Matrix::min() const
{
  return d_min;
}



boost::any dal::Matrix::max() const
{
  return d_max;
}



namespace dal {

void Matrix::eraseCells()
{
  switch(d_typeId) {
    case TI_INT1 : { eraseCells<INT1>();  break; }
    case TI_INT2 : { eraseCells<INT2>();  break; }
    case TI_INT4 : { eraseCells<INT4>();  break; }
    case TI_UINT1: { eraseCells<UINT1>(); break; }
    case TI_UINT2: { eraseCells<UINT2>(); break; }
    case TI_UINT4: { eraseCells<UINT4>(); break; }
    case TI_REAL4: { eraseCells<REAL4>(); break; }
    case TI_REAL8: { eraseCells<REAL8>(); break; }
    case TI_UINT1_VECTOR: { eraseCells<UINT1_VECTOR>(); break; }
    case TI_INT4_VECTOR: { eraseCells<INT4_VECTOR>(); break; }
    case TI_REAL4_VECTOR: { eraseCells<REAL4_VECTOR>(); break; }
    default: { assert(false); break; }
  }
}



void Matrix::copyCells(
         void const* cells)
{
  switch(d_typeId) {
    case TI_INT1 : { copyCells<INT1>(static_cast<INT1 const*>(cells));  break; }
    case TI_INT2 : { copyCells<INT2>(static_cast<INT2 const*>(cells));  break; }
    case TI_INT4 : { copyCells<INT4>(static_cast<INT4 const*>(cells));  break; }
    case TI_UINT1: { copyCells<UINT1>(static_cast<UINT1 const*>(cells)); break; }
    case TI_UINT2: { copyCells<UINT2>(static_cast<UINT2 const*>(cells)); break; }
    case TI_UINT4: { copyCells<UINT4>(static_cast<UINT4 const*>(cells)); break; }
    case TI_REAL4: { copyCells<REAL4>(static_cast<REAL4 const*>(cells)); break; }
    case TI_REAL8: { copyCells<REAL8>(static_cast<REAL8 const*>(cells)); break; }
    default: { assert(false); break; }
  }
}



//! Creates the datastructure for holding cell values.
/*!
  \return    Reference to the array.
  \warning   The datastructure must not already be created.
  \sa        eraseCells()

  The array will contain nrCells() values. Useful if the call assigns to the
  cells in the array.
*/
template<typename T>
inline T* Matrix::createCells()
{
  assert(!cellsAreCreated());

  T* pointer = new T[nrCells()];
  d_cells = pointer;

  assert(cells<T>()); 

  return pointer;
}



//! Accepts the ownership of \a cells.
/*!
  \param     cells Array to adopt, if ownership==TakeOwnership then cells
             must be allocated with new T[]
  \param     ownership Determines whether \a cells is ours to delete upon
             destruction or not.
  \warning   The current cells will be erased first.
  \sa        release()
*/
template<typename T>
inline void Matrix::transfer(
         T* cells,
         Ownership ownership)
{
  eraseCells<T>();
  d_cells = cells;
  d_ownership = ownership;
}



//! Releases ownership of the layered array of cell values.
/*!
  \return    Pointer to the array of cell values.
  \warning   An array of cell values must have been created.
  \sa        transfer(T*)
*/
template<typename T>
inline T* Matrix::release()
{
  assert(cellsAreCreated());

  T* pointer = boost::any_cast<T*>(d_cells);
  d_cells = boost::any();

  return pointer;
}



//! Returns a reference to the layered array with cell values.
/*!
  \return    Array with cell values.
  \warning   The array for cell values must have been created.

  This function assumes that there are cell values to return the array for.
  Use void const* dal::Matrix::cells() const to test whether this is the case.
*/
template<typename T>
inline T const* Matrix::cells() const
{
  assert(cellsAreCreated());

  T* pointer = boost::any_cast<T*>(d_cells);

  return pointer;
}



template<typename T>
inline PCR_DAL_DECL T* Matrix::cells()
{
  assert(cellsAreCreated());

  T* pointer = boost::any_cast<T*>(d_cells);

  return pointer;
}



template<typename T>
T const& Matrix::cell(
         size_t index) const
{
  assert(cellsAreCreated());

  return cells<T>()[index];
}



template<typename T>
T& Matrix::cell(
         size_t index)
{
  assert(cellsAreCreated());

  return cells<T>()[index];
}



template<typename T>
T const& Matrix::cell(
         size_t row,
         size_t col) const
{
  return cell<T>((row * d_nrCols) + col);
}



template<typename T>
T& Matrix::cell(
         size_t row,
         size_t col)
{
  return cell<T>((row * d_nrCols) + col);
}



//! Deletes the layered array for cell values.
/*!
  \sa        createCells()

  Nothing happens if the array has not been created.
*/
template<typename T>
inline void Matrix::eraseCells()
{
  if(cellsAreCreated() && d_ownership == TakeOwnership) {
    delete[] cells<T>();
  }

  d_cells = boost::any();
}



//! Fills the matrix with MV values.
/*!
  \warning   The cells must have been created already.

  allMV() will return true after calling this function. hasExtremes() will
  return false after calling this function.
*/
template<typename T>
inline void Matrix::setAllMV()
{
  assert(cellsAreCreated());
  T* pointer = cells<T>();

  for(size_t i = 0; i < nrCells(); ++i) {
    pcr::setMV(pointer[i]);
  }

  d_allMV = true;
  d_hasExtremes = false;
  d_min = boost::any();
  d_max = boost::any();
}



//! Set missing value cells based on the missing values in \a matrix.
/*!
  \param     matrix Matrix to take missing value cells from.
  \exception .
  \warning   Missing valueness of the whole matrix and extreme values are not determined.
  \sa        .

  For each cell in \a matrix that contains a missing value a corresponding
  missing value is set in *this. Other cells in *this are not touched.
*/
template<typename T>
inline void Matrix::takeMV(
         Matrix const& matrix)
{
  assert(cellsAreCreated());
  assert(matrix.cellsAreCreated());
  assert(matrix.nrCells() == nrCells());

  T const* source = matrix.cells<T>();
  T* destination = cells<T>();

  for(size_t i = 0; i < nrCells(); ++i) {
    if(pcr::isMV(source[i])) {
      pcr::setMV(destination[i]);
    }
  }
}



//! Fills the matrix with \a value values.
/*!
  \warning   The cells must have been created already.
*/
template<typename T>
inline void Matrix::fill(T const& value)
{
  assert(cellsAreCreated());

  T* pointer = cells<T>();

  for(size_t i = 0; i < nrCells(); ++i) {
    pointer[i] = value;
  }
}



template<typename T>
inline T Matrix::min() const
{
  return boost::any_cast<T>(d_min);
}

template<typename T>
inline T Matrix::max() const
{
  return boost::any_cast<T>(d_max);
}



template<typename T>
inline void Matrix::copyCells(T const* cells)
{
  assert(cellsAreCreated());

  d_cells = copyNrCells(this->cells<T>(), cells, nrCells());
}



// KDJ 060213
// I ran into a runtime issue where two Python extensions created with
// Boost.Python conflicted with each other. The problem surfaced in the
// cells<T>() member of the Matrix class and seemed to be related to the fact
// that an application (python) linked / loaded two shared libraries
// (pcrasterpy, pcrblockpy) with each had their own set of instantiated
// templates. Using RTTI to compare these instantiated types gives unexpected
// results because comparison is done on the address instead of the content
// of the type (gcc > 3.2 I believe). boost::any_cast suffers from this.
// See also
// - http://wiki.python.org/moin/boost.python/CrossExtensionModuleDependencies
// - http://gcc.gnu.org/faq.html#dso
#define InstantiateTemplateMembers(type) \
template type* Matrix::createCells<type>(); \
template type* Matrix::release<type>(); \
template type const* Matrix::cells<type>() const; \
template type* Matrix::cells<type>(); \
template PCR_DAL_DECL type Matrix::min<type>() const; \
template PCR_DAL_DECL type Matrix::max<type>() const; \
template void Matrix::copyCells<type>(type const*); \
template void Matrix::transfer<type>(type*, Ownership); \
template void Matrix::eraseCells<type>(); \
template PCR_DAL_DECL void Matrix::fill<type>(type const&); \
template PCR_DAL_DECL type const& Matrix::cell<type>(size_t) const; \
template PCR_DAL_DECL type& Matrix::cell<type>(size_t); \
template PCR_DAL_DECL type const& Matrix::cell<type>(size_t, size_t) const; \
template PCR_DAL_DECL type& Matrix::cell<type>(size_t, size_t);

InstantiateTemplateMembers(UINT1)
InstantiateTemplateMembers(UINT2)
InstantiateTemplateMembers(UINT4)
InstantiateTemplateMembers(INT1)
InstantiateTemplateMembers(INT2)
InstantiateTemplateMembers(INT4)
InstantiateTemplateMembers(REAL4)
InstantiateTemplateMembers(REAL8)
InstantiateTemplateMembers(UINT1_VECTOR)
InstantiateTemplateMembers(INT4_VECTOR)
InstantiateTemplateMembers(REAL4_VECTOR)

#define InstantiateTemplateMembersForPCRTypesOnly(type) \
template void Matrix::setAllMV<type>(); \
template void Matrix::takeMV<type>(Matrix const&);

InstantiateTemplateMembersForPCRTypesOnly(UINT1)
InstantiateTemplateMembersForPCRTypesOnly(UINT2)
InstantiateTemplateMembersForPCRTypesOnly(UINT4)
InstantiateTemplateMembersForPCRTypesOnly(INT1)
InstantiateTemplateMembersForPCRTypesOnly(INT2)
InstantiateTemplateMembersForPCRTypesOnly(INT4)
InstantiateTemplateMembersForPCRTypesOnly(REAL4)
InstantiateTemplateMembersForPCRTypesOnly(REAL8)

} // namespace dal



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



