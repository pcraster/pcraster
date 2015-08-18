#ifndef INCLUDED_DAL_TABLE
#define INCLUDED_DAL_TABLE



// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_ANY
#include <boost/any.hpp>
#define INCLUDED_BOOST_ANY
#endif

#ifndef INCLUDED_BOOST_LEXICAL_CAST
#include <boost/lexical_cast.hpp>
#define INCLUDED_BOOST_LEXICAL_CAST
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_ARRAY
#include "dal_Array.h"
#define INCLUDED_DAL_ARRAY
#endif

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

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h" // copyCells
#define INCLUDED_DAL_UTILS
#endif



namespace dal {
  // Table declarations.
}



namespace dal {



//! This is the Dataset class for Tabular data.
/*!
  A table is a set of data arranged in records and columns. Per column all
  values are of the same type. Between columns the types can differ.

  This class supports table columns with an optional title.

  \todo Refactor, rename, document.
*/
class PCR_DAL_DECL Table: public Dataset
{

  friend class TableTest;

private:

  //! Title of the table, possibly empty.
  std::string      d_title;

  //! Titles of the columns, possibly empty.
  std::vector<std::string> d_titles;

  //! Type of each column.
  std::vector<TypeId> d_typeIds;

  //! Columns with data.
  std::vector<boost::any> d_cols;

  template<typename T>
  void             create              (size_t col);

  void             create              (size_t col,
                                        TypeId typeId);

  void             appendRec           (size_t col);

  template<typename T>
  void             appendRec           (size_t col);

  // template<typename T>
  // Array<T>&        appendCol           ();

  template<typename T>
  Array<T>&        appendCol           (std::string const& title);

  void             skipCol             (size_t col,
                                        std::string const& title);

  void             copyCols            (Table const& table);

  void             copyCol             (Table const& table,
                                        size_t col);
  template<typename T>
  void             copyCol             (Table const& table,
                                        size_t col);

  bool             isCreated           (size_t col) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Table               ();

                   Table               (std::string const& title,
                                        std::vector<std::string> const& titles);

                   Table               (std::vector<std::string> const& titles);

                   Table               (std::vector<TypeId> const& typeIds);

                   Table               (std::vector<std::string> const& titles,
                                        std::vector<TypeId> const& typeIds);

                   Table               (std::string const& title,
                                        std::vector<std::string> const& titles,
                                        std::vector<TypeId> const& typeIds);

                   Table               (Table const& rhs);

  /* virtual */    ~Table              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             init                (std::vector<std::string> const& titles);

  void             init                (std::string const& title,
                                        std::vector<std::string> const& titles);

  void             init                (std::vector<TypeId> const& typeIds);

  void             init                (std::vector<std::string> const& titles,
                                        std::vector<TypeId> const& typeIds);

  void             init                (std::string const& title,
                                        std::vector<std::string> const& titles,
                                        std::vector<TypeId> const& typeIds);

  Table&           operator=           (Table const& rhs);

  void             appendRec           ();

  void             insertCol           (size_t col,
                                        std::string const& title,
                                        TypeId typeId);

  template<typename T>
  Array<T>&        insertCol           (size_t col,
                                        std::string const& title);

  void             appendCol           (std::string const& title,
                                        TypeId typeId);

  void             appendCol           (Table const& table,
                                        size_t col);

  template<typename T>
  Array<T>&        appendCol           ();

  template<typename T>
  void             takeCol             (size_t destinationCol,
                                        Table& table,
                                        size_t sourceCol);

  template<typename T>
  void             takeCol             (size_t destinationCol,
                                        Array<T>& array);

  void             insert              (size_t col,
                                        TypeId typeId);

  void             erase               (size_t col);

  void             erase               (size_t col,
                                        size_t size);

  template<typename T>
  void             erase               (size_t col);

  template<typename T>
  T*               release             (size_t col);

  void             setTitle            (std::string const& title);

  void             setTitles           (std::vector<std::string> const& titles);

  void             setTitle            (size_t col,
                                        std::string const& title);

  void             setTypeIds          (TypeId typeId);

  void             setTypeId           (size_t col,
                                        TypeId typeId);

  // void             setTypeIds          (std::vector<TypeId> const& typeIds);

  void             createCols          ();

  void             clear               ();

  void             resize              (size_t nrRecs);

  void             setAllMV            ();

  template<typename T>
  void             assign              (size_t colToWrite,
                                        size_t joinFrom,
                                        Table const& table,
                                        size_t colToRead,
                                        size_t joinTo);

  void             assign              (size_t colToWrite,
                                        size_t joinFrom,
                                        Table const& table,
                                        size_t colToRead,
                                        size_t joinTo);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             equals              (Table const& table) const;

  template<typename T>
  T const&         cell                (size_t rec,
                                        size_t col) const;

  bool             colsAreCreated      () const;

  size_t           nrCols              () const;

  size_t           nrRecs              () const;

  size_t           indexOf             (std::string const& name) const;

  TypeId           typeId              (size_t col) const;

  template<typename T>
  Array<T> const&  col                 (size_t col) const;

  template<typename T>
  Array<T>&        col                 (size_t col);

  std::string const& title             () const;

  std::string const& title             (size_t col) const;

  std::vector<std::string> const& titles() const;

  std::string      asString            (size_t rec,
                                        size_t col) const;

  template<typename T>
  std::string      asString            (size_t rec,
                                        size_t col) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Creates an empty table.
*/
inline Table::Table()

  : Dataset(TABLE)

{
}

//! Constructor.
/*!
  \param     title Title of the table.
  \param     titles Titles of the columns.
  \warning   The columns for the data cannot be accessed yet after the
             table object is constructed. First columns need to be created.
  \sa        appendCol(), appendCol(std::string const&)
*/
inline Table::Table(std::string const& title,
         std::vector<std::string> const& titles)

  : Dataset(TABLE)

{
  init(title, titles);
}

//! Constructor.
/*!
  \param     titles Titles of the columns.
  \warning   The columns for the data cannot be accessed yet after the
             table object is constructed. First columns need to be created.
  \sa        appendCol(), appendCol(std::string const&)
*/
inline Table::Table(// std::string const& name,
         std::vector<std::string> const& titles)

  : Dataset(TABLE)

{
  init(titles);
}

//  \param     name Name of the dataset.
//! Constructor.
/*!
  \param     typeIds Type ids of the columns.
  \warning   The columns for the data cannot be accessed yet after the
             table object is constructed. First columns need to be created.
  \sa        createCols(), appendCol(), appendCol(std::string const&)

  Column titles will be empty.
*/
inline Table::Table(// std::string const& name,
         std::vector<TypeId> const& typeIds)

  : Dataset(TABLE)

{
  init(typeIds);
}

//! Constructor.
/*!
  \param     titles Titles of the columns.
  \param     typeIds Type ids of the columns.
  \warning   The columns for the data cannot be accessed yet after the
             table object is constructed. First columns need to be created.
  \sa        createCols(), appendCol(), appendCol(std::string const&)
*/
inline Table::Table(// std::string const& name,
         std::vector<std::string> const& titles,
         std::vector<TypeId> const& typeIds)

  : Dataset(TABLE)

{
  init(titles, typeIds);
}

inline Table::Table(
         std::string const& title,
         std::vector<std::string> const& titles,
         std::vector<TypeId> const& typeIds)

  : Dataset(TABLE)

{
  init(title, titles, typeIds);
}

//! Copy constructor.
/*!
  \param     rhs Object to copy from.
*/
inline Table::Table(
         Table const& rhs)
  : Dataset(rhs),
    d_title(rhs.d_title)
    // d_titles(rhs.d_titles),
    // d_typeIds(rhs.d_typeIds)
{
  copyCols(rhs);
}

//! Assignment operator.
inline Table& Table::operator=(Table const& rhs)
{
  if(this != &rhs) {
    dynamic_cast<Dataset&>(*this) = rhs;
    d_title = rhs.d_title;
    // d_titles = rhs.d_titles;
    copyCols(rhs);
  }

  return *this;
}

/*!
  \overload

  Title will be empty and the type ids of the columns will be TI_NR_TYPES.
*/
inline void Table::init(std::vector<std::string> const& titles)
{
  init("", titles, std::vector<TypeId>(titles.size(), TI_NR_TYPES));
}

/*!
  \overload

  Type ids of the columns will be TI_NR_TYPES.
*/
inline void Table::init(std::string const& title,
         std::vector<std::string> const& titles)
{
  init(title, titles, std::vector<TypeId>(titles.size(), TI_NR_TYPES));
}

/*!
  \overload

  Title and column titles will be empty.
*/
inline void Table::init(std::vector<TypeId> const& typeIds)
{
  init("", std::vector<std::string>(typeIds.size()), typeIds);
}

/*!
  \overload

  Title will be empty.
*/
inline void Table::init(std::vector<std::string> const& titles,
         std::vector<TypeId> const& typeIds)
{
  init("", titles, typeIds);
}

//! Initialises the table object.
/*!
  \param     title Title of the table.
  \param     titles Titles of the columns.
  \param     typeIds Type id s of the columns.
  \warning   Columns for the data values are not yet created.
  \sa        createCols(), appendCol(), appendCol(std::string const&)
*/
inline void Table::init(std::string const& title,
         std::vector<std::string> const& titles,
         std::vector<TypeId> const& typeIds)
{
  assert(titles.size() == typeIds.size());
  d_title = title;
  d_titles = titles;
  d_typeIds = typeIds;
}

//! Destructor.
/*!
  All columns are deleted. If you want to keep hold of the data call
  release(size_t) for each column.
*/
inline Table::~Table()
{
  erase(0, d_cols.size());

  assert(d_cols.empty());
}

//! Returns the number of columns in the table.
/*!
  \return    Number of columns.
  \sa        nrRecs()

  The number of columns equals the maximum of the number of column titles
  set and the number of type ids set.
*/
inline size_t Table::nrCols() const
{
  assert(d_cols.size() <= d_titles.size());
  assert(d_cols.size() <= d_typeIds.size());

  return std::max<>(d_titles.size(), d_typeIds.size());
}

//! Returns the number of records in the table.
/*!
  \return    Number of records.
  \warning   This is a relative expensive operation.
  \sa        nrCols()

  It is assumed that the number of values in the first column which has a
  valid type id set, equals the number of records in the table. If no such
  column exists, 0 is returned.
*/
inline size_t Table::nrRecs() const
{
  if(nrCols() == 0 || d_cols.empty()) {
    return 0;
  }

  size_t nrRecs = 0;

  for(size_t i = 0; i < d_cols.size(); ++i) {
    if(d_typeIds[i] != TI_NR_TYPES) {
      switch(d_typeIds[i]) {
        case TI_INT1 : nrRecs = col<INT1>(i).size(); break;
        case TI_INT4 : nrRecs = col<INT4>(i).size(); break;
        case TI_UINT1: nrRecs = col<UINT1>(i).size(); break;
        case TI_UINT2: nrRecs = col<UINT2>(i).size(); break;
        case TI_UINT4: nrRecs = col<UINT4>(i).size(); break;
        case TI_REAL4: nrRecs = col<REAL4>(i).size(); break;
        case TI_REAL8: nrRecs = col<REAL8>(i).size(); break;
        case TI_STRING: nrRecs = col<std::string>(i).size(); break;
        default: assert(false); break;
      }

      break;
    }
  }

  return nrRecs;
}

inline size_t Table::indexOf(
         std::string const& name) const
{
  size_t result = 0;

  while(result < d_titles.size() && d_titles[result] != name) {
    ++result;
  }

  return result;
}

//! Returns the type id of column \a col.
/*!
  \param     col Column index.
  \return    Type id.
*/
inline TypeId Table::typeId(size_t col) const
{
  assert(col < d_typeIds.size());

  return d_typeIds[col];
}

//! Sets the title of the table to \a title.
/*!
  \param     title New title.
*/
inline void Table::setTitle(std::string const& title)
{
  d_title = title;
}

//! Sets the titles of the columns to \a titles.
/*!
  \param     titles New titles.
*/
inline void Table::setTitles(std::vector<std::string> const& titles)
{
  d_titles = titles;
}

inline void Table::setTitle(
         size_t col,
         std::string const& title)
{
  assert(col < d_titles.size());

  d_titles[col] = title;
}

inline bool Table::colsAreCreated() const
{
  return d_typeIds.size() == 0 || d_cols.size() > 0;
}

//! Creates columns for the data values.
/*!
  \warning   Columns must not already have been created. Columns with type id
             TI_NR_TYPES are skipped.
  \sa        appendCol(), appendCol(std::string const&)

  As a side effect empty column titles are created if they are not already set.
*/
inline void Table::createCols()
{
  if(d_titles.empty()) {
    d_titles.resize(d_typeIds.size());
  }

  assert(d_titles.size() == d_typeIds.size());
  assert(d_cols.size() == 0);

  d_cols.resize(d_typeIds.size());

  for(size_t col = 0; col < d_cols.size(); ++col) {
    create(col, d_typeIds[col]);
  }

  assert(d_titles.size() == d_cols.size());
  assert(d_typeIds.size() == d_cols.size());
}

//! Creates column \a col using type id \a typeId.
/*!
  \param     .
  \return    .
  \exception .
  \warning   It is assumed that the column is not created already.
  \sa        .

  This function also assigns the typeId for \a col to \a typeId.

  No column is created if typeId equals TI_NR_TYPES.

  The new column will be empty.
*/
inline void Table::create(
         size_t col,
         TypeId typeId)
{
  switch(typeId) {
    case TI_INT1:   { create<INT1>(col);   break; }
    case TI_INT2:   { create<INT2>(col);   break; }
    case TI_INT4:   { create<INT4>(col);   break; }
    case TI_UINT1:  { create<UINT1>(col);  break; }
    case TI_UINT2:  { create<UINT2>(col);  break; }
    case TI_UINT4:  { create<UINT4>(col);  break; }
    case TI_REAL4:  { create<REAL4>(col);  break; }
    case TI_REAL8:  { create<REAL8>(col);  break; }
    case TI_STRING: { create<std::string>(col); break; }
    // Skip column: not created.
    default:        { d_typeIds[col] = TI_NR_TYPES; break; }
  }

  assert(col < d_typeIds.size());
  d_typeIds[col] = typeId;
}

//! Creates column \a col.
/*!
  The new column will be empty.
*/
template<typename T>
inline void Table::create(
         size_t col)
{
  assert(!isCreated(col));
  assert(col < d_cols.size());

  d_cols[col] = new Array<T>();
}

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
inline void Table::copyCols(
         Table const& table)
{
  assert(table.d_typeIds.size() == table.d_cols.size());
  assert(table.d_typeIds.size() == table.d_titles.size());
  assert(d_typeIds.size() == d_cols.size());
  assert(d_typeIds.size() == d_titles.size());

  for(size_t col = 0; col < std::min<size_t>(nrCols(), table.nrCols()); ++col) {
    // Copy columns from cols to already created columns in *this.
    copyCol(table, col);
  }

  if(nrCols() > table.nrCols()) {
    // Erase columns from *this which are not present in cols.
    erase(table.nrCols(), nrCols() - table.nrCols());
  }
  else {
    // Append columns from cols to columns in *this.
    for(size_t col = nrCols(); col < table.nrCols(); ++col) {
      appendCol(table, col);
    }
  }

  assert(d_titles == table.d_titles);
  assert(d_typeIds == table.d_typeIds);
  assert(d_cols.size() == table.d_cols.size());
  assert(nrRecs() == table.nrRecs());
}

//! Copy column \a col from \a table to column \a col.
/*!
  \param     table Table to copy from.
  \param     col Index of column to copy.

  All information about the column is copied: title, typeId and the column
  itself, with the values.
*/
inline void Table::copyCol(
         Table const& table,
         size_t col)
{
  if(!isCreated(col)) {
    if(table.isCreated(col)) {
      create(col, table.typeId(col));
    }
  }
  else {
    if(!table.isCreated(col)) {
      // replaceCol(col, table.typeIds(col));
      erase(col);
      insert(col, table.typeId(col));
    }
    else {
      if(typeId(col) != table.typeId(col)) {
        // replaceCol(col, table.typeIds(col));
        erase(col);
        insert(col, table.typeId(col));
      }
    }
  }

  switch(table.typeId(col)) {
    case TI_INT1:   { copyCol<INT1>(table, col);   break; }
    case TI_INT2:   { copyCol<INT2>(table, col);   break; }
    case TI_INT4:   { copyCol<INT4>(table, col);   break; }
    case TI_UINT1:  { copyCol<UINT1>(table, col);  break; }
    case TI_UINT2:  { copyCol<UINT2>(table, col);  break; }
    case TI_UINT4:  { copyCol<UINT4>(table, col);  break; }
    case TI_REAL4:  { copyCol<REAL4>(table, col);  break; }
    case TI_REAL8:  { copyCol<REAL8>(table, col);  break; }
    case TI_STRING: { copyCol<std::string>(table, col); break; }
    case TI_NR_TYPES: { /* skip column */ break; }
    default:        { assert(false); break; }
  }
}

//! Copies column \a col from \a table to \a col.
/*!
  \param     table Table to copy column from.
  \param     col Column id to copy.
  \warning   Destination column must have been created already (but the size
             does not matter).
*/
template<typename T>
inline void Table::copyCol(
         Table const& table,
         size_t col)
{
  assert(col < this->nrCols());
  assert(col < table.nrCols());

  Array<T> const& source(table.col<T>(col));
  Array<T>& destination(this->col<T>(col));

  destination.resize(source.size());
  copyCells(source.begin(), source.end(), destination.begin());
}

inline void Table::clear()
{
  assert(d_cols.size() == nrCols());

  for(size_t col = 0; col < nrCols(); ++col) {
    switch(typeId(col)) {
      case TI_INT1: {
        this->col<INT1>(col).clear();
        break;
      }
      case TI_INT2: {
        this->col<INT2>(col).clear();
        break;
      }
      case TI_INT4: {
        this->col<INT4>(col).clear();
        break;
      }
      case TI_UINT1: {
        this->col<UINT1>(col).clear();
        break;
      }
      case TI_UINT2: {
        this->col<UINT2>(col).clear();
        break;
      }
      case TI_UINT4: {
        this->col<UINT4>(col).clear();
        break;
      }
      case TI_REAL4: {
        this->col<REAL4>(col).clear();
        break;
      }
      case TI_REAL8: {
        this->col<REAL8>(col).clear();
        break;
      }
      case TI_STRING: {
        this->col<std::string>(col).clear();
        break;
      }
      default: { /* skip */ break; }
    }
  }
}

inline void Table::resize(size_t nrRecs)
{
  for(size_t col = 0; col < nrCols(); ++col) {
    switch(typeId(col)) {
      case TI_INT1: {
        this->col<INT1>(col).resize(nrRecs);
        break;
      }
      case TI_INT2: {
        this->col<INT2>(col).resize(nrRecs);
        break;
      }
      case TI_INT4: {
        this->col<INT4>(col).resize(nrRecs);
        break;
      }
      case TI_UINT1: {
        this->col<UINT1>(col).resize(nrRecs);
        break;
      }
      case TI_UINT2: {
        this->col<UINT2>(col).resize(nrRecs);
        break;
      }
      case TI_UINT4: {
        this->col<UINT4>(col).resize(nrRecs);
        break;
      }
      case TI_REAL4: {
        this->col<REAL4>(col).resize(nrRecs);
        break;
      }
      case TI_REAL8: {
        this->col<REAL8>(col).resize(nrRecs);
        break;
      }
      case TI_STRING: {
        this->col<std::string>(col).resize(nrRecs);
        break;
      }
      default: { /* skip */ break; }
    }
  }
}

inline bool Table::equals(Table const& table) const
{
  if(d_title == table.d_title && d_titles == table.d_titles &&
         d_typeIds == table.d_typeIds && d_cols.size() == table.d_cols.size()) {
    for(size_t col = 0; col < d_cols.size(); ++col) {
      switch(typeId(col)) {
        case TI_INT1: {
          if(this->col<INT1>(col) != table.col<INT1>(col)) {
            return false;
          }
          break;
        }
        case TI_INT2: {
          if(this->col<INT2>(col) != table.col<INT2>(col)) {
            return false;
          }
          break;
        }
        case TI_INT4: {
          if(this->col<INT4>(col) != table.col<INT4>(col)) {
            return false;
          }
          break;
        }
        case TI_UINT1: {
          if(this->col<UINT1>(col) != table.col<UINT1>(col)) {
            return false;
          }
          break;
        }
        case TI_UINT2: {
          if(this->col<UINT2>(col) != table.col<UINT2>(col)) {
            return false;
          }
          break;
        }
        case TI_UINT4: {
          if(this->col<UINT4>(col) != table.col<UINT4>(col)) {
            return false;
          }
          break;
        }
        case TI_REAL4: {
          if(this->col<REAL4>(col) != table.col<REAL4>(col)) {
            return false;
          }
          break;
        }
        case TI_REAL8: {
          if(this->col<REAL8>(col) != table.col<REAL8>(col)) {
            return false;
          }
          break;
        }
        case TI_STRING: {
          if(this->col<std::string>(col) != table.col<std::string>(col)) {
            return false;
          }
          break;
        }
        case TI_NR_TYPES: { /* skip */ break; }
        default: { assert(false); break; }
      }
    }

    return true;
  }

  return false;
}

//! Appends an empty placeholder column to the table.
/*!
  \warning   Never try to reference this column: it doesn't exist.
*/
inline void Table::skipCol(
         size_t col,
         std::string const& title)
{
  assert(d_titles.size() == d_cols.size());
  assert(d_typeIds.size() == d_cols.size());

  d_titles.insert(d_titles.begin() + col, title);
  d_typeIds.insert(d_typeIds.begin() + col, TI_NR_TYPES);
  d_cols.insert(d_cols.begin() + col, boost::any());

  // d_titles.push_back(title);
  // d_typeIds.push_back(TI_NR_TYPES);
  // d_cols.push_back(boost::any());

  assert(d_titles.size() == d_cols.size());
  assert(d_typeIds.size() == d_cols.size());
}

inline void Table::insertCol(
         size_t col,
         std::string const& title,
         TypeId typeId)
{
  assert(d_titles.size() == d_cols.size());
  assert(d_typeIds.size() == d_cols.size());

  switch(typeId) {
    case TI_INT1:  { insertCol<INT1>(col, title);  break; }
    case TI_INT2:  { insertCol<INT2>(col, title);  break; }
    case TI_INT4:  { insertCol<INT4>(col, title);  break; }
    case TI_UINT1: { insertCol<UINT1>(col, title); break; }
    case TI_UINT2: { insertCol<UINT2>(col, title); break; }
    case TI_UINT4: { insertCol<UINT4>(col, title); break; }
    case TI_REAL4: { insertCol<REAL4>(col, title); break; }
    case TI_REAL8: { insertCol<REAL8>(col, title); break; }
    case TI_STRING: { insertCol<std::string>(col, title); break; }
    default: { skipCol(col, title); break; }
  }

  assert(d_titles.size() == d_cols.size());
  assert(d_typeIds.size() == d_cols.size());
}

template<typename T>
inline Array<T>& Table::insertCol(
         size_t col,
         std::string const& title)
{
  assert(d_titles.size() == d_cols.size());
  assert(d_typeIds.size() == d_cols.size());

  Array<T>* array = new Array<T>(nrRecs());

  d_titles.insert(d_titles.begin() + col, title);
  d_typeIds.insert(d_typeIds.begin() + col, TypeTraits<T>::typeId);
  d_cols.insert(d_cols.begin() + col, array);

  assert(d_titles.size() == d_cols.size());
  assert(d_typeIds.size() == d_cols.size());

  return *array;
}

inline void Table::appendCol(
         Table const& table,
         size_t col)
{
  appendCol(table.title(col), table.typeId(col));
  copyCol(table, col);
}

inline void Table::appendCol(
         std::string const& title,
         TypeId typeId)
{
  assert(d_titles.size() == d_cols.size());
  assert(d_typeIds.size() == d_cols.size());

  // d_titles.push_back(title);
  // d_typeIds.push_back(typeId);

  switch(typeId) {
    case TI_INT1:  { appendCol<INT1>(title);  break; }
    case TI_INT2:  { appendCol<INT2>(title);  break; }
    case TI_INT4:  { appendCol<INT4>(title);  break; }
    case TI_UINT1: { appendCol<UINT1>(title); break; }
    case TI_UINT2: { appendCol<UINT2>(title); break; }
    case TI_UINT4: { appendCol<UINT4>(title); break; }
    case TI_REAL4: { appendCol<REAL4>(title); break; }
    case TI_REAL8: { appendCol<REAL8>(title); break; }
    case TI_STRING: { appendCol<std::string>(title); break; }
    default: { skipCol(d_cols.size(), title); break; }
  }

  assert(d_titles.size() == d_cols.size());
  assert(d_typeIds.size() == d_cols.size());
}

// //! Creates and appends a column to the table.
// /*!
//   \return    Reference to the created column.
//   \sa        appendCol(std::string const&)
// 
//   It is assumed that a title for this column is already set.
// 
//   The column will have nrRecs() default constructed values.
// */
// template<typename T>
// inline Array<T>& Table::appendCol()
// {
//   assert(d_titles.size() > d_cols.size());
// 
//   Array<T>* array = new Array<T>(nrRecs());
// 
//   d_cols.push_back(array);
// 
//   return *array;
// }

//! Creates and appends a column with title \a title to the table.
/*!
  \return    Reference to the created column.
  \warning   The number of titles  and the number of created columns must be
             equal to each other.
  \sa        appendCol()
*/
template<typename T>
inline Array<T>& Table::appendCol(
         std::string const& title)
{
  assert(d_titles.size() == d_cols.size());

  Array<T>* array = new Array<T>(nrRecs());

  d_titles.push_back(title);
  d_typeIds.push_back(TypeTraits<T>::typeId);
  d_cols.push_back(array);

  return *array;
}

//! Creates and appends a column to the table.
/*!
  \param     .
  \return    Reference to the created column.
  \exception .
  \warning   .
  \sa        .

  An empty column title is added. The type id corresponding to T is added.
*/
template<typename T>
inline Array<T>& Table::appendCol()
{
  return appendCol<T>("");
}

//! Appends a record to the table.
/*!
*/
inline void Table::appendRec()
{
  for(size_t col = 0; col < nrCols(); ++col) {
    appendRec(col);
  }
}

//! Appends a record for column \a col to the table.
/*!
  \param     col Column index.

  Columns with an invalid type id set are skipped.
*/
inline void Table::appendRec(size_t col)
{
  switch(typeId(col)) {
    case TI_INT1:  { appendRec<INT1>(col);  break; }
    case TI_INT2:  { appendRec<INT2>(col);  break; }
    case TI_INT4:  { appendRec<INT4>(col);  break; }
    case TI_UINT1: { appendRec<UINT1>(col); break; }
    case TI_UINT2: { appendRec<UINT2>(col); break; }
    case TI_UINT4: { appendRec<UINT4>(col); break; }
    case TI_REAL4: { appendRec<REAL4>(col); break; }
    case TI_REAL8: { appendRec<REAL8>(col); break; }
    case TI_STRING: { appendRec<std::string>(col); break; }
    default: { /* skip */ break; }
  }
}

//! Appends a record for column \a col to the table.
/*!
  \param     col Column index.
  \exception boost::bad_any_cast When T does not correspond to type id of column \a col.

  The new record is filled with a default value for the types stored in the
  column.
*/
template<typename T>
inline void Table::appendRec(size_t col)
{
  Array<T>* array = boost::any_cast<Array<T>*>(d_cols[col]);
  array->push_back(T());
}

inline void Table::insert(
         size_t col,
         TypeId typeId)
{
  assert(col < d_typeIds.size());
  assert(col < d_cols.size());

  d_typeIds.insert(d_typeIds.begin() + col, typeId);
  d_cols.insert(d_cols.begin() + col, boost::any());
  create(col, typeId);
}

inline void Table::erase(size_t col)
{
  erase(col, 1);
}

//! Erases \a size columns, starting with column \a col.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  This function also erases the titles and typeIds of the erased columns.
*/
inline void Table::erase(
         size_t col,
         size_t size)
{
  size_t end = col + size;

  for(; col < end; --end) {
    TypeId typeId = d_typeIds[col];

    switch(typeId) {
      case TI_INT1:   { erase<INT1>(col);   break; }
      case TI_INT2:   { erase<INT2>(col);   break; }
      case TI_INT4:   { erase<INT4>(col);   break; }
      case TI_UINT1:  { erase<UINT1>(col);  break; }
      case TI_UINT2:  { erase<UINT2>(col);  break; }
      case TI_UINT4:  { erase<UINT4>(col);  break; }
      case TI_REAL4:  { erase<REAL4>(col);  break; }
      case TI_REAL8:  { erase<REAL8>(col);  break; }
      case TI_STRING: { erase<std::string>(col); break; }
      case TI_NR_TYPES: {
        assert(typeId == TI_NR_TYPES);
        assert(col < d_cols.size());
        assert(!isCreated(col));

        d_cols.erase(d_cols.begin() + col);
        d_titles.erase(d_titles.begin() + col);
        d_typeIds.erase(d_typeIds.begin() + col);

        break;
      }
      default: { assert(false); break;}
    }
  }
}

//! Erases the column with data from the table.
/*!
  \param     col Column index.

  The column title and typeId are also erased.
*/
template<typename T>
inline void Table::erase(size_t col)
{
  assert(col < d_titles.size());

  // Test whether this column is already created.
  if(col < d_cols.size()) {
    if(!d_cols[col].empty()) {
      Array<T>* array = boost::any_cast<Array<T>*>(d_cols[col]);
      delete array;
    }

    d_cols.erase(d_cols.begin() + col);
  }

  d_titles.erase(d_titles.begin() + col);
  d_typeIds.erase(d_typeIds.begin() + col);
}

//! Returns a pointer to the data values of column \a col.
/*!
  \param     col Column index.
  \return    Pointer to data.
  \warning   No effort is made to keep the Table object in a valid state. You
             can use release(size_t) to get ownership of the pointers to the
             data values of each column, but you shouldn't continue using the
             Table object. Make this the last thing you do with a table object.
  \exception boost::bad_any_cast When T does not correspond to type id of column \a col.
*/
template<typename T>
T* Table::release(size_t col)
{
  Array<T>* array = boost::any_cast<Array<T>*>(d_cols[col]);
  return array->release();
}

//! Returns the column with index \a col.
/*!
  \param     col Column index.
  \return    Const reference to the column.
  \exception boost::bad_any_cast When T does not correspond to type id of column \a col.
*/
template<typename T>
inline Array<T> const& Table::col(size_t col) const
{
  assert(col < d_cols.size());

  return *boost::any_cast<Array<T>*>(d_cols[col]);
}

//! Returns the column with index \a col.
/*!
  \param     col Column index.
  \return    Reference to the column.
  \exception boost::bad_any_cast When T does not correspond to type id of column \a col.
*/
template<typename T>
inline Array<T>& Table::col(size_t col)
{
  assert(col < d_cols.size());

  return *boost::any_cast<Array<T>*>(d_cols[col]);
}

//! Returns the title of the table.
/*!
  \return    Title.
*/
inline std::string const& Table::title() const
{
  return d_title;
}

//! Returns the title of the columns with index \a col.
/*!
  \param     col Column index.
  \return    Title.
*/
inline std::string const& Table::title(size_t col) const
{
  assert(col < d_titles.size());

  return d_titles[col];
}

//! Sets the type id of all columns to \a typeId.
/*!
  \param     typeId New type id.
  \sa        setTypeId(size_t, TypeId)
*/
inline void Table::setTypeIds(TypeId typeId)
{
  for(size_t col = 0; col < d_typeIds.size(); ++col) {
    d_typeIds[col] = typeId;
  }
}

//! Sets the type id of column \a col to \a typeId.
/*!
  \param     col Column index.
  \param     typeId New type id.
  \warning   Column \a col must not already be created.
  \sa        setTypeIds(TypeId)
*/
inline void Table::setTypeId(size_t col, TypeId typeId)
{
  assert(col < nrCols());
  assert(col >= d_cols.size());

  d_typeIds[col] = typeId;
}

//! Returns the cell value at column \a col and record \a rec.
/*!
  \param     rec Record index.
  \param     col Column index.
  \return    Cell value.
*/
template<typename T>
inline T const& Table::cell(
         size_t rec,
         size_t col) const
{
  assert(col < nrCols() && rec < nrRecs());

  return Table::col<T>(col)[rec];
}

//! Returns the titles of the columns.
/*!
  \return    Column titles.
*/
inline std::vector<std::string> const& Table::titles() const
{
  return d_titles;
}

template<typename T>
inline std::string Table::asString(size_t rec, size_t col) const
{
  assert(col < nrCols() && rec < nrRecs());

  return boost::lexical_cast<std::string>(cell<T>(rec, col));
}

inline std::string Table::asString(size_t rec, size_t col) const
{
  assert(col < nrCols() && rec < nrRecs());

  switch(typeId(col)) {
    case TI_INT1:     { return asString<INT1>(rec, col);  break; }
    case TI_INT2:     { return asString<INT2>(rec, col);  break; }
    case TI_INT4:     { return asString<INT4>(rec, col);  break; }
    case TI_UINT1:    { return asString<UINT1>(rec, col); break; }
    case TI_UINT2:    { return asString<UINT2>(rec, col); break; }
    case TI_UINT4:    { return asString<UINT4>(rec, col); break; }
    case TI_REAL4:    { return asString<REAL4>(rec, col); break; }
    case TI_REAL8:    { return asString<REAL8>(rec, col); break; }
    case TI_STRING:   { return asString<std::string>(rec, col); break; }
    default:       { assert(false); return ""; break; }
  }
}

inline bool Table::isCreated(
         size_t col) const
{
  assert(col < d_cols.size());

  return !d_cols[col].empty();
}

inline void Table::setAllMV()
{
  for(size_t col = 0; col < nrCols(); ++col) {
    switch(typeId(col)) {
      case TI_INT1: {
        this->col<INT1>(col).setAllMV();
        break;
      }
      case TI_INT2: {
        this->col<INT2>(col).setAllMV();
        break;
      }
      case TI_INT4: {
        this->col<INT4>(col).setAllMV();
        break;
      }
      case TI_UINT1: {
        this->col<UINT1>(col).setAllMV();
        break;
      }
      case TI_UINT2: {
        this->col<UINT2>(col).setAllMV();
        break;
      }
      case TI_UINT4: {
        this->col<UINT4>(col).setAllMV();
        break;
      }
      case TI_REAL4: {
        this->col<REAL4>(col).setAllMV();
        break;
      }
      case TI_REAL8: {
        this->col<REAL8>(col).setAllMV();
        break;
      }
      case TI_STRING: {
        this->col<std::string>(col).setAllMV();
        break;
      }
      default: { /* skip */ break; }
    }
  }
}



template<typename T>
inline void Table::takeCol(
         size_t destinationCol,
         Table& table,
         size_t sourceCol)
{
  assert(destinationCol < nrCols());
  assert(sourceCol < table.nrCols());

  // Make sure this object stays in a valid state.
  assert(typeId(destinationCol) == table.typeId(sourceCol));
  assert((nrCols() == 1 && nrRecs() == 0) || nrRecs() == table.nrRecs());

  takeCol(destinationCol, table.col<T>(sourceCol));
  setTitle(destinationCol, table.title(sourceCol));
}

template<typename T>
inline void Table::takeCol(
         size_t destinationCol,
         Array<T>& array)
{
  assert(destinationCol < nrCols());
  assert((nrCols() == 1 && nrRecs() == 0) || nrRecs() == array.size());

  col<T>(destinationCol).reset(array.release());
}

//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Optimize for case where feature-id's are increasing.
*/
template<typename T>
inline void Table::assign(
         size_t colToWrite,
         size_t joinFrom,
         Table const& table,
         size_t colToRead,
         size_t joinTo)
{
  assert(typeId(colToWrite) == table.typeId(colToRead));
  assert(typeId(joinFrom) == table.typeId(joinTo));
  assert(typeId(joinFrom) == TI_INT4);
  assert(nrRecs() == table.nrRecs());

  Array<INT4> const& joinFromCol(col<INT4>(joinFrom));
  Array<INT4> const& joinToCol(table.col<INT4>(joinTo));
  assert(joinFromCol.size() == joinToCol.size());

  Array<T>& destination(col<T>(colToWrite));
  Array<T> const& source(table.col<T>(colToRead));
  assert(destination.size() == source.size());

  assert(joinFromCol.size() == source.size());

  size_t const nrRecs = joinFromCol.size();

  INT4 joinValue;

  for(size_t i = 0; i < nrRecs; ++i) {
    // Value at source of join.
    joinValue = joinFromCol[i];

    // Find value at destination of join.
    size_t j;

    for(j = 0; j < nrRecs; ++j) {
      if(joinToCol[j] == joinValue) {
        break;
      }
    }

    if(j == nrRecs) {
      // Values could not be joined.
      pcr::setMV(destination[i]);
    }
    else {
      // Found, now assign from destination to source.
      if(!pcr::isMV(source[i])) {
        destination[i] = source[j];
      }
      else {
        pcr::setMV(destination[j]);
      }
    }
  }
}

inline void Table::assign(
         size_t colToWrite,
         size_t joinFrom,
         Table const& table,
         size_t colToRead,
         size_t joinTo)
{
  switch(typeId(colToWrite)) {
    case TI_INT4: {
      assign<INT4>(colToWrite, joinFrom, table, colToRead, joinTo);
      break;
    }
    case TI_REAL4: {
      assign<REAL4>(colToWrite, joinFrom, table, colToRead, joinTo);
      break;
    }
    case TI_REAL8: {
      assign<REAL4>(colToWrite, joinFrom, table, colToRead, joinTo);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

inline bool operator==(Table const& lhs, Table const& rhs)
{
  return lhs.equals(rhs);
}

inline bool operator!=(Table const& lhs, Table const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
