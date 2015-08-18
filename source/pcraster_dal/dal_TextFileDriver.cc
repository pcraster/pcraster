#ifndef INCLUDED_DAL_TEXTFILEDRIVER
#include "dal_TextFileDriver.h"
#define INCLUDED_DAL_TEXTFILEDRIVER
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif


/*!
  \file
  This file contains the implementation of the TextFileDriver class.
*/



//------------------------------------------------------------------------------

/*
namespace dal {

class TextFileDriverPrivate
{
public:

  TextFileDriverPrivate()
  {
  }

  ~TextFileDriverPrivate()
  {
  }

};

} // namespace dal
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEXTFILEDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TEXTFILEDRIVER MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
*/
dal::TextFileDriver::TextFileDriver()
{
}



//! Destructor.
/*!
*/
dal::TextFileDriver::~TextFileDriver()
{
}



//! Returns the type information.
/*!
  \return    Type information.
*/
dal::Types const& dal::TextFileDriver::types() const
{
  return d_types;
}



/*!
  \overload
*/
void dal::TextFileDriver::determineTypeId(
         std::vector<std::vector<std::string> > const& rows,
         TypeId& typeId) const
{
  TypeId ti = TI_NR_TYPES;

  for(size_t row = 0; row < rows.size(); ++row) {

    determineTypeId(rows[row], ti);

    if(typeId == TI_NR_TYPES) {
      // No valid type id stored yet.
      typeId = ti;
    }
    else {
      // Choose id of biggest type of stored and determined type id.
      typeId = d_types.idOfLargestType(typeId, ti);
    }
  }
}



//! Tries to find the id of the smallest type which can hold the values in \a row.
/*!
  \param     row Row with columns of values.
  \param     typeId Type id of smallest type which can hold the values.
  \sa        dal::Types::idOfSmallestType(std::string const&)

  Values are ordered in rows.
*/
void dal::TextFileDriver::determineTypeId(
         std::vector<std::string> const& row,
         TypeId& typeId) const
{
  for(size_t col = 0; col < row.size(); ++col) {

    TypeId ti = d_types.idOfSmallestType(row[col]);

    if(typeId == TI_NR_TYPES) {
      // No valid type id stored yet.
      typeId = ti;
    }
    else {
      // Choose id of biggest type of stored and determined type id.
      typeId = d_types.idOfLargestType(typeId, ti);
    }
  }
}



/*!
  \overload
*/
void dal::TextFileDriver::determineTypeIds(
         std::vector<std::vector<std::string> > const& records,
         std::vector<TypeId>& typeIds) const
{
  std::fill(typeIds.begin(), typeIds.end(), TI_NR_TYPES);

  for(size_t rec = 0; rec < records.size(); ++rec) {

    determineTypeIds(records[rec], typeIds);

    if(std::find(typeIds.begin(), typeIds.end(), TI_NR_TYPES) !=
       typeIds.end()) {
      break;
    }
  }
}



//! Tries to find the id of the smallest types which can hold the values in a column of a \a row.
/*!
  \param     record Record of values.
  \param     typeIds Record of type ids of smallest types which can hold the
             values in each column.
  \sa        Types::idOfSmallestType(std::string const&)

  Values are ordered in columns.
*/
void dal::TextFileDriver::determineTypeIds(
         std::vector<std::string> const& record,
         std::vector<TypeId>& typeIds) const
{
  assert(record.size() == typeIds.size());

  for(size_t col = 0; col < record.size(); ++col) {

    TypeId typeId = d_types.idOfSmallestType(record[col]);

    if(typeIds[col] == TI_NR_TYPES) {
      // No typeId stored yet for this column.
      typeIds[col] = typeId;
    }
    else {
      // Choose id of biggest type of stored and determined typeId.
      typeIds[col] = d_types.idOfLargestType(typeIds[col], typeId);
    }
  }
}



//! Opens the stream \a stream using \a path.
/*!
  \param     stream Stream to open.
  \param     name Name of file to use.
  \return    Whether the stream could be opened successfully.
  \todo      See com::open for stuff which should be put here.
*/
bool dal::TextFileDriver::open(
         std::ifstream& stream,
         boost::filesystem::path const& path,
         std::ios::openmode flags) const
{
  stream.open(path.string().c_str(), flags);
  return stream.good();
}



bool dal::TextFileDriver::open(
         std::ofstream& stream,
         boost::filesystem::path const& path,
         std::ios::openmode flags) const
{
  stream.open(path.string().c_str(), flags);
  return stream.good();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



