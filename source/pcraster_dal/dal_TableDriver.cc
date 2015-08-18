#ifndef INCLUDED_DAL_TABLEDRIVER
#include "dal_TableDriver.h"
#define INCLUDED_DAL_TABLEDRIVER
#endif

// Library headers.
#ifndef INCLUDED_BOOST_LEXICAL_CAST
#include <boost/lexical_cast.hpp>
#define INCLUDED_BOOST_LEXICAL_CAST
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



/*!
  \file
  This file contains the implementation of the TableDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLEDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

TableDriver::TableDriver(
         Format const& format)

  : Driver(format)

{
}



TableDriver::~TableDriver()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement.
*/
Table* TableDriver::open(
         std::string const& /* name */,
         dal::DataSpace const& /* space */,
         dal::DataSpaceAddress const& /* address */) const
{
  throw Exception("dal::TableDriver::open not implemented for driver");
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Is our handling / detection of the time dimension a valid one?
             We need a hint from the user here! The first column might be
             something entirely else.

  If the values in the first column are integral and if the values form a
  regular increasing range we assume that it is the time dimension.
*/
DataSpace TableDriver::dataSpace(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  boost::shared_ptr<Table> table(open(name, space, address));

  if(!table) {
    throwCannotBeOpened(name, TABLE);
  }

  // Otherwise change code below which sets column stuff explicitly.
  assert(!table->colsAreCreated());

  DataSpace result;

  for(size_t i = 0; i < table->nrCols(); ++i) {
    if(  ((i == 0 /* && table->title(i).empty() */) ||
          (table->title(i) == "time" ||
           table->title(i) == "timestep" ||
           table->title(i) == "timesteps" ||
           table->title(i) == "year")) &&
         isInteger(table->typeId(i))) {
      // Read the time column.
      size_t timeCol = i;
      table->setTypeId(timeCol, TI_INT4);   // We might encounter neg values.
      for(++i; i < table->nrCols(); ++i) {
        table->setTypeId(i, TI_NR_TYPES);
      }

      table->createCols();
      read(*table, name, space, address);
      Array<INT4> const& timeSteps = table->col<INT4>(timeCol);

      // Check the values.
      int first, last, interval;
      if(isRegularIncreasingRange(first, last, interval,
         timeSteps.begin(), timeSteps.end()) && first >= 1) {
        // Configure the time dimension.
        std::vector<size_t> timeSteps;
        timeSteps.push_back(first);
        timeSteps.push_back(last);
        timeSteps.push_back(interval);

        result.addDimension(Dimension(Time, timeSteps));
      }

      break;
    }
    else {
      // Current column is not a column with time steps.
      table->setTypeId(i, TI_NR_TYPES);
    }
  }

  return result;
}



//! Opens the table with name \a name and reads the data.
/*!
  \return    Pointer to Table object.
  \exception Exception In case the table cannot be opened.
  \sa        read(std::string&, Table&), read(std::string&, TypeId)

  This function will return a Table object or throw an exception.

  The table is read using the table properties deduced by
  open(std::string const&). If the table properties are known by the client
  than it should call open(std::string const& name), configure the table
  and call read(std::string const&, Table*).
*/
Table* TableDriver::read(
         std::string const& name) const
{
  Table* table = dynamic_cast<Table*>(Driver::open(name));

  if(!table) {
    throwCannotBeOpened(name, TABLE);
  }

  if(!table->colsAreCreated()) {
    table->createCols();
  }

  read(*table, name);

  return table;
}



//! Opens the table with name \a name and reads the data.
/*!
  \return    Pointer to Table object.
  \exception Exception In case the table cannot be opened.
  \sa        read(std::string&), read(std::string&, Table&)

  This function will return a Table object or throw an exception.

  The table is read using the table properties deduced by
  open(std::string const&). But before reading the actual values of the cells
  the type id of all columns is set to \a typeId.
*/
Table* TableDriver::read(
         std::string const& name,
         TypeId typeId) const
{
  Table* table = dynamic_cast<Table*>(Driver::open(name));

  if(!table) {
    throwCannotBeOpened(name, TABLE);
  }

  assert(!table->colsAreCreated());

  for(size_t col = 0; col < table->nrCols(); ++col) {
    table->setTypeId(col, typeId);
  }

  table->createCols();
  read(*table, name);

  return table;
}



//! Opens the table with name \a name and reads the data into \a table.
/*!
  \exception Exception In case the table cannot be opened.
  \todo      Adjust overloads of this function so that columns with
             TI_NR_TYPES are skipped.

  This function will fail if the values read from \a name are not in sync
  with the properties of \a table. For example, if a column is configured to
  contain UINT2 values and a negative value is encountered an exception will
  be thrown.

  Columns with a type id of TI_NR_TYPES should be skipped during the read.
  (this is not implemenented yet for all table drivers).
*/
void TableDriver::read(
         Table& table,
         std::string const& name) const
{
  read(table, name, DataSpace(), DataSpaceAddress());
}



Table* TableDriver::read(
         std::string const& /* name */,
         dal::DataSpace const& /* space */,
         dal::DataSpaceAddress const& /* address */) const
{
  throw Exception("dal::TableDriver::read not implemented for driver");
}



void TableDriver::read(
         dal::Table& /* table */,
         std::string const& /* name */,
         dal::DataSpace const& /* space */,
         dal::DataSpaceAddress const& /* address */) const
{
  throw Exception("dal::TableDriver::read not implemented for driver");
}



void TableDriver::write(
         Table const& table,
         std::string const& name) const
{
  write(table, DataSpace(), DataSpaceAddress(), name);
}


void TableDriver::write(
         Table const& /* table */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */,
         std::string const& /* name */) const
{
  throw Exception("dal::TableDriver::write not implemented for driver");
}



void TableDriver::append(
         std::string const& name,
         Table const& table) const
{
  append(name, DataSpace(), DataSpaceAddress(), table);
}



void TableDriver::append(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */,
         Table const& /* table */) const
{
  throw Exception("dal::TableDriver::append not implemented for driver");
}



bool TableDriver::extremes(
         boost::any& min,
         boost::any& max,
         size_t col,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space) const
{
  switch(typeId) {
    case TI_UINT1: {
      UINT1 i, a;
      if(extremes<UINT1>(i, a, col, typeId, name, space)) {
        min = i;
        max = a;
        return true;
      }

      break;
    }
    case TI_INT4: {
      INT4 i, a;
      if(extremes<INT4>(i, a, col, typeId, name, space)) {
        min = i;
        max = a;
        return true;
      }

      break;
    }
    case TI_REAL4: {
      REAL4 i, a;
      if(extremes<REAL4>(i, a, col, typeId, name, space)) {
        min = i;
        max = a;
        return true;
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return false;
}



void TableDriver::read(
         void* cell,
         TypeId
#ifdef DEBUG_BUILD
           typeId
#endif
         ,
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(typeId == TI_REAL4);
  pcr::setMV(static_cast<REAL4*>(cell));
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal
