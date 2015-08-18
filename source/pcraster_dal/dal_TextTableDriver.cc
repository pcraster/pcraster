#ifndef INCLUDED_DAL_TEXTTABLEDRIVER
#include "dal_TextTableDriver.h"
#define INCLUDED_DAL_TEXTTABLEDRIVER
#endif

// Library headers.
#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_BASICTYPES
#include "dal_BasicTypes.h"
#define INCLUDED_DAL_BASICTYPES
#endif

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif



/*!
  \file
  This file contains the implementation of the TextTableDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEXTTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

Table* TextTableDriver::markSelectedCols(
         Table* table,
         std::vector<std::string> const& selectedCols)
{
  if(table && !selectedCols.empty()) {
    // Set type id of not explicitly selected columns to TI_NR_TYPES.
    for(size_t i = 0; i < table->nrCols(); ++i) {
      if(std::find(selectedCols.begin(), selectedCols.end(),
            boost::lexical_cast<std::string>(i + 1)) == selectedCols.end()) {
        table->setTypeId(i, TI_NR_TYPES);
      }
    }
  }

  return table;
}



//------------------------------------------------------------------------------
// DEFINITION OF TEXTTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

TextTableDriver::TextTableDriver(HeaderType headerType)

  : TableDriver(Format("text", "Text table file format",
         TABLE, Format::File, Format::Vector, Format::Attribute)),
    TextFileDriver(),
    d_headerType(headerType)

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  properties |= Writer;

  std::vector<std::string> extensions;
  extensions.push_back(".col");
  format().setExtensions(extensions);
}



TextTableDriver::TextTableDriver(
         Format const& format,
         HeaderType headerType)

  : TableDriver(format),
    TextFileDriver(),
    d_headerType(headerType)

{
}



/* NOT IMPLEMENTED
//! Copy constructor.
TextTableDriver::TextTableDriver(TextTableDriver const& rhs)

  : Base(rhs)

{
}
*/



TextTableDriver::~TextTableDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
TextTableDriver& TextTableDriver::operator=(TextTableDriver const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



bool TextTableDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return pathExists(
         boost::get<0>(splitNameAndSelection(name)), space, address);
}



bool TextTableDriver::open(
         Table& table,
         std::istream& stream) const
{
  typedef std::vector<std::string> Record;
  typedef std::vector<Record> Records;

  // Adjust this if separator is variable.
  // See http://www.boost.org/libs/spirit/example/fundamental/list_parser.cpp
  using namespace boost::spirit::classic;
  rule<> valueParser, recordParser;
  valueParser = +graph_p;
  Record titles(table.titles());
  Record record;

  recordParser = *blank_p >>
       valueParser[PushBack(record)] % +blank_p >> *blank_p >> !eol_p;

  std::string line;
  std::getline(stream, line);
  if(stream.fail()) {
    return false;
  }

  if(!parse(line.c_str(), recordParser).full) {
    return false;
  }

  assert(!record.empty());
  size_t const nrCols = record.size();
  Records records;

  if(d_headerType == HEADER) {
    // We know for sure the first line is a header line.
    titles = record;
  }
  else {
    // Assume for now that the first line contains data.
    titles.resize(nrCols);
    records.push_back(record);
  }

  // The goal here is to determine a type for each column. We read some lines
  // and check the type of the values, per column. It is not unlikely for a
  // column to contain multiple records with '0'. In that case we continue
  // reading until we see a different value. Otherwise the type will be int4,
  // which may be wrong in case subsequent records contain floating points.
  size_t nrLinesToProbe = d_headerType == HEADER ? 6 : 5;
  std::vector<bool> non_zero_found(nrCols, false);
  bool non_zero_found_for_each_column = false;
  for(size_t i = 0;
      // Check at least nrLinesToProbe lines...
      (i < nrLinesToProbe ||
      // ... and continue as long as we see a '0'.
      (i >= nrLinesToProbe && !non_zero_found_for_each_column)) &&
      !stream.eof(); ++i) {
    std::getline(stream, line);
    if(line.empty()) {
      break;
    }
    if(stream.fail()) {
      return false;
    }

    record.clear();
    if(!parse(line.c_str(), recordParser).full) {
      return false;
    }

    assert(!record.empty());

    // Check whether all records read contain the same number of values.
    if(record.size() != nrCols) {
      return false;
    }

    records.push_back(record);

    // For each column...
    for(size_t col = 0; col < nrCols; ++col) {
      for(size_t rec = 0; rec <= i; ++rec) {
        if(records[rec][col] != "0") {
          // OK, column contains at least one value other than zero.
          non_zero_found[col] = true;
          break;
        }
      }
    }

    // Determine whether all columns contain values other than zero.
    non_zero_found_for_each_column = std::count(non_zero_found.begin(),
      non_zero_found.end(), true) == static_cast<int>(nrCols);
  }

  std::vector<TypeId> typeIds(nrCols, TI_NR_TYPES);

  if(records.empty()) {
    table.init(table.title(), titles);
  }
  else {
    // Determine the smallest type of the values in each column to hold all
    // values. Break if no such type could be found.
    determineTypeIds(records, typeIds);

    if(std::find(typeIds.begin(), typeIds.end(), TI_NR_TYPES) !=
         typeIds.end()) {

      if(d_headerType == NO_HEADER) {
        return false;
      }

      assert(d_headerType == AUTO_HEADER);

      // Maybe we succeed when we assume the first line is a header line.
      titles = records[0];
      records.erase(records.begin());
      determineTypeIds(records, typeIds);

      if(std::find(typeIds.begin(), typeIds.end(), TI_NR_TYPES) !=
           typeIds.end()) {
        return false;
      }
    }

    assert(titles.size() == typeIds.size());
    table.init(table.title(), titles, typeIds);
  }

  return true;
}



Table* TextTableDriver::open(
         boost::tuple<std::string, std::vector<std::string> > const& tuple) const
{
  Table* table = open(pathFor(boost::get<0>(tuple)));

  return markSelectedCols(table, boost::get<1>(tuple));
}



Table* TextTableDriver::open(
         boost::tuple<std::string, std::vector<std::string> > const& tuple,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  Table* table = open(pathForDataSpaceAddress(boost::get<0>(tuple),
       space, address));

  return markSelectedCols(table, boost::get<1>(tuple));
}



//! Creates a Table object from the data pointed to by \a name.
/*!
  \param     name Name of tabular data for which to create a Table object.
  \return    Pointer to Table.
  \sa        Driver::open(std::string const&)

  We assume that \a name is a table if the first 5 lines in the file have the
  same number of columns and that each column contains values of the same type.

  If the file contains less than 5 lines only those lines are considered.
*/
Table* TextTableDriver::open(
         boost::filesystem::path const& path) const
{
  std::ifstream stream;

  if(!TextFileDriver::open(stream, path)) {
    return 0;
  }

#ifdef __GXX_EXPERIMENTAL_CXX0X__
    std::unique_ptr<Table> table(new Table());
#else
    std::auto_ptr<Table> table(new Table());
#endif

  if(!open(*table, stream)) {
    return 0;
  }

  return table.release();
}



// Table* TextTableDriver::open(
//          std::string const& name) const
// {
//   try {
//     return open(splitNameAndSelection(name));
//   }
//   catch(Exception const&) {
//     return 0;
//   }
// }



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
Table* TextTableDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  try {
    return open(splitNameAndSelection(name), space, address);
  }
  catch(Exception const&) {
    return 0;
  }
}



/*
Table* TextTableDriver::read(std::string const& name) const
{
  Table* table = open(name);

  if(!table) {
    throw Exception(
         (boost::format("Table '%1%': cannot be opened") % name).str());
  }

  read(name, *table);

  return table;
}
*/



// DataSpace TextTableDriver::dataSpace(
//          std::string const& name,
//          DataSpace const& space,
//          DataSpaceAddress const& address) const
// {
//   boost::shared_ptr<Table> table(open(name, space, address));
// 
//   if(!table) {
//     throwCannotBeOpened(name, TABLE);
//   }
// 
//   DataSpace dataSpace;
// 
//   if(isUnsignedInteger(table->typeId(0))) {
//     // Read the time column.
//     for(size_t i = 0; i < table->nrCols(); ++i) {
//       table->setTypeId(i, TI_NR_TYPES);
//     }
// 
//     size_t timeCol = 0;
//     table->setTypeId(timeCol, TI_INT4);       // To be able to read neg values.
//     table->createCols();
// 
//     read(*table, name, space, address);
//     Array<INT4> const& timeSteps = table->col<INT4>(timeCol);
// 
//     // Check the values.
//     int first, last, interval;
// 
//     if(isRegularIncreasingRange(first, last, interval,
//        timeSteps.begin(), timeSteps.end()) && first >= 1) {
//       // Configure the time dimension.
//       std::vector<boost::any> timeSteps;
//       timeSteps.push_back(size_t(first));
//       timeSteps.push_back(size_t(last));
//       timeSteps.push_back(size_t(interval));
// 
//       dataSpace.addDimension(Dimension(Time, timeSteps));
//     }
//   }
// 
//   return dataSpace;
// }



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Alternative implementation for more speed.
*/
void TextTableDriver::readValues(
         Table& table,
         std::ifstream& stream,
         std::string const& name) const
{
  using namespace boost::spirit::classic;

  stored_rule<> recordParser, valueParser;

  for(size_t col = 0; col < table.nrCols(); ++col) {
    switch(table.typeId(col)) {
      case TI_UINT1: {
        Array<UINT1>& column = table.col<UINT1>(col);
        valueParser = Uint1Type::Parser()[push_back_a(column)];
        break;
      }
      case TI_UINT2: {
        Array<UINT2>& column = table.col<UINT2>(col);
        valueParser = Uint2Type::Parser()[push_back_a(column)];
        break;
      }
      case TI_UINT4: {
        Array<UINT4>& column = table.col<UINT4>(col);
        valueParser = Uint4Type::Parser()[push_back_a(column)];
        break;
      }
      case TI_INT1: {
        Array<INT1>& column = table.col<INT1>(col);
        valueParser = Int1Type::Parser()[push_back_a(column)];
        break;
      }
      case TI_INT2: {
        Array<INT2>& column = table.col<INT2>(col);
        valueParser = Int2Type::Parser()[push_back_a(column)];
        break;
      }
      case TI_INT4: {
        Array<INT4>& column = table.col<INT4>(col);
        valueParser = Int4Type::Parser()[push_back_a(column)];
        break;
      }
      case TI_REAL4: {
        Array<REAL4>& column = table.col<REAL4>(col);
        valueParser = Real4Type::Parser()[push_back_a(column)];
        break;
      }
      case TI_REAL8: {
        Array<REAL8>& column = table.col<REAL8>(col);
        valueParser = Real8Type::Parser()[push_back_a(column)];
        break;
      }
      case TI_NR_TYPES: {
        // Skip column.
        valueParser = +graph_p;
        break;
      }
      default: { assert(false); break; }
    }

    if(col == 0) {
      recordParser = valueParser.copy();
    }
    else {
      recordParser = recordParser.copy() >> +blank_p >> valueParser.copy();
    }
  }

  recordParser = *blank_p >> recordParser.copy() >> *blank_p >> !eol_p;

  std::string line;

  if(d_headerType == HEADER) {
    // Skip first line, titles already read by open function.
    assert(!stream.eof());
    std::getline(stream, line);

    if(stream.fail()) {
      throwDataSourceError(name, TABLE, "cannot read column titles");
    }
  }

  size_t nrRecords = 0;
  bool headerRead = false;

  while(!stream.eof()) {
    std::getline(stream, line);
    ++nrRecords;

    if(line.empty()) {
      break;
    }

    if(stream.fail()) {
      throwCannotReadRecord(name, TABLE, nrRecords);
    }

    if(!parse(line.c_str(), recordParser).full) {
      // In case parsing of the first record fails and header type is AUTO,
      // we assume that the first line contains the header titles. These have
      // already been read and set by de open function.
      if(d_headerType == AUTO_HEADER && !headerRead) {
        headerRead = true;
        --nrRecords;
      }
      else {
        throwCannotReadRecord(name, TABLE, nrRecords);
      }
    }
  }

  for(size_t col = 0; col < table.nrCols(); ++col) {
    switch(table.typeId(col)) {
      case TI_UINT1: {
        break;
      }
      case TI_UINT2: {
        break;
      }
      case TI_UINT4: {
        break;
      }
      case TI_INT1: {
        break;
      }
      case TI_INT2: {
        break;
      }
      case TI_INT4: {
        break;
      }
      case TI_REAL4: {
        Array<REAL4>& column = table.col<REAL4>(col);
        toStdMV<REAL4>(column.begin(), column.end(), REAL4(1e+31));
        break;
      }
      case TI_REAL8: {
        break;
      }
      case TI_NR_TYPES: {
        // Skip column.
        break;
      }
      default: { assert(false); break; }
    }
  }
}



void TextTableDriver::read(
         Table& table,
         boost::filesystem::path const& path) const
{
  std::ifstream stream;

  if(!TextFileDriver::open(stream, path)) {
    throwCannotBeOpened(path.string(), TABLE);
  }

  readValues(table, stream, path.string());
}



// /*!
//   \exception Exception In case an error occurs during the parse.
//   \warning   This function fails if the type id of one of the columns equals
//              TI_NR_TYPES. This happens, for example, when a file only
//              contains column titles and no data. Be sure to check beforehand.
// */
// void TextTableDriver::read(
//          Table& table,
//          std::string const& name) const
// {
//   read(table, pathFor(boost::get<0>(splitNameAndSelection(name))));
// }



void TextTableDriver::read(
         Table& table,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  read(table,
         pathForDataSpaceAddress(boost::get<0>(splitNameAndSelection(name)),
         space, address));
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
Table* TextTableDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  Table* result = open(name, space, address);

  if(!result) {
    throwCannotBeOpened(name, TABLE, space, address);
  }

  read(*result, name, space, address);

  return result;
}



// bool TextTableDriver::extremes(
//          boost::any& min,
//          boost::any& max,
//          size_t col,
//          TypeId typeId,
//          std::string const& name,
//          DataSpace const& space) const
// {
//   switch(typeId) {
//     case TI_UINT1: {
//       UINT1 i, a;
//       if(extremes<UINT1>(i, a, col, typeId, name, space)) {
//         min = i;
//         max = a;
//         return true;
//       }
// 
//       break;
//     }
//     case TI_INT4: {
//       INT4 i, a;
//       if(extremes<INT4>(i, a, col, typeId, name, space)) {
//         min = i;
//         max = a;
//         return true;
//       }
// 
//       break;
//     }
//     case TI_REAL4: {
//       REAL4 i, a;
//       if(extremes<REAL4>(i, a, col, typeId, name, space)) {
//         min = i;
//         max = a;
//         return true;
//       }
// 
//       break;
//     }
//     default: {
//       assert(false);
//       break;
//     }
//   }
// 
//   return false;
// }



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal


