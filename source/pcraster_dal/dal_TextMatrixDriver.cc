#include "dal_TextMatrixDriver.h"
#include "dal_BasicTypes.h"
#include "dal_Exception.h"
#include "dal_Table.h"
#include "dal_FilesystemUtils.h"

#include <fstream>


/*!
  \file
  This file contains the implementation of the TextMatrixDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEXTMATRIXDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TEXTMATRIXDRIVER MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
*/
TextMatrixDriver::TextMatrixDriver()

  : MatrixDriver(Format("TextMatrix", "Text matrix file format",
         MATRIX, Format::File, Format::Attribute)),
    TextFileDriver()

{
  auto& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;

  std::vector<std::string> extensions;
  extensions.push_back(".txt");
  format().setExtensions(extensions);
}



/* NOT IMPLEMENTED
//! Copy constructor.
TextMatrixDriver::TextMatrixDriver(TextMatrixDriver const& rhs)

  : Base(rhs)

{
}
*/



//! Destructor.
/*!
*/
TextMatrixDriver::~TextMatrixDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
TextMatrixDriver& TextMatrixDriver::operator=(TextMatrixDriver const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



bool TextMatrixDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return pathExists(name, space, address);
}



/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement for space and address setting.

  The number of rows in \a name cannot be determined whithout reading the
  whole file and will be set to 0. Only after read(std::string const&) or
  similar has been called will the actual number of rows in \a name be known.

  This function will guess a type id for the values in \a name, based on the
  values in the first rows of the matrix. If you know better, explicitly
  set the type id in the returned Matrix object before reading values into
  the matrix.
*/
Matrix* TextMatrixDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  // assert(space.rank() == 0);
  // assert(address.size() == 0);

  std::ifstream stream;

  try {
    if(!TextFileDriver::open(stream,
         pathForDataSpaceAddress(name, space, address))) {
      return nullptr;
    }
  }
  catch(Exception const&) {
    return nullptr;
  }

  std::string line;
  std::getline(stream, line);

  if(stream.fail()) {
    return nullptr;
  }

  Matrix* matrix = nullptr;

  typedef std::vector<std::string> Row;
  typedef std::vector<Row> Rows;

  // Adjust this if separator is variable.
  // See http://www.boost.org/libs/spirit/example/fundamental/list_parser.cpp
  using namespace boost::spirit::classic;
  rule<> valueParser;
  rule<> spaceParser;
  rule<> rowParser;
  valueParser = +graph_p;
  spaceParser = +ch_p(' ');
  Row row;
  rowParser = !spaceParser >>
       valueParser[PushBack(row)] % spaceParser >> !spaceParser;

  // Parse first line.
  if(parse(line.c_str(), rowParser).full) {
    assert(!row.empty());

    Rows rows;
    rows.push_back(row);

    size_t nrCols = row.size();

    // Parse some more lines.
    size_t nrLinesToProbe = 5;
    for(size_t i = 1; i < nrLinesToProbe + 1 && !stream.eof(); ++i) {

      std::getline(stream, line);

      if(line.empty()) {
        break;
      }

      if(stream.fail()) {
        return nullptr;
      }

      row.clear();
      if(!parse(line.c_str(), rowParser).full) {
        return nullptr;
      }

      assert(!row.empty());

      // Check whether all rows read contain the same number of values.
      if(row.size() != nrCols) {
        return nullptr;
      }

      rows.push_back(row);
    }

    TypeId typeId(TI_NR_TYPES);

    if(rows.empty()) {
      matrix = new Matrix();
    }
    else {
      // Determine the smallest type of the values in all columns to hold all
      // values. Break if no such type could be found.
      determineTypeId(rows, typeId);

      if(typeId == TI_NR_TYPES) {
        return nullptr;
      }

      matrix = new Matrix(nrCols, typeId);
    }
  }

  return matrix;
}



DataSpace TextMatrixDriver::dataSpace(
         std::string const& /* name */) const
{
  // A matrix doesn't have a data space of its own.
  return {};
}



DataSpace TextMatrixDriver::dataSpace(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  // A matrix doesn't have a data space of its own.
  return {};
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Make a template version which is called from here.
*/
void TextMatrixDriver::read(
         std::string const& name,
         Matrix& matrix) const
{
  assert(matrix.nrRows() == 0);

  using namespace boost::spirit::classic;
  rule<> spaceParser = +ch_p(' ');
  stored_rule<> valueParser;
  stored_rule<> rowParser;

  std::vector<TypeId> typeIds;
  typeIds.push_back(matrix.typeId());
  Table table(typeIds);
  table.createCols();

  switch(matrix.typeId()) {
    case TI_UINT1: {
      Array<UINT1>& cells = table.col<UINT1>(0);
      valueParser = Uint1Type::Parser()[push_back_a(cells)];
      break;
    }
    case TI_UINT2: {
      Array<UINT2>& cells = table.col<UINT2>(0);
      valueParser = Uint2Type::Parser()[push_back_a(cells)];
      break;
    }
    case TI_UINT4: {
      Array<UINT4>& cells = table.col<UINT4>(0);
      valueParser = Uint4Type::Parser()[push_back_a(cells)];
      break;
    }
    case TI_INT1: {
      Array<INT1>& cells = table.col<INT1>(0);
      valueParser = Int1Type::Parser()[push_back_a(cells)];
      break;
    }
    case TI_INT2: {
      Array<INT2>& cells = table.col<INT2>(0);
      valueParser = Int2Type::Parser()[push_back_a(cells)];
      break;
    }
    case TI_INT4: {
      Array<INT4>& cells = table.col<INT4>(0);
      valueParser = Int4Type::Parser()[push_back_a(cells)];
      break;
    }
    case TI_REAL4: {
      Array<REAL4>& cells = table.col<REAL4>(0);
      valueParser = Real4Type::Parser()[push_back_a(cells)];
      break;
    }
    case TI_REAL8: {
      Array<REAL8>& cells = table.col<REAL8>(0);
      valueParser = Real8Type::Parser()[push_back_a(cells)];
      break;
    }
    default: { assert(false); break; }
  }

  rowParser = !spaceParser >> valueParser.copy();

  if(matrix.nrCols() > 1) {
    for(size_t col = 1; col < matrix.nrCols(); ++col) {
      rowParser = rowParser.copy() >> spaceParser >> valueParser.copy();
    }
  }

  rowParser = rowParser.copy() >> !spaceParser;

  std::ifstream stream;

  try {
    if(!TextFileDriver::open(stream, pathFor(name))) {
      throwCannotBeOpened(name, MATRIX);
    }
  }
  catch(Exception const& exception) {
    throwCannotBeOpened(name, MATRIX, exception.message());
  }

  std::string line;

  int nrRows = 0;
  while(!stream.eof()) {
    std::getline(stream, line);

    if(line.empty()) {
      break;
    }

    if(stream.fail() || !parse(line.c_str(), rowParser).full) {
      throwCannotReadRecord(name, MATRIX, nrRows + 1);
    }

    ++nrRows;
  }

  switch(matrix.typeId()) {
    case TI_UINT1: {
      matrix.transfer(table.col<UINT1>(0).release());
      break;
    }
    case TI_UINT2: {
      matrix.transfer(table.col<UINT2>(0).release());
      break;
    }
    case TI_UINT4: {
      matrix.transfer(table.col<UINT4>(0).release());
      break;
    }
    case TI_INT1: {
      matrix.transfer(table.col<INT1>(0).release());
      break;
    }
    case TI_INT2: {
      matrix.transfer(table.col<INT2>(0).release());
      break;
    }
    case TI_INT4: {
      matrix.transfer(table.col<INT4>(0).release());
      break;
    }
    case TI_REAL4: {
      matrix.transfer(table.col<REAL4>(0).release());
      break;
    }
    case TI_REAL8: {
      matrix.transfer(table.col<REAL8>(0).release());
      break;
    }
    default: { assert(false); break; }
  }

  matrix.setNrRows(nrRows);
}



Matrix* TextMatrixDriver::read(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(false);
  return nullptr;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal
