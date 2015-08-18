#ifndef INCLUDED_DAL_GEOEASTABLEDRIVER
#include "dal_GeoEASTableDriver.h"
#define INCLUDED_DAL_GEOEASTABLEDRIVER
#endif

// Library headers.
#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_BOOST_ALGORITHM_STRING
#include <boost/algorithm/string.hpp>
#define INCLUDED_BOOST_ALGORITHM_STRING
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

#ifndef INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC
#include <boost/spirit/include/classic.hpp>
#define INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC
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
  This file contains the implementation of the GeoEASTableDriver class.
*/


namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GEOEASTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GEOEASTABLEDRIVER MEMBERS
//------------------------------------------------------------------------------

dal::GeoEASTableDriver::GeoEASTableDriver()

  : TextTableDriver(Format("Geo-EAS", "Geo-EAS table file format",
         TABLE, Format::File, Format::Vector, Format::Attribute), NO_HEADER)

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  properties |= Writer;

  std::vector<std::string> extensions;
  extensions.push_back(".eas");
  extensions.push_back(".tss");
  extensions.push_back(".col");
  format().setExtensions(extensions);
}



dal::GeoEASTableDriver::~GeoEASTableDriver()
{
}



bool dal::GeoEASTableDriver::readHeader(
         Table& table,
         std::istream& stream) const
{
  std::string line;
  std::getline(stream, line);
  if(stream.fail()) {
    return false;
  }

  boost::trim(line);
  table.setTitle(line);

  std::getline(stream, line);
  if(stream.fail()) {
    return false;
  }

  boost::trim(line);

  using namespace boost::spirit::classic;
  size_t nrAttributes;
  rule<> valueParser = Uint4Type::Parser()[assign_a(nrAttributes)];

  if(!parse(line.c_str(), valueParser).full) {
    return false;
  }

  std::vector<std::string> names(nrAttributes);
  for(std::vector<std::string>::iterator it = names.begin(); it != names.end();
         ++it) {
    std::getline(stream, line);
    if(stream.fail()) {
      return false;
    }

    boost::trim(line);
    *it = line;
  }

  table.setTitles(names);

  return true;
}



dal::Table* dal::GeoEASTableDriver::open(
         boost::filesystem::path const& path) const
{
  std::ifstream stream;

  if(!TextFileDriver::open(stream, path)) {
    return 0;
  }

  Table* table(new Table());

  if(!readHeader(*table, stream) || !TextTableDriver::open(*table, stream)) {
    delete table;
    table = 0;
  }

  return table;
}



/*
dal::Table* dal::GeoEASTableDriver::open(std::string const& name) const
{
  try {
    return open(pathFor(name));
  }
  catch(Exception const&) {
    return 0;
  }
}
*/



/*
dal::Table* dal::GeoEASTableDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  try {
    return open(pathForDataSpaceAddress(name, space, address));
  }
  catch(Exception const&) {
    return 0;
  }
}
*/



/*
dal::DataSpace dal::GeoEASTableDriver::dataSpace(
         std::string const& name) const
{
  assert(false);
  return DataSpace();
}
*/



// dal::DataSpace dal::GeoEASTableDriver::dataSpace(
//          std::string const& name,
//          DataSpace const& space,
//          DataSpaceAddress const& address) const
// {
//   boost::shared_ptr<Table> table(TextTableDriver::open(name, space,
//     address));
// 
//   if(!table) {
//     throwCannotBeOpened(name, TABLE);
//   }
// 
//   // If one of the column titles is "timestep" and if its values are integral
//   // and if the values form an regular increasing range we assume that it is
//   // the time dimension.
// 
//   DataSpace tableSpace;
// 
//   for(size_t i = 0; i < table->nrCols(); ++i) {
//     if(  (table->title(i) == "time" ||
//           table->title(i) == "timestep" ||
//           table->title(i) == "timesteps") &&
//          isUnsignedInteger(table->typeId(i))) {
//       // Read the time column.
//       size_t timeCol = i;
//       table->setTypeId(timeCol, TI_INT4);   // We might encounter neg values.
//       for(++i; i < table->nrCols(); ++i) {
//         table->setTypeId(i, TI_NR_TYPES);
//       }
// 
//       table->createCols();
// 
//       TextTableDriver::read(*table, name, space, address);
//       Array<INT4> const& timeSteps = table->col<INT4>(timeCol);
// 
//       // Check the values.
//       int first, last, interval;
//       if(isRegularIncreasingRange(first, last, interval,
//          timeSteps.begin(), timeSteps.end()) && first >= 1) {
//         // Configure the time dimension.
//         std::vector<boost::any> timeSteps;
//         timeSteps.push_back(size_t(first));
//         timeSteps.push_back(size_t(last));
//         timeSteps.push_back(size_t(interval));
// 
//         tableSpace.addDimension(Dimension(Time, timeSteps));
//       }
// 
//       break;
//     }
//     else {
//       table->setTypeId(i, TI_NR_TYPES);
//     }
//   }
// 
//   return tableSpace;
// }



/*
bool dal::GeoEASTableDriver::extremes(
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
*/



/*
void dal::GeoEASTableDriver::read(std::string const& name, Table& table) const
{
  std::ifstream stream;
  if(!TextFileDriver::open(stream, name)) {
    throwCannotBeOpened(name, TABLE);
  }

  if(!readHeader(stream, table)) {
    throwCannotReadHeader(name, TABLE);
  }

  readValues(name, stream, table);
}
*/



void dal::GeoEASTableDriver::read(
         dal::Table& table,
         boost::filesystem::path const& path) const
{
  std::ifstream stream;

  if(!TextFileDriver::open(stream, path)) {
    throwCannotBeOpened(path.string(), TABLE);
  }

  if(!readHeader(table, stream)) {
    throwCannotReadHeader(path.string(), TABLE);
  }

  readValues(table, stream, path.string());

  /*
  for(size_t i = 0; i < table.nrCols(); ++i) {
    switch(table.typeId(i)) {
      case TI_REAL4: {
        Array<REAL4>& array(table.col<REAL4>(i));
        for(Array<REAL4>::iterator it = array.begin(); it != array.end(); ++it) {
          // std::cout << *it << std::endl;
          if(std::abs(*it - REAL4(1e31)), std::max(REAL4(1), std::max(std::abs(*it), std::abs(REAL4(1e31)))) < 0.000001) {
            // std::cout << "setmv!" << std::endl;
            pcr::setMV(*it);
          }
        }
        break;
      }
      // case TI_REAL8: {
      //   Array<REAL8>& array(table.col<REAL8>(i));
      //   toStdMV<REAL8>(array.begin(), array.end(), 1e31);
      //   break;
      // }
      default: {
        break;
      }
    }
  }
*/
}



/*
void dal::GeoEASTableDriver::read(
         dal::Table& table,
         std::string const& name) const
{
  read(table, pathFor(name));
}
*/



/*
void dal::GeoEASTableDriver::read(
         dal::Table& table,
         std::string const& name,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  read(table, pathForDataSpaceAddress(name, space, address));
}
*/



dal::Table* dal::GeoEASTableDriver::read(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(false);
  return 0;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement this in terms of writeHeader/writeValues and move this
             to TextTableDriver. See read.
*/
void GeoEASTableDriver::write(
         Table const& table,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& name) const
{
  std::ofstream stream;
  boost::filesystem::path path(pathForDataSpaceAddress(name, space, address));

  if(!TextFileDriver::open(stream, path)) {
    throwCannotBeOpened(path.string(), TABLE);
  }

  // write header
  stream << "GeoEAS Table written by PCRaster Aguila\n";
  stream << table.nrCols() << '\n';

  for(size_t col = 0; col < table.nrCols(); ++col) {
    stream << table.title(col) << '\n';
  }

  // write values
  for(size_t rec = 0; rec < table.nrRecs(); ++rec) {
    for(size_t col = 0; col < table.nrCols(); ++col) {
      switch(table.typeId(col)) {
        case TI_INT1: {
          writeValue(table.col<INT1>(col), rec, stream);
          break;
        }
        case TI_INT4: {
          writeValue(table.col<INT4>(col), rec, stream);
          break;
        }
        case TI_UINT1: {
          writeValue(table.col<UINT1>(col), rec, stream);
          break;
        }
        case TI_UINT2: {
          writeValue(table.col<UINT2>(col), rec, stream);
          break;
        }
        case TI_UINT4: {
          writeValue(table.col<UINT4>(col), rec, stream);
          break;
        }
        case TI_REAL4: {
          writeValue(table.col<REAL4>(col), rec, stream);
          break;
        }
        case TI_REAL8: {
          writeValue(table.col<REAL8>(col), rec, stream);
          break;
        }
        case TI_STRING: {
          writeValue(table.col<std::string>(col), rec, stream);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      if(col != table.nrCols() - 1) {
        stream << '\t';
      }
    }

    stream << '\n';
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

