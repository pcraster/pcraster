#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif

// Library headers.
#ifndef INCLUDED_BOOST_ALGORITHM_STRING
#include <boost/algorithm/string.hpp>
#define INCLUDED_BOOST_ALGORITHM_STRING
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_LIBRARY
#include "dal_Library.h"
#define INCLUDED_DAL_LIBRARY
#endif

#ifndef INCLUDED_DAL_MATRIX
#include "dal_Matrix.h"
#define INCLUDED_DAL_MATRIX
#endif

#ifndef INCLUDED_DAL_MATRIXDAL
#include "dal_MatrixDal.h"
#define INCLUDED_DAL_MATRIXDAL
#endif

#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif

#ifndef INCLUDED_DAL_RASTERDAL
#include "dal_RasterDal.h"
#define INCLUDED_DAL_RASTERDAL
#endif

#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif

#ifndef INCLUDED_DAL_TABLEDAL
#include "dal_TableDal.h"
#define INCLUDED_DAL_TABLEDAL
#endif



namespace dal {

void* newBuffer(
         TypeId typeId,
         size_t size)
{
  void* buffer = 0;

  switch(typeId) {
    case TI_UINT1: { buffer = new UINT1[size]; break; }
    case TI_INT4:  { buffer = new INT4[size];  break; }
    case TI_REAL4: { buffer = new REAL4[size]; break; }
    case TI_INT2:  { buffer = new INT2[size];  break; }
    case TI_INT1:  { buffer = new INT1[size];  break; }
    case TI_UINT2: { buffer = new UINT2[size]; break; }
    case TI_UINT4: { buffer = new UINT4[size]; break; }
    case TI_REAL8: { buffer = new REAL8[size]; break; }
    default: { assert(false); break; }
  }

  return buffer;
}



void deleteBuffer(
         TypeId typeId,
         void* buffer)
{
  switch(typeId) {
    case TI_UINT1: delete[] static_cast<UINT1*>(buffer); break;
    case TI_INT4:  delete[] static_cast<INT4*>(buffer);  break;
    case TI_REAL4: delete[] static_cast<REAL4*>(buffer); break;
    case TI_INT1:  delete[] static_cast<INT1*>(buffer);  break;
    case TI_INT2:  delete[] static_cast<INT2*>(buffer);  break;
    case TI_UINT2: delete[] static_cast<UINT2*>(buffer); break;
    case TI_UINT4: delete[] static_cast<UINT4*>(buffer); break;
    case TI_REAL8: delete[] static_cast<REAL8*>(buffer); break;
    default: assert(false); break;
  }
}



void toStdMV(
         TypeId typeId,
         void* buffer,
         size_t nrValues,
         double mv)
{
  switch(typeId) {
    case TI_UINT1: toStdMV<UINT1>(
         static_cast<UINT1*>(buffer), nrValues, static_cast<UINT1>(mv)); break;
    case TI_INT4:  toStdMV<INT4> (
         static_cast<INT4*> (buffer), nrValues, static_cast<INT4>(mv));  break;
    case TI_REAL4: toStdMV<REAL4>(
         static_cast<REAL4*>(buffer), nrValues, static_cast<REAL4>(mv)); break;
    case TI_INT1:  toStdMV<INT1> (
         static_cast<INT1*> (buffer), nrValues, static_cast<INT1>(mv));  break;
    case TI_INT2:  toStdMV<INT2> (
         static_cast<INT2*> (buffer), nrValues, static_cast<INT2>(mv));  break;
    case TI_UINT2: toStdMV<UINT2>(
         static_cast<UINT2*>(buffer), nrValues, static_cast<UINT2>(mv)); break;
    case TI_UINT4: toStdMV<UINT4>(
         static_cast<UINT4*>(buffer), nrValues, static_cast<UINT4>(mv)); break;
    case TI_REAL8: toStdMV<REAL8>(
         static_cast<REAL8*>(buffer), nrValues, static_cast<REAL8>(mv)); break;
    default: assert(false); break;
  }
}



PCR_DAL_DECL CSF_VS typeIdToValueScale(
         TypeId typeId)
{
  CSF_VS result = VS_UNDEFINED;

  switch(typeId) {
    case TI_UINT1:
    case TI_UINT2:
    case TI_UINT4:
    case TI_INT1:
    case TI_INT2:
    case TI_INT4: {
      result = VS_NOMINAL;
      break;
    }
    case TI_REAL4:
    case TI_REAL8: {
      result = VS_SCALAR;
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



PCR_DAL_DECL std::string valueScaleToString(
         CSF_VS valueScale)
{
  std::string result;

  switch(valueScale) {
    case VS_BOOLEAN:    { result = "BOOLEAN"    ; break; }
    case VS_NOMINAL:    { result = "NOMINAL"    ; break; }
    case VS_ORDINAL:    { result = "ORDINAL"    ; break; }
    case VS_SCALAR:     { result = "SCALAR"     ; break; }
    case VS_DIRECTION:  { result = "DIRECTIONAL"; break; }
    case VS_LDD:        { result = "LDD"        ; break; }
    default:            { result = ""; break; }
  }

  return result;
}



PCR_DAL_DECL std::string typeIdToString(
         TypeId typeId)
{
  switch(typeId) {
    case TI_UINT1:  { return "UINT1"; }
    case TI_INT4:   { return "INT4" ; }
    case TI_REAL4:  { return "REAL4"; }
    case TI_INT1:   { return "INT1" ; }
    case TI_INT2:   { return "INT2" ; }
    case TI_UINT2:  { return "UINT2"; }
    case TI_UINT4:  { return "UINT4"; }
    case TI_REAL8:  { return "REAL8"; }
    case TI_STRING: { return "STRING"; }
    default:        { return ""; }
  }
}



PCR_DAL_DECL bool isDatasetType(
         std::string const& name)
{
  assert(NR_DATASET_TYPES == 5);

  return name == "raster" ||
         name == "feature" ||
         name == "block"  ||
         name == "table"  ||
         name == "matrix";
}



PCR_DAL_DECL std::string datasetTypeToString(
         DatasetType type)
{
  std::string result;

  switch(type) {
    case RASTER:   { result = "raster"; break; }
    case FEATURE:  { result = "feature"; break; }
    case VECTOR:   { result = "vector"; break; }
    case BLOCK:    { result = "block";  break; }
    case TABLE:    { result = "table";  break; }
    case MATRIX:   { result = "matrix"; break; }
    case CONSTANT: { result = "constant"; break; }
    case GRAPHIC:  { result = "graphic"; break; }
    case NR_DATASET_TYPES: { result = "unknown"; break; }
  }

  return result;
}



PCR_DAL_DECL DatasetType stringToDatasetType(
         std::string const& name)
{
  if(name == "raster") {
    return RASTER;
  }
  else if(name == "feature") {
    return FEATURE;
  }
  else if(name == "table") {
    return TABLE;
  }
  else if(name == "block") {
    return BLOCK;
  }
  else if(name == "matrix") {
    return MATRIX;
  }
  else if(name == "constant") {
    return CONSTANT;
  }
  else {
    assert(false);
    return NR_DATASET_TYPES;
  }
}



PCR_DAL_DECL std::string filenameConventionToString(
         FilenameConvention convention)
{
  std::string result;

  switch(convention) {
    case DALConvention: {
      result = "DAL";
      break;
    }
    case PCRConvention: {
      result = "PCRaster";
      break;
    }
    default: {
      assert(false);
      result = "unknown";
    }
  }

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Finish. We probably want only a max amount of set values in the
             string: {a, b, ..., z}
*/
PCR_DAL_DECL std::string dimensionToString(
         Dimension const& dimension)
{
  std::string result;

  switch(dimension.meaning()) {
    case Scenarios: {
      assert(dimension.discretisation() == ExactDiscretisation);
      result = "scenarios{";

      if(dimension.nrValues() > 0) {
        size_t i;

        for(i = 0; i < dimension.nrValues() - 1; ++i) {
          result += dimension.value<std::string>(i);
          result += ", ";
        }

        result += dimension.value<std::string>(i);
      }

      result += "}";

      break;
    }
    case CumulativeProbabilities: {
      assert(dimension.discretisation() == RegularDiscretisation);
      result = "cumulative probabilities";

      assert(dimension.nrValues() == 3);
      if(dimension.nrValues() == 3) {
        result += (boost::format("[%1%, %2%, %3%]")
              % dimension.value<float>(0)
              % dimension.value<float>(1)
              % dimension.value<float>(2)).str();
      }

      break;
    }
    case Samples: {
      result = "samples";
      break;
    }
    case Time: {
      assert(dimension.discretisation() == RegularDiscretisation);
      result = "time";

      assert(dimension.nrValues() == 3);
      if(dimension.nrValues() == 3) {
        result += (boost::format("[%1%, %2%, %3%]")
              % dimension.value<size_t>(0)
              % dimension.value<size_t>(1)
              % dimension.value<size_t>(2)).str();
      }

      break;
    }
    case Space: {
      result = "space";

      switch(dimension.discretisation()) {
        case RegularDiscretisation: {
          assert(dimension.nrValues() == 1);
          RasterDimensions const& rasterDimensions(
              dimension.value<RasterDimensions>(0));

          if(dimension.nrValues() == 1) {
            result += (boost::format("[%1%, %2%, %3%]")
                % rasterDimensions.nrRows()
                % rasterDimensions.nrCols()
                % rasterDimensions.cellSize()
                ).str();
          }

          break;
        }
        case BorderedDiscretisation: {
          assert(dimension.nrValues() == 1);
          SpaceDimensions const& spaceDimensions(
              dimension.value<SpaceDimensions>(0));

          if(dimension.nrValues() == 1) {
            result += (boost::format("[(%1%, %2%) - (%3%, %4%)]")
                % spaceDimensions.west()
                % spaceDimensions.north()
                % spaceDimensions.east()
                % spaceDimensions.south()
                ).str();
          }

          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



PCR_DAL_DECL std::string dataSpaceToString(
         DataSpace const& space)
{
  std::string result;

  if(space.rank() == 0) {
    result = "/";
  }
  else {
    for(size_t i = 0; i < space.rank(); ++i) {
      result += (boost::format("/%1%")
         % dimensionToString(space.dimension(i))).str();
    }
  }

  return result;
}



std::string nameAndSpaceToString(
         std::string const& name,
         DataSpace const& space)
{
  return name + "(" + dataSpaceToString(space) + ")";
}



std::string nameToString(
         std::string const& name,
         DataSpace const& space,
         DatasetType type)
{
  return name + "(" + dataSpaceToString(space) + ", " +
         datasetTypeToString(type) + ")";
}



PCR_DAL_DECL std::string coordinateToString(
         Dimension const& dimension,
         size_t index)
{
  std::string result;

  switch(dimension.meaning()) {
    case Scenarios: {
      result = dimension.coordinate<std::string>(index);
      break;
    }
    case CumulativeProbabilities: {
      // Tricky: conversion from float to string might give numbers at more
      // decimal places than in the original string.
      // boost::format gives right string version of float number.
      result = (boost::format("%1%")
         % dimension.coordinate<float>(index)).str();
      break;
    }
    case Samples:
    case Time: {
      result = boost::lexical_cast<std::string>(
         dimension.coordinate<size_t>(index));
      break;
    }
    case Space: {
      // Cannot be done.
      assert(false);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



PCR_DAL_DECL std::string coordinateToString(
         DataSpace const& space,
         DataSpaceAddress const& address,
         size_t index)
{
  std::string result = "unset";

  if(address.isValid(index)) {
    Dimension const& dimension = space.dimension(index);

    switch(dimension.meaning()) {
      case Scenarios: {
        result = address.coordinate<std::string>(index);
        break;
      }
      case CumulativeProbabilities: {
        // Tricky: conversion from float to string might give numbers at more
        // decimal places than in the original string.
        // boost::format gives right string version of float number.
        result =
              (boost::format("%1%") % address.coordinate<float>(index)).str();
        break;
      }
      case Samples:
      case Time: {
        result = boost::lexical_cast<std::string>(
              address.coordinate<size_t>(index));
        break;
      }
      case Space: {
        SpatialCoordinate const& spatialCoordinate(
           address.coordinate<SpatialCoordinate>(index));

        result = (boost::format("(%1%, %2%)")
           % spatialCoordinate.x()
           % spatialCoordinate.y()
           ).str();

        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

  return result;
}



PCR_DAL_DECL std::string dataSpaceAddressToString(
         DataSpace const& space,
         DataSpaceAddress const& address)
{
  std::string result;
  // std::string result(dataSpaceToString(space));
  // result += "(";

  for(size_t i = 0; i < space.rank(); ++i) {
    if(i > 0) {
      result += ", ";
    }

    if(!address.isValid(i)) {
      result += "x";
    }
    else {
      result += coordinateToString(space, address, i);
    }
  }

  // result += ")";

  return result;
}



/*
void throwUnsupportedFormat(std::string const& name)
{
  throw Exception((boost::format(
         "Data source %1%: unsupported format") % name).str());
}
*/



void throwDataSourceError(
         std::string const& name,
         std::string const& description)
{
  throw Exception((boost::format("Data source %1%:\n%2%")
         % name
         % description).str());
}



void throwDataSourceError(
         std::string const& name,
         DatasetType type,
         std::string const& description)
{
  throw Exception((boost::format("Data source %1%(%2%):\n%3%")
         % name
         % datasetTypeToString(type)
         % description).str());
}



void throwDataSourceError(
         std::string const& name,
         DataSpace const& space,
         std::string const& description)
{
  if(space.isEmpty()) {
    throwDataSourceError(name, description);
  }

  throw Exception((boost::format("Data source %1% in %2%:\n%3%")
         % name
         % dataSpaceToString(space)
         % description).str());
}



void throwDataSourceError(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& description)
{
  if(space.isEmpty()) {
    throwDataSourceError(name, description);
  }

  throw Exception((boost::format("Data source %1% at %2%:\n%3%")
         % name
         % dataSpaceAddressToString(space, address)
         % description).str());
}



void throwDataSourceError(
         std::string const& name,
         DatasetType type,
         DataSpace const& space,
         std::string const& description)
{
  if(space.isEmpty()) {
    throwDataSourceError(name, type, description);
  }

  throw Exception((boost::format("Data source %1%(%2%) in %3%:\n%4%")
         % name
         % datasetTypeToString(type)
         % dataSpaceToString(space)
         % description).str());
}



void throwDataSourceError(
         std::string const& name,
         DatasetType type,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& description)
{
  if(space.isEmpty()) {
    throwDataSourceError(name, type, description);
  }

  throw Exception((boost::format("Data source %1%(%2%) at %3%:\n%4%")
         % name
         % datasetTypeToString(type)
         % dataSpaceAddressToString(space, address)
         % description).str());
}



PCR_DAL_DECL void throwCannotBeOpened(
         std::string const& name)
{
  throwDataSourceError(name, "cannot be opened");
}



PCR_DAL_DECL void throwCannotBeOpened(
         std::string const& name,
         DatasetType type,
         std::string const& reason)
{
  if(reason.empty()) {
    throwDataSourceError(name, type, "cannot be opened");
  }
  else {
    throwDataSourceError(name, type,
         (boost::format("cannot be opened: %1%")
         % reason).str());
  }
}



PCR_DAL_DECL void throwCannotBeOpened(
         std::string const& name,
         DataSpace const& space,
         DatasetType type)
{
  throwDataSourceError(name, type, space, "cannot be opened");
}



/*
void throwCannotBeOpened(std::string const& name,
         DatasetType type, size_t timeStep)
{
  throwDataSourceError(name, type,
         (boost::format("at time step %1%: cannot be opened")
         % timeStep).str());
}
*/



PCR_DAL_DECL void throwCannotBeOpened(
         std::string const& name,
         DataSpace const& space)
{
  throwDataSourceError(name, space, "cannot be opened");
}



PCR_DAL_DECL void throwCannotBeOpened(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address)
{
  throwDataSourceError(name, space, address, "cannot be opened");
}



PCR_DAL_DECL void throwCannotBeOpened(
         std::string const& name,
         DatasetType type,
         DataSpace const& space,
         DataSpaceAddress const& address)
{
  throwDataSourceError(name, type, space, address, "cannot be opened");
}



/*
void throwCannotBeOpened(
         std::string const& name,
         DatasetType type,
         std::string const& reason)
{
  if(reason.empty()) {
    throwDataSourceError(name, type, "cannot be opened");
  }
  else {
    throwDataSourceError(name, type,
         (boost::format("cannot be opened: %1%") % reason).str());
  }
}
*/



void throwCannotBeCreated(
         std::string const& name,
         DatasetType type,
         std::string const& reason)
{
  if(reason.empty()) {
    throwDataSourceError(name, type, "cannot be created");
  }
  else {
    throwDataSourceError(name, type,
         (boost::format("cannot be created: %1%")
         % reason).str());
  }
}



void throwCannotBeClosed(
         std::string const& name,
         DatasetType type)
{
  throwDataSourceError(name, type, "cannot be closed");
}



void throwCannotReadCell(
    std::string const& name,
    DatasetType type)
   // hack CW see CSFMapTest::testError std::string driverSpecificMsg)
{
  // throwDataSourceError(name, type, "cannot read cell"+driverSpecificMsg);
  throwDataSourceError(name, type, "cannot read cell");
}



void throwCannotReadCell(
         std::string const& name,
         DatasetType type,
         DataSpace const& space,
         DataSpaceAddress const& address)
{
  throwDataSourceError(name, type, space, address, "cannot read cell");
}



void throwCannotReadCells(
         std::string const& name,
         DatasetType type)
{
  throwDataSourceError(name, type, "cannot read cells");
}



void throwCannotReadCells(
         std::string const& name,
         DatasetType type,
         DataSpace const& space,
         DataSpaceAddress const& address)
{
  throwDataSourceError(name, type, space, address, "cannot read cells");
}



void throwCannotReadRecord(
         std::string const& name,
         DatasetType type, size_t record)
{
  throwDataSourceError(name, type,
         (boost::format("cannot read record %1%")
         % record).str());
}



void throwCannotWrite(
         std::string const& name,
         DatasetType type,
         std::string const& reason)
{
  if(reason.empty()) {
    throwDataSourceError(name, type, "cannot write");
  }
  else {
    throwDataSourceError(name, type,
         (boost::format("cannot write: %1%")
         % reason).str());
  }
}



void throwCannotWriteCells(
         std::string const& name,
         DatasetType type)
{
  throwDataSourceError(name, type, "cannot write cells");
}



void throwCannotWriteRecord(
         std::string const& name,
         DatasetType type,
         size_t record, std::string const& reason)
{
  throwDataSourceError(name, type,
         (boost::format("cannot write record %1%: %2%")
         % record
         % reason).str());
}



void throwCannotBeRead(
         std::string const& name,
         DatasetType type,
         std::string const& reason)
{
  if(reason.empty()) {
    throwDataSourceError(name, type, "cannot read");
  }
  else {
    throwDataSourceError(name, type,
         (boost::format("cannot read: %1%")
         % reason).str());
  }
}



void throwCannotReadHeader(
         std::string const& name,
         DatasetType type)
{
  throwDataSourceError(name, type, "cannot read header");
}



void throwCannotBeDeleted(
         std::string const& name,
         DatasetType type,
         std::string const& reason)
{
  throwDataSourceError(name, type,
         (boost::format("cannot be deleted: %1%") % reason).str());
}



void throwCannotReadLegend(
         std::string const& name)
{
  throwDataSourceError(name, RASTER, "cannot read legend");
}



/*
void throwUnsupportedDatasetType(std::string const& name,
         DatasetType type)
{
  throw Exception((boost::format(
         "Data source %1% with %2% data: unsupported dataset type")
         % name
         % datasetTypeToString(type)).str());
}
*/



// //! Returns the type of the data in \a name.
// /*!
//   \param     name Name of datasource.
//   \return    Dataset type.
//   \exception Exception If format is unsupported.
//   \todo      Get rid of stackinfo stuff. See overload below.
//   \todo      Remove? See Dal::datasetType(std::string const&,
//              DataSpace const&).
// */
// DatasetType datasetType(std::string const& name)
// {
//   try {
//     // Raster?
//     {
//       StackInfo info(name);
//       boost::shared_ptr<Raster> raster;
//       RasterDal dal(true);
// 
//       if(!info.isDynamic()) {
//         raster.reset(dal.open(info.name()));
//       }
//       else {
//         info.scanFirst();
//         if(info.size()) {
//           raster.reset(dal.open(
//                 info.filename(*info.begin()).string()));
//         }
//       }
// 
//       if(raster) {
//         return RASTER;
//       }
//     }
// 
//     // Table?
//     {
//       boost::shared_ptr<Table> table(TableDal(true).open(name));
//       if(table) {
//         return TABLE;
//       }
//     }
// 
//     // Matrix?
//     {
//       boost::shared_ptr<Matrix> matrix(MatrixDal(true).open(name));
//       if(matrix) {
//         return MATRIX;
//       }
//     }
//   }
//   catch(Exception& exception) {
//     throwDataSourceError(name, exception.message());
//   }
// 
//   return NR_DATASET_TYPES;
// }



PCR_DAL_DECL std::vector<std::string> scenarios(
         DataSpace const& space)
{
  std::vector<std::string> result;

  if(space.hasScenarios()) {
    Dimension dimension = space.dimension(space.indexOf(Scenarios));
    for(size_t i = 0; i < dimension.nrValues(); ++i) {
      result.push_back(dimension.value<std::string>(i));
    }
  }

  return result;
}



PCR_DAL_DECL std::set<size_t> timeSteps(
         DataSpace const& space)
{
  std::set<size_t> result;

  if(space.hasTime()) {
    size_t index = space.indexOf(Time);
    Dimension dimension = space.dimension(index);

    assert(dimension.discretisation() == RegularDiscretisation);
    assert(dimension.nrValues() == 3);

    size_t first = dimension.value<size_t>(0);
    size_t last = dimension.value<size_t>(1);
    size_t step = dimension.value<size_t>(2);

    for(size_t i = first; i <= last; i += step) {
      result.insert(i);
    }
  }

  return result;
}



std::set<float> quantiles(
         DataSpace const& space)
{
  std::set<float> result;

  if(space.hasCumProbabilities()) {
    size_t index = space.indexOf(CumulativeProbabilities);
    Dimension dimension = space.dimension(index);

    assert(dimension.discretisation() == RegularDiscretisation);
    assert(dimension.nrValues() == 3);

    float first = dimension.value<float>(0);
    float last = dimension.value<float>(1);
    float step = dimension.value<float>(2);

    for(float i = first; i <= last; i += step) {
      result.insert(i);
    }
  }

  return result;
}



template<>
PCR_DAL_DECL std::string timeStep<std::string>(
         DataSpace const& space,
         DataSpaceAddress const& address)
{
  return boost::lexical_cast<std::string>(
         address.coordinate<size_t>(space.indexOf(Time)));
}



template<>
PCR_DAL_DECL size_t timeStep<size_t>(
         DataSpace const& space,
         DataSpaceAddress const& address)
{
  return address.coordinate<size_t>(space.indexOf(Time));
}



/// template<>
/// std::string colIndex<std::string>(
///          DataSpace const& space,
///          DataSpaceAddress const& address)
/// {
///   assert(space.hasRaster());
///   // TODO test, + 1?
///   return boost::lexical_cast<std::string>(
///          address.coordinate<size_t>(space.indexOf(Space)));
/// }
/// 
/// 
/// 
/// template<>
/// size_t colIndex<size_t>(
///          DataSpace const& space,
///          DataSpaceAddress const& address)
/// {
///   assert(space.hasRaster());
///   // TODO test, + 1?
///   return address.coordinate<size_t>(space.indexOf(Space));
/// }
/// 
/// 
/// 
/// template<>
/// std::string rowIndex<std::string>(
///          DataSpace const& space,
///          DataSpaceAddress const& address)
/// {
///   assert(space.hasRaster());
///   return boost::lexical_cast<std::string>(
///          address.coordinate<size_t>(space.indexOf(Space)));
/// }
/// 
/// 
/// 
/// template<>
/// size_t rowIndex<size_t>(
///          DataSpace const& space,
///          DataSpaceAddress const& address)
/// {
///   assert(space.hasRaster());
///   return address.coordinate<size_t>(space.indexOf(Space));
/// }



//! Splits \a name into the data set name part and the selection part.
/*!
  \param     name Data set name with possibly a selection part.
  \return    Tuple with the name and a vector with the selection tokens. If
             \a name does not contain a selection specification than the
             same name will be returned and the returned vector will be
             empty. If \a name does contain a selection specification, but
             it is empty (as in blabla{}), than the name before the
             specification will be returned and the returned vector will be
             empty.

  A selection specification is a collection of words seperated by comma's and
  optional whitespace and enclosed by curly braces. The closing brace must be
  the last character in \a name, otherwise the whole name is regarded as a
  dataset name without a selection specification.

  Selection tokens returned are stripped from whitespace, the returned
  dataset name is not.
*/
PCR_DAL_DECL boost::tuple<std::string, std::vector<std::string> >
splitNameAndSelection(
         std::string name)
{
  size_t left = name.rfind('{');
  size_t right = name.rfind('}');
  std::vector<std::string> tokens;

  if(right == name.size() - 1 && left < right) {
    if(right - left > 1) {
      std::string selection(name.substr(left + 1, right - left - 1));
      boost::split(tokens, selection, boost::is_any_of(","));
      // std::for_each(tokens.begin(), tokens.end(), boost::trim);

      for(std::vector<std::string>::iterator it = tokens.begin();
           it != tokens.end(); ++it) {
        boost::trim(*it);
      }
    }

    name = name.substr(0, left);
  }

  return boost::make_tuple(name, tokens);
}



/*
std::string replaceEnvironmentVariables(
         std::string const& string)
{
  return string;
}
*/



// template<typename T>
// inline size_t multiplierToIntegral(
//          T value)
// {
//   typedef boost::test_tools::close_at_tolerance<T> Tester;
// 
//   size_t i = 1;
// 
//   while(1) {
//     std::cout << i << '\t'
//               << i * value << ": " << '\t'
//               << Tester(T(1e-1), boost::test_tools::FPC_STRONG)(fmod(i * value, 1.0), 0.0) << '\t'
//               << Tester(T(1e-2), boost::test_tools::FPC_STRONG)(fmod(i * value, 1.0), 0.0) << '\t'
//               << Tester(T(1e-3), boost::test_tools::FPC_STRONG)(fmod(i * value, 1.0), 0.0) << '\t'
//               << Tester(T(1e-4), boost::test_tools::FPC_STRONG)(fmod(i * value, 1.0), 0.0) << '\t'
//               << std::endl;
//     if(comparable(fmod(i * value, 1.0), 0.0)) {
//       break;
//     }
// 
//     i *= 10;
//   }
// 
//   return i;
// }
// 
// 
// 
// double gcdDouble(
//          double a,
//          double b)
// {
//   return gcdNew(a, b);
// 
//   size_t i = multiplierToIntegral(a);
//   size_t j = multiplierToIntegral(b);
// 
//   std::cout << i << '\t' << j << std::endl;
// 
//   a = rintf(i * a);
//   b = rintf(j * b);
// 
//   if(i > j) {
//     b *= i / j;
//   }
//   else if(j > i) {
//     a *= j / i;
//   }
// 
//   // Use the real gcd with converted arguments, convert back the result.
//   return gcd(static_cast<long int>(a), static_cast<long int>(b)) /
//          std::max(i, j);
// }
// 
// 
// 
// //! Determines the greatest common divisor of two floating point numbers.
// /*!
//   \param     a First number.
//   \param     b Second number.
//   \return    Greatest common divisor of \a and \a b.
//   \warning   Not sure whether this works with lots of non-zero's behind the
//              decimal. This probably needs to be considered a hack.
// */
// #ifdef _MSC_VER
// // bug otherwise see gcd in dal_utilstest.cc
// #pragma optimize( "", off )
// #endif
// float gcdFloat(
//          float a,
//          float b)
// {
//   return gcdDouble(a, b);
// 
// 
// 
// 
//   std::cout << std::endl;
//   std::cout << a << '\t' << b << std::endl;
//   // 0.2     0.2
//   //
//   // 0.2     0.2
//   // 0.2
//   // 2       0
//   // 20      0
//   // 200     0
//   // 2000    0
//   // 20000   0
//   // 200000  0
//   // 2e+06   0
//   // 2e+07   0
//   // 2e+08   0
//   // 2e+09   0
//   // 2e+10   0
//   // 2e+11   0
//   // 2e+12   0
//   // 2e+13   0
//   // 2e+14   0
//   // 100?: 1e+15
//   // 100?: 1e+15
//   // 2e+14   2e+14   1e+15
// 
//   float multiplierA = 1.0f;
// 
//   // Determine how to convert a to an integral.
//   std::cout << fmod(multiplierA * a, 1.0) << std::endl;
//   // 0.2
//   while(!comparable(fmod(multiplierA * a, 1.0), 0.0)) {
//     multiplierA *= 10.0f;
//   std::cout << multiplierA * a << '\t' << fmod(multiplierA * a, 1.0) << '\t' << comparable(fmod(multiplierA * a, 1.0), 0.0) << std::endl;
//   }
// 
//   std::cout << "100?: " << multiplierA << std::endl;
// 
//   a = rintf(multiplierA * a);
// 
//   float multiplierB = 1.0f;
// 
//   // Determine how to convert b to an integral.
//   while(!comparable(fmodf(multiplierB * b, 1.0f), 0.0f)) {
//     multiplierB *= 10.0f;
//   }
// 
//   std::cout << "100?: " << multiplierB << std::endl;
// 
//   b = rintf(multiplierB * b);
// 
//   if(multiplierA > multiplierB) {
//     b *= multiplierA / multiplierB;
//   }
//   else if(multiplierB > multiplierA) {
//     a *= multiplierB / multiplierA;
//   }
// 
//   std::cout << a << '\t' << b << '\t' << std::max(multiplierA, multiplierB) << std::endl;
// 
//   // Use the real gcd with converted arguments, convert back the result.
//   return gcd(static_cast<long int>(a), static_cast<long int>(b)) /
//          std::max(multiplierA, multiplierB);
// }
// #ifdef _MSC_VER
// #pragma optimize( "", on )
// #endif



//! Returns a copy of \a space with the coordinates for dimension \a indexOfCoordinate replaced by the single value pointed to by \a indexOfCoordinate.
/*!
  \param     space Data space to copy.
  \param     indexOfDimension Index of dimension to update.
  \param     indexOfCoordinate Index of coordinate to put in updated dimension.
  \return    Copied and updated data space.
  \warning   Currently, this function assumes the dimension being narrowed is
             a scenarios dimension.
*/
PCR_DAL_DECL DataSpace dataSpaceWithNarrowedDimension(
         DataSpace const& space,
         size_t indexOfDimension,
         size_t indexOfCoordinate)
{
  assert(space.dimension(indexOfDimension).meaning() == Scenarios);
  assert(indexOfCoordinate <
         space.dimension(indexOfDimension).nrCoordinates());

  DataSpace result(space);
  result.dimension(indexOfDimension).setValue(
         space.dimension(indexOfDimension).coordinate<std::string>(
              indexOfCoordinate));

  return result;
}



//! Erases information about dimension \a meaning from \a space and \a address.
/*!
  \param     space Space to update.
  \param     address Address to update.
  \param     Meaning of dimension to erase information of.
  \return    \a space and \a address are updated.

  For all occurrences of dimensions with \a meaning the information is erased.
*/
void eraseDimension(
         DataSpace& space,
         DataSpaceAddress& address,
         Meaning meaning)
{
  assert(space.size() == address.size());

  for(int i = 0; i < static_cast<int>(space.size()); ++i) {
    if(space.dimension(i).meaning() == meaning) {
      space.eraseDimension(i);
      address.eraseCoordinate(i);
      --i;
    }
  }
}



//! Returns a set with field names corresponding to the dimensions present in the \a space passed in.
/*!
  \param     space Data space to return field names for.
  \return    Set of field names.

  The folowing field names are returned for each type of supported dimension:
  - scenarios: scenario
  - time: date
  - cumulative probability: quantile
*/
std::set<std::string> dataSpaceToFieldNames(
         DataSpace const& space)
{
  std::set<std::string> result;

  for(size_t i = 0; i < space.rank(); ++i) {
    Dimension const& dimension(space.dimension(i));

    switch(dimension.meaning()) {
      case Scenarios: {
        result.insert("scenario");
        break;
      }
      case Time: {
        result.insert("date");
        break;
      }
      case CumulativeProbabilities: {
        result.insert("quantile");
        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

  return result;
}



//! Returns a set with field names corresponding to the dimensions present in the \a space, for which \a address has valid coordinates.
/*!
  \param     space Data space to return field names for.
  \param     address Data space address to check coordinates of.
  \return    Set of field names.
  \sa        std::set<std::string> dataSpaceToFieldNames(DataSpace const&)
*/
std::set<std::string> dataSpaceAddressToFieldNames(
         DataSpace const& space,
         DataSpaceAddress const& address)
{
  std::set<std::string> result;

  for(size_t i = 0; i < space.rank(); ++i) {
    if(!address.isValid(i)) {
      Dimension const& dimension(space.dimension(i));

      switch(dimension.meaning()) {
        case Scenarios: {
          result.insert("scenario");
          break;
        }
        case Time: {
          result.insert("date");
          break;
        }
        case CumulativeProbabilities: {
          result.insert("quantile");
          break;
        }
        default: {
          assert(false);
          break;
        }
      }
    }
  }

  return result;
}



std::string dataSpaceAddressToSqlQuery(
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& tableName,
         std::string const& fieldName)
{
  std::vector<std::string> fieldNames;
  fieldNames.push_back(fieldName);

  return dataSpaceAddressToSqlQuery(space, address, tableName, fieldNames);
}



std::string dataSpaceAddressToSqlQuery(
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& tableName,
         std::vector<std::string> fieldNames)
{
  assert(!tableName.empty());
  assert(!fieldNames.empty());
  // assert(std::find(fieldNames.begin(), fieldNames.end(), "date") ==
  //        fieldNames.end());
  // assert(std::find(fieldNames.begin(), fieldNames.end(), "quantile") ==
  //        fieldNames.end());
  // assert(std::find(fieldNames.begin(), fieldNames.end(), "scenario") ==
  //        fieldNames.end());

  std::vector<std::string> tmpFieldNames;
  std::vector<std::string> predicates;

  for(size_t i = 0; i < space.rank(); ++i) {
    Dimension const& dimension(space.dimension(i));

    switch(dimension.meaning()) {
      case Scenarios: {
        if(std::find(fieldNames.begin(), fieldNames.end(), "scenario") ==
              fieldNames.end()) {
          tmpFieldNames.push_back("scenario");
        }

        if(address.isValid(i)) {
          predicates.push_back((boost::format("scenario='%1%'")
              % address.coordinate<std::string>(i)).str());
        }

        break;
      }
      case Time: {
        if(std::find(fieldNames.begin(), fieldNames.end(), "date") ==
              fieldNames.end()) {
          tmpFieldNames.push_back("date");
        }

        if(address.isValid(i)) {
          predicates.push_back((boost::format("date=%1%")
              % address.coordinate<size_t>(i)).str());
        }

        break;
      }
      case CumulativeProbabilities: {
        if(std::find(fieldNames.begin(), fieldNames.end(), "quantile") ==
              fieldNames.end()) {
          tmpFieldNames.push_back("quantile");
        }

        if(address.isValid(i)) {
          predicates.push_back((boost::format("quantile=%1%")
              % address.coordinate<float>(i)).str());
        }

        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

  fieldNames.insert(fieldNames.begin(), tmpFieldNames.begin(),
         tmpFieldNames.end());

  std::string result =
         "SELECT " + boost::algorithm::join(fieldNames, ",") +
              " FROM " + tableName;

  if(!predicates.empty()) {
    result += " WHERE " + boost::algorithm::join(predicates, " AND ");
  }

  return result;
}

} // namespace
