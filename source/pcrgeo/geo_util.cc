#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_UTIL
#include "geo_util.h"
#define INCLUDED_GEO_UTIL
#endif

// Std
#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_BOOST_MATH_TR1
#include <boost/math/tr1.hpp>
#define INCLUDED_BOOST_MATH_TR1
#endif
using namespace boost::math; // then use tr1

// Pcr
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif

#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

#ifndef INCLUDED_GEO_AVERAGEFILTER
#include "geo_averagefilter.h"
#define INCLUDED_GEO_AVERAGEFILTER
#endif

#ifndef INCLUDED_GEO_CSFMAP
#include "geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif

#ifndef INCLUDED_GEO_CSFSTACKNAME
#include "geo_csfstackname.h"
#define INCLUDED_GEO_CSFSTACKNAME
#endif

#ifndef INCLUDED_GEO_EXCEPTION
#include "geo_exception.h"
#define INCLUDED_GEO_EXCEPTION
#endif

#ifndef INCLUDED_GEO_FILTERENGINE
#include "geo_filterengine.h"
#define INCLUDED_GEO_FILTERENGINE
#endif



/*!
  \file
    a bunch of functions for various conversions and tests

  \todo
    split up in geo_testFileTypes and geo_strEnums ofzo

  \todo
   many functions in here can is of type strToEnum and vice versa, implement
   a template that offers a "2-way map" and also support case insensitivity
*/



//------------------------------------------------------------------------------

static const std::string YIncrB2TString = ("y increases from bottom to top");
static const std::string YIncrT2BString = ("y increases from top to bottom");

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

/*!
  \param   s Stream to check format of, the stream position is not
           altered
  \return  True if \a s is geo-eas formatted.

  It is decided that the stream is geo-eas formatted if
  the second line contains a single integer and nothing else.

  \todo
    crashed on tss file with no header due to the
    fromString problems as described in com_strlibtest.cc
*/
bool geo::geoEasFormat(std::istream &s)
{
  // Save current stream position.
  std::streampos p = s.tellg();

  // Ignore the first line.
  std::string l;
  std::getline(s, l);

  // Read the second line.
  std::getline(s, l);

  // Check if the string contains one single integer and nothing more.
  bool result = true;
  try {
    com::removeFrontEndSpace(l);
    (void)com::strToInt(l);     // Ignore result.
  } catch (std::range_error) {
    result = false;
  }

  // Restore the original stream position.
  s.seekg(p);

  return result;
}



bool geo::isCSFStack(const com::PathName& pn)
{
  bool result;

  try {
    geo::CSFStackName sn(pn, false);
    com::PathName fn = sn.fileName();
    geo::CSFMap m(fn);
    result = true;
  }
  catch(geo::NotA_PCRasterMap& /* e */) {
    result = false;
  }

  return result;
}



/*!
  \param     pn Path name of file to test.
  \return    true if \a pn is the name of a block file.
  \sa        isCSFStack(const com::PathName&)
*/
bool geo::isBlock(const com::PathName& pn)
{
  // Open file.
  std::ifstream s;
  com::open(s, pn);

  // Read first line: 5 tokens.
  std::string line;
  size_t n;
  std::getline(s, line);
  n = com::split(line).size();
  if(n != 5) {
    return false;
  }

  // Read second line: projection.
  std::getline(s, line);
  if(line != YIncrB2TString && line != YIncrT2BString) {
    return false;
  }

  return true;
}



//! Returns true if \a pathName points to a time series file.
/*!
  \param     pathName Path name of file to test.
  \return    true or false.
  \sa        isColumnFile(const com::PathName&)

  First the header is read and than the first line of the data section.
  This function returns true if the first line of the data section contains
  an equal amount of values as there are columns according to the header.
*/
bool geo::isTimeSeriesFile(const com::PathName& pathName)
{
  // Open file.
  std::ifstream stream;
  com::open(stream, pathName);

  if(geoEasFormat(stream)) {
    return true;
  }

  // else file with no header

  std::string line;
  std::getline(stream, line);
  std::vector<std::string> fields = com::split(line);

  // Time series file must have at least 2 columns.
  if(fields.size() < 2) {
    return false;
  }

  // First column in time series file is time step and should be a
  // positive integer value.
  try {
    (void)com::strToSize_t(fields[0]);
  } catch (std::range_error&) {
    return false;
  }

  // Check if all columns, but the first, contain numbers (first column is
  // checked above).
  for(size_t i = 1; i < fields.size(); ++i) {
    try {
      (void)com::strToDouble(fields[i]);
    } catch (std::range_error&) {
      return false;
    }
  }

  // File seems to contain one or more time series.
  return true;

/*
    std::getline(stream, line);

    // Line 2: header, number of columns.
    std::getline(stream, line);

    // Line 3: header, description of first column.
    std::getline(stream, line);

    // result of pcrcalc
    if(line == "timestep") {
      return true;
    }

  return false;

  // Line 1: header, description.
  std::string description;
  std::getline(stream, description);

  // Line 2: header, number of columns.
  size_t nrCols;
  stream >> nrCols;

  // Line 3: header, time column description.
  std::string timeDescription;
  std::getline(stream, timeDescription);

  // Line 4 -- nrCols + 3: header, description of 2nd and following columns.
  std::string colDescription;
  for(size_t i = 0; i < 3 + nrCols; ++i) {
    std::getline(stream, colDescription);
  }

  // Line nrCols + 4: data section.
  // Determine number of values in line.
  std::string row;
  std::getline(stream, row);
  size_t nrValues = com::split(row).size();

  // Stream must be in a valid state, the description of the first columns
  // must be "timestep" and the nrValues read on one line must
  // equal the number of columns read.
  return stream && timeDescription == std::string("timestep") &&
                   nrValues == nrCols;
*/
}



/*!
  \param     pn Path name of file to test.
  \return    true if \a pn is the name of file containing columns.
  \sa        isCSFStack(const com::PathName&), isBlock(const com::PathName&)

  A file is considered to be formatted in columns if the first 5 lines contain
  equal numbers of tokens and if this number of tokens is larger than 0.
*/
bool geo::isColumnFile(const com::PathName& pn)
{
  // Open file.
  std::ifstream stream;
  com::open(stream, pn);

  // Read and test i lines.
  std::string line;
  size_t nc, n;

  std::getline(stream, line);
  nc = com::split(line).size();
  if(nc == 0) {
    return false;
  }

  // Try to check the first 4 lines.
  for(size_t i = 1; i < 4; ++i) {

    if(stream.eof()) {
      // Benefit of the doubt.
      return true;
    }
    else {
      // Another line to read.
      std::getline(stream, line);
      n = com::split(line).size();
      if(n != nc) {
        // Number of tokens doesn't equal number of tokens in first line.
        return false;
      }
    }
  }

  // Return result.
  return true;
}



bool geo::isModelScriptFile(const com::PathName& /* pathName */)
{
  // Return true if
  return true;
}



// geo::DataType geo::dataType(const com::PathName& pathName)
// {
//   DataType dataType = DT_INVALID;
//
//   if(isCSFStack(pathName)) {
//     dataType = STACK;
//   }
//   else if(isTimeSeriesFile(pathName)) {
//     dataType = TIMESERIES;
//   }
//   else if(isModelScriptFile(pathName)) {
//     dataType = MODELSCRIPT;
//   }
//   else if(isColumnFile(pathName)) {
//     dataType = POINTS;
//   }
//   else if(isBlock(pathName)) {
//     dataType = BLOCK;
//   }
//
//   return dataType;
// }
//
//
//
// bool geo::isSpatial(DataType dataType)
// {
//   return dataType == STACK || dataType == POINTS;
// }
//
//
//
// bool geo::isTimeSeries(DataType dataType)
// {
//   return dataType == TIMESERIES;
// }
//
//
//
// bool geo::isModelScript(DataType dataType)
// {
//   return dataType == MODELSCRIPT;
// }



/*!
  \param     p CSF Projection type to convert.
  \return    The geo::Projection type version of \a p.
*/
geo::Projection geo::csfProjToGeo(CSF_PT p)
{
  Projection proj;

  if(p == PT_YINCT2B)
    proj = YIncrT2B;
  else if(p == PT_YDECT2B)
    proj = YIncrB2T;
  else
    proj = IllegalProjection;

  return proj;
}



CSF_PT geo::geoProjToCsf(Projection p)
{
  CSF_PT proj;

  if(p == YIncrT2B)
    proj = PT_YINCT2B;
  else if(p == YIncrB2T)
    proj = PT_YDECT2B;
  else
    proj = PT_UNDEFINED;

  return proj;
}



std::string geo::projToStr(Projection p)
{
  std::string s;

  if(p == YIncrB2T)
    s = YIncrB2TString;
  else if(p == YIncrT2B)
    s = YIncrT2BString;
  else
    s = "illegal projection";

  return s;
}



geo::Projection geo::strToProj(const std::string &s)
{
  Projection p;

  if(s == YIncrB2TString)
    p = YIncrB2T;
  else if(s == YIncrT2BString)
    p = YIncrT2B;
  else
    p = IllegalProjection;

  return p;
}



CSF_VS geo::strToValueScale(const std::string &str)
{
  CSF_VS vs;

  if(str == "BOOLEAN") {
    vs = VS_BOOLEAN;
  }
  else if(str == "NOMINAL") {
    vs = VS_NOMINAL;
  }
  else if(str == "ORDINAL") {
    vs = VS_ORDINAL;
  }
  else if(str == "SCALAR") {
    vs = VS_SCALAR;
  }
  else if(str == "DIRECTIONAL") {
    vs = VS_DIRECTION;
  }
  else if(str == "LDD") {
    vs = VS_LDD;
  }
  else {
    PRECOND(false);
    vs = VS_UNDEFINED;       // Never reached.
  }

  return vs;
}



std::string geo::valueScaleToStr(const CSF_VS& vs)
{
  std::string str;

  switch(vs) {

    case VS_BOOLEAN:
      str = "BOOLEAN";
      break;

    case VS_NOMINAL:
      str = "NOMINAL";
      break;

    case VS_ORDINAL:
      str = "ORDINAL";
      break;

    case VS_SCALAR:
      str = "SCALAR";
      break;

    case VS_DIRECTION:
      str = "DIRECTIONAL";
      break;

    case VS_LDD:
      str = "LDD";
      break;

    default:
      PRECOND(false);
      str = "UNKOWN";        // Never reached.
      break;

  }

  return str;
}



// std::string geo::dataTypeToStr(const DataType& dataType)
// {
//   std::string str;
//
//   switch(dataType) {
//
//     case STACK:
//       str = "STACK";
//       break;
//
//     case BLOCK:
//       str = "BLOCK";
//       break;
//
//     case TIMESERIES:
//       str = "TIMESERIES";
//       break;
//
//     case MODELSCRIPT:
//       str = "MODELSCRIPT";
//       break;
//
//     case POINTS:
//       str = "POINTS";
//       break;
//
//     case DT_INVALID:
//       str = "INVALID";
//       break;
//
//     default:
//       PRECOND(false);
//       str = "UNKOWN";        // Never reached.
//       break;
//
//   }
//
//   return str;
// }



CSF_CR geo::ValueScale2CellRepr::defaultCR() const
{
    switch(d_vs) {
     case VS_SCALAR:
     case VS_DIRECTION:
        return CR_REAL4; break;
     case VS_NOMINAL:
     case VS_ORDINAL:
        return CR_INT4; break;
    case VS_BOOLEAN:
    case VS_LDD:
        return CR_UINT1; break;
    default:
        PRECOND(FALSE);
        return CR_UNDEFINED; break;          // Never reached.
   }
}

CSF_CR geo::ValueScale2CellRepr::smallCR() const
{
   switch(d_vs) {
     case VS_NOMINAL:
     case VS_ORDINAL:
       return CR_UINT1; break;
     default:
       return defaultCR();
  }
}

CSF_CR geo::ValueScale2CellRepr::largeCR() const
{
  return defaultCR();
}



/*
template<class T>
void geo::nonMVRowSection(const geo::Raster<T>& raster,
                   size_t r, size_t c, size_t& start, size_t& n)
{
  start = c;
  n = 0;

  while(start < raster.nrCols() && geo::isMV(raster.cell(r, start)))
    ++start;

  if(start < raster.nrCols()) {
    c = start + 1;
    while(c < raster.nrCols() && !geo::isMV(raster.cell(r, c))) {
      ++c;
    }
    n = c - start;
  }
}
*/



template<class T>
T geo::average(const Raster<T>& raster, size_t r, size_t c, size_t l)
{
  T mv;
  pcr::setMV(mv);

  T a = 0;
  size_t n = 0;

  for(size_t i = 0; i < l; ++i) {
    for(size_t j = 0; j < l; ++j) {
      if(!pcr::isMV(raster.cell(r + i, c + j))) {
        a += raster.cell(r + i, c + j);
        ++n;
      }
    }
  }

  return n == 0 ? mv : a / n;
}



template<class T>
T geo::average(T const* cells, geo::RasterDim const& dim,
         size_t row, size_t col, size_t length)
{
  T mv;
  pcr::setMV(mv);

  T average(0);
  size_t n = 0;
  size_t index;

  for(size_t i = 0; i < length; ++i) {
    for(size_t j = 0; j < length; ++j) {
      index = dim.convert(row + i, col + j);
      if(!pcr::isMV(cells[index])) {
        average += cells[index];
        ++n;
      }
    }
  }

  return n == 0 ? mv : average / n;
}



//! Assigns values from \a raster to \a boundaries.
/*!
  \param     raster Raster to copy values from.
  \param     boundaries Object to assign values to.
  \warning   This function assumes that type T is a floating point type.

  Values from \a raster are converted to boundary values by taking the
  average value of the cell values at either side of the boundary.
*/
template<class T>
void geo::raster2Boundaries(const SimpleRaster<T>& raster,
         RasterBoundaries<T>& boundaries)
{
  PRECOND(raster.nrRows() == boundaries.nrRows());
  PRECOND(raster.nrCols() == boundaries.nrCols());

  // Strategy:
  // 1. Calculate average values for all right cell boundaries. Vertical
  //    boundaries missed during this run: the left boundaries of the first
  //    column.
  // 2. Calculate average values for all bottom cell boundaries. Horizontal
  //    boundaries missed during this run: the top boundaries of the first
  //    row.
  // 3. Calculate values for all missed boundaries.
  // 4. Handle pecularities of this strategy.

  // Raster for intermediate results.
  SimpleRaster<double> dummy(raster.nrRows(), raster.nrCols());

  // Engine to manage the filter.
  std::unique_ptr<FilterEngine<T, double> > engine;

  // Filter weights.
  SimpleRaster<double> weights(3, 3, 0.0);

  // Calculate average values for right boundaries.
  // Right border.
  // 0 0 0
  // 0 1 1
  // 0 0 0
  weights.cell(1, 1) = 1.0;
  weights.cell(1, 2) = 1.0;
  AverageFilter filter(weights);
  engine.reset(new FilterEngine<T, double>(raster, filter, dummy));
  engine->calc();

  // Extract values from raster.
  for(size_t row = 0; row < raster.nrRows(); ++row) {
    for(size_t col = 0; col < raster.nrCols(); ++col) {

      // Set right boundary value.
      boundaries.rightBoundary(row, col) = dummy.cell(row, col);
    }
  }

  // Calculate average values for bottom boundaries.
  // Bottom border.
  // 0 0 0
  // 0 1 0
  // 0 1 0
  weights.cell(1, 2) = 0.0;
  weights.cell(2, 1) = 1.0;
  filter = AverageFilter(weights);
  engine.reset(new FilterEngine<T, double>(raster, filter, dummy));
  engine->calc();

  // Extract values from raster.
  for(size_t row = 0; row < raster.nrRows(); ++row) {
    for(size_t col = 0; col < raster.nrCols(); ++col) {

      // Set bottom boundary value.
      boundaries.bottomBoundary(row, col) = dummy.cell(row, col);
    }
  }

  // Calculate values for cells missed.
  // Cell values for left boundary of first column equals the raster values
  // of the first column.
  for(size_t row = 0; row < raster.nrRows(); ++row) {

    // Set left boundary value.
    boundaries.leftBoundary(row, 0) = raster.cell(row, 0);
  }

  // Cell values for top boundary of first row equals the raster values
  // of the first row.
  for(size_t col = 0; col < raster.nrCols(); ++col) {

    // Set top boundary value.
    boundaries.topBoundary(0, col) = raster.cell(0, col);
  }

  // A non-border cell with a missing value doesn't get an average value
  // assigned in the above procedure while there is one: the value of the
  // neighbour.
  // Eg:
  //   V1 | MV | V2
  // The right value of the middle cell equals the value of V2.
  // So, find all MV and check whether they have a neighbour on their right or
  // bottom.
  if(raster.nrRows() > 2) {
    for(size_t row = 0; row < raster.nrRows() - 1; ++row) {
      for(size_t col = 0; col < raster.nrCols(); ++col) {
        if(pcr::isMV(raster.cell(row, col))) {
          if(!pcr::isMV(raster.cell(row + 1, col))) {
            // Copy value from bottom neighbour.
            boundaries.bottomBoundary(row, col) = raster.cell(row + 1, col);
          }
        }
      }
    }
  }

  if(raster.nrCols() > 2) {
    for(size_t row = 0; row < raster.nrRows(); ++row) {
      for(size_t col = 0; col < raster.nrCols() - 1; ++col) {
        if(pcr::isMV(raster.cell(row, col))) {
          if(!pcr::isMV(raster.cell(row, col + 1))) {
            // Copy value from right neighbour.
            boundaries.rightBoundary(row, col) = raster.cell(row, col + 1);
          }
        }
      }
    }
  }
}



template<class T>
void geo::magnitude(const RasterBoundaries<T>& xVector,
         const RasterBoundaries<T>& yVector, RasterBoundaries<T>& result)
{
  PRECOND(xVector.nrRows() == yVector.nrRows());
  PRECOND(xVector.nrRows() == result.nrRows());
  PRECOND(xVector.nrCols() == yVector.nrCols());
  PRECOND(xVector.nrCols() == result.nrCols());

  size_t nrRows = xVector.nrRows();
  size_t nrCols = xVector.nrCols();

  // Right and bottom boundaries of all cells.
  for(size_t row = 0; row < nrRows; ++row) {
    for(size_t col = 0; col < nrCols; ++col) {

      if(pcr::isMV(xVector.rightBoundary(row, col)) ||
         pcr::isMV(yVector.rightBoundary(row, col))) {
        pcr::setMV(result.rightBoundary(row, col));
      }
      else {
        result.rightBoundary(row, col) =
          tr1::hypot(xVector.rightBoundary(row, col),
                     yVector.rightBoundary(row, col));
      }

      if(pcr::isMV(xVector.bottomBoundary(row, col)) ||
         pcr::isMV(yVector.bottomBoundary(row, col))) {
        pcr::setMV(result.bottomBoundary(row, col));
      }
      else {
        result.bottomBoundary(row, col) =
          tr1::hypot(xVector.bottomBoundary(row, col),
                     yVector.bottomBoundary(row, col));
      }
    }
  }

  // Left boundaries of first column.
  for(size_t row = 0; row < nrRows; ++row) {
    if(pcr::isMV(xVector.leftBoundary(row, 0)) ||
       pcr::isMV(yVector.leftBoundary(row, 0))) {
      pcr::setMV(result.leftBoundary(row, 0));
    }
    else {
      result.leftBoundary(row, 0) =
        tr1::hypot(xVector.leftBoundary(row, 0),
                   yVector.leftBoundary(row, 0));
    }
  }

  // Top boundaries of first row.
  for(size_t col = 0; col < nrCols; ++col) {
    if(pcr::isMV(xVector.topBoundary(0, col)) ||
       pcr::isMV(yVector.topBoundary(0, col))) {
      pcr::setMV(result.topBoundary(0, col));
    }
    else {
      result.topBoundary(0, col) =
        tr1::hypot(xVector.topBoundary(0, col),
                   yVector.topBoundary(0, col));
    }
  }
}



// Instantiate the template functions using explicit instantiation declarations.
namespace geo {
template float average<float>(const Raster<float>&, size_t, size_t, size_t);
template double average<double>(const Raster<double>&, size_t, size_t, size_t);
template float average<float>(float const* cells, RasterDim const& dim, size_t, size_t, size_t);
template double average<double>(double const* cells, RasterDim const& dim, size_t, size_t, size_t);
template void raster2Boundaries<double>(const SimpleRaster<double>&,
         RasterBoundaries<double>&);
template void magnitude<double>(const RasterBoundaries<double>&,
         const RasterBoundaries<double>&, RasterBoundaries<double>&);
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


