#ifndef INCLUDED_DAL_CSFMAP
#include "dal_CSFMap.h"
#define INCLUDED_DAL_CSFMAP
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM_CONVENIENCE
#include <boost/filesystem/convenience.hpp>
#define INCLUDED_BOOST_FILESYSTEM_CONVENIENCE
#endif

#ifndef INCLUDED_BOOST_SCOPED_ARRAY
#include <boost/scoped_array.hpp>
#define INCLUDED_BOOST_SCOPED_ARRAY
#endif

#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
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
  This file contains the implementation of the CSFMap class.
*/



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CSFMAP MEMBERS
//------------------------------------------------------------------------------

namespace dal {

//! Translates TypeId values to CSF_CR equivalents.
/*!
  \param     typeId Type id.
  \return    CSF cell representation.
  \warning   There must exist a CSF_CR equivalent for \a typeId.
*/
static CSF_CR typeId2CellRepresentation(
         TypeId typeId)
{
  CSF_CR cellRepresentation = CR_UNDEFINED;

  switch(typeId) {
    case TI_UINT1: cellRepresentation = CR_UINT1; break;
    case TI_INT4 : cellRepresentation = CR_INT4;  break;
    case TI_REAL4: cellRepresentation = CR_REAL4; break;
    case TI_INT1 : cellRepresentation = CR_INT1;  break;
    case TI_INT2 : cellRepresentation = CR_INT2;  break;
    case TI_UINT2: cellRepresentation = CR_UINT2; break;
    case TI_UINT4: cellRepresentation = CR_UINT4; break;
    case TI_REAL8: cellRepresentation = CR_REAL8; break;
    default: break;
  }

  assert(cellRepresentation != CR_UNDEFINED);

  return cellRepresentation;
}

// std::string const CSFMap::d_defaultExtension = ".pcrmap";

// bool CSFMap::exists(
//          std::string const& name,
//          DataSpace const& space,
//          DataSpaceAddress const& address)
// {
//   boost::filesystem::path path;
// 
//   try {
//     path = pathForDataSpaceAddress(name, space, address);
//   }
//   catch(Exception const&) {
//     return false;
//   }
// 
//   if(dal::exists(path)) {
//     return true;
//   }
//   else if(boost::filesystem::extension(path).empty()) {
//     path = boost::filesystem::change_extension(path, d_defaultExtension);
//     return dal::exists(path);
//   }
//   else {
//     return false;
//   }
// }

template<typename T> static void setExtremesImpl(
         MAP        *map,
         boost::any min,
         boost::any max) {
    T minSet = boost::any_cast<T>(min);
    T maxSet = boost::any_cast<T>(max);
    RputMinVal(map, &minSet);
    RputMaxVal(map, &maxSet);
}

template<typename T> static boost::any getExtremeImpl(
         const MAP *map,
         int       (*get)(const MAP *m, void *v))
{
  boost::any max;
  T        value;
  if(get(map, &value))
        max = value;
  return max;
}

}



//------------------------------------------------------------------------------
// DEFINITION OF CSFMAP MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     path Path to CSF raster file.
  \param     allowUpdate Whether or not the raster should be opened in r/w mode.
*/
dal::CSFMap::CSFMap(
         boost::filesystem::path const& path,
         bool allowUpdate)

  : d_path(path), d_map(0)

{
  open(allowUpdate);
}



//! Constructor to create new CSF raster file.
/*!
  \param     path Path to CSF raster file.
  \param     nrRows Number of rows.
  \param     nrCols Number of cols.
  \param     west Western-most x-coordinate.
  \param     north Northern-most y-coordinate.
  \param     angle Angle.
  \param     cellSize Cellsize.
  \param     typeId Type id of values.
  \param     valueScale Value scale of values.

  \todo
   The default extension is NOT added to \a path if it not already has an
   extension. Should do it once, but not now
*/
dal::CSFMap::CSFMap(
         boost::filesystem::path const& path,
         size_t nrRows,
         size_t nrCols,
         double west,
         double north,
         double angle,
         double cellSize,
         TypeId typeId,
         CSF_VS valueScale,
         CSF_PT projectionType)

  : d_path(path), d_map(0)

{
  // TODO
  // if(boost::filesystem::extension(d_path).empty()) {
  //   d_path = boost::filesystem::change_extension(d_path, d_defaultExtension);
  // }

  create(nrRows, nrCols, west, north, angle, cellSize, typeId, valueScale,
         projectionType);
}



/* NOT IMPLEMENTED
//! Copy constructor.
dal::CSFMap::CSFMap(CSFMap const& rhs)

  : Base(rhs)

{
}
*/



//! Destructor.
/*!
  Closes the file.
*/
dal::CSFMap::~CSFMap()
{
  close();
}



/* NOT IMPLEMENTED
//! Assignment operator.
dal::CSFMap& dal::CSFMap::operator=(CSFMap const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



//! Opens a CSF raster file.
/*!
  \param     allowUpdate Whether or not the raster should be opened in r/w mode.
  \exception Exception If file is not a valid CSF-2.0 file or cannot be opened.
*/
void dal::CSFMap::open(
         bool allowUpdate)
{
  assert(!d_map);

  MOPEN_PERM permission = allowUpdate ? M_READ_WRITE : M_READ;

  d_map = Mopen(d_path.string().c_str(), permission);

  // if(!d_map && boost::filesystem::extension(d_path).empty()) {
  //   // No extension present, add default one.
  //   d_map = Mopen((d_path.string() + d_defaultExtension).c_str(),
  //         permission);

  //   if(d_map) {
  //     // Use new name.
  //     d_path = d_path.string() + d_defaultExtension;
  //   }
  // }

  if(!d_map) {
    throwCannotBeOpened(d_path.string(), RASTER);
  }
}



//! Creates a CSF raster file.
/*!
  \param     nrRows Number of rows.
  \param     nrCols Number of cols.
  \param     west Western-most x-coordinate.
  \param     north Northern-most y-coordinate.
  \param     angle Angle.
  \param     cellSize Cellsize.
  \param     typeId Type id of values.
  \param     valueScale Value scale of values.
  \exception Exception If the raster file cannot be created.
*/
void dal::CSFMap::create(
         size_t nrRows,
         size_t nrCols,
         double west,
         double north,
         double angle,
         double cellSize,
         TypeId typeId,
         CSF_VS valueScale,
         CSF_PT projectionType)
{
  assert(!d_map);

  d_map = Rcreate(d_path.string().c_str(), nrRows, nrCols,
         typeId2CellRepresentation(typeId), valueScale,
         projectionType,
         west, north, angle, cellSize);

  if(!d_map) {
    throwCannotBeCreated(d_path.string(), RASTER);
  }
}



//! Closes the raster file.
/*!
  \exception Exception If the raster file cannot be closed.
*/
void dal::CSFMap::close()
{
  assert(d_map);

  if(Mclose(d_map)) {
    throwCannotBeClosed(d_path.string(), RASTER);
  }

  d_map = 0;
}



//! Sets the in-app cell representation to \a typeId.
/*!
  \param     typeId New type id.
*/
void dal::CSFMap::useAs(
         TypeId typeId)
{
  RuseAs(d_map, typeId2CellRepresentation(typeId));
}



//! Returns the number of rows.
/*!
  \return    Number of rows.
*/
size_t dal::CSFMap::nrRows() const
{
  return RgetNrRows(d_map);
}



//! Returns the number of cols.
/*!
  \return    Number of cols.
*/
size_t dal::CSFMap::nrCols() const
{
  return RgetNrCols(d_map);
}



//! Returns the number of cells.
/*!
  \return    Number of cells.
*/
size_t dal::CSFMap::nrCells() const
{
  return nrRows() * nrCols();
}



//! Returns the cell size.
/*!
  \return    Cell size.
*/
double dal::CSFMap::cellSize() const
{
  return RgetCellSize(d_map);
}



//! Returns the western-most x-coordinate.
/*!
  \return    Western-most x-coordinate.
*/
double dal::CSFMap::west() const
{
  return RgetXUL(d_map);
}



//! Returns the northern-most y-coordinate.
/*!
  \return    Northern-most y-coordinate.
*/
double dal::CSFMap::north() const
{
  return RgetYUL(d_map);
}



//! Returns the angle.
/*!
  \return    Angle.
*/
double dal::CSFMap::angle() const
{
  return RgetAngle(d_map);
}



/*
dal::Projection dal::CSFMap::projection() const
{
  CSF_PT projection = MgetProjection(d_map);
  assert(projection == PT_YINCT2B || projection == PT_YDECT2B);

  return projection == PT_YINCT2B ? YINCRN2S : YINCRS2N;
}
*/



//! Returns the value scale.
/*!
  \return    Value scale.
*/
CSF_VS dal::CSFMap::valueScale() const
{
  return RgetValueScale(d_map);
}



CSF_PT dal::CSFMap::projectionType() const
{
  return MgetProjection(d_map);
}


//! Returns the in-file type id of the values.
/*!
  \return    Type id.
*/
dal::TypeId dal::CSFMap::fileTypeId() const
{
  TypeId typeId = TI_NR_TYPES;

  switch(RgetCellRepr(d_map)) {
    case CR_UINT1: typeId = dal::TI_UINT1; break;
    case CR_INT4 : typeId = dal::TI_INT4;  break;
    case CR_REAL4: typeId = dal::TI_REAL4; break;
    case CR_INT1 : typeId = dal::TI_INT1;  break;
    case CR_INT2 : typeId = dal::TI_INT2;  break;
    case CR_UINT2: typeId = dal::TI_UINT2; break;
    case CR_UINT4: typeId = dal::TI_UINT4; break;
    case CR_REAL8: typeId = dal::TI_REAL8; break;
    default: break;
  }

  assert(typeId != TI_NR_TYPES);

  return typeId;
}



dal::TypeId dal::CSFMap::useTypeId() const
{
  TypeId typeId = TI_NR_TYPES;

  switch(RgetUseCellRepr(d_map)) {
    case CR_UINT1: typeId = dal::TI_UINT1; break;
    case CR_INT4 : typeId = dal::TI_INT4;  break;
    case CR_REAL4: typeId = dal::TI_REAL4; break;
    case CR_INT1 : typeId = dal::TI_INT1;  break;
    case CR_INT2 : typeId = dal::TI_INT2;  break;
    case CR_UINT2: typeId = dal::TI_UINT2; break;
    case CR_UINT4: typeId = dal::TI_UINT4; break;
    case CR_REAL8: typeId = dal::TI_REAL8; break;
    default: break;
  }

  assert(typeId != TI_NR_TYPES);

  return typeId;
}



//! Reads cells into \a buffer.
/*!
  \param     offset Offset to use.
  \param     nrCells Number of cells to read.
  \param     buffer Buffer to write values in.
  \exception Exception If reading the values fails.
  \sa        getCell(size_t, size_t, void*)
*/
void dal::CSFMap::getCells(size_t offset, size_t nrCells, void* buffer) const
{
  if(RgetSomeCells(d_map, offset, nrCells, buffer) != nrCells) {
    throwCannotReadCells(d_path.string(), RASTER);
  }
}



//! Reads a cell into \a value.
/*!
  \param     row Row index of cell.
  \param     col Col index of cell.
  \param     value Buffer to write value in.
  \exception Exception If reading the value fails.
  \sa        getCells(size_t, size_t, void*)
*/
void dal::CSFMap::getCell(size_t row, size_t col, void* value) const
{
  if(RgetCell(d_map, row, col, value) != 1) {
    throwCannotReadCell(d_path.string(), RASTER);
      // hack CW see CSFMapTest::testError std::string(" specifics: ")+MstrError());
  }
}



//! Writes cells from \a buffer into the raster file.
/*!
  \param     buffer Buffer to read values from.
  \exception Exception If writing the values fails.
  \sa        putCells(size_t, size_t, void const*)
*/
void dal::CSFMap::putCells(void const* buffer)
{
  assert(RputDoNotChangeValues(d_map));

  if(RputSomeCells(d_map, 0, nrCells(),
         const_cast<void*>(buffer)) != nrCells()) {
    throwCannotWriteCells(d_path.string(), RASTER);
  }
}



//! Writes cells from \a buffer into the raster file.
/*!
  \param     offset Offset to use.
  \param     nrCells Number of cells to write.
  \param     buffer Buffer to read values from.
  \exception Exception If writing the values fails.
  \sa        putCells(size_t, size_t, void const*)
*/
void dal::CSFMap::putCells(size_t offset, size_t nrCells, void const* buffer)
{
  assert(RputDoNotChangeValues(d_map));

  if(RputSomeCells(d_map, offset, nrCells,
         const_cast<void*>(buffer)) != nrCells) {
    throwCannotWriteCells(d_path.string(), RASTER);
  }
}



//! Returns whether \a value contains a missing value.
/*!
  \param     value Pointer to buffer with the value.
  \return    true or false
*/
bool dal::CSFMap::isMV(void const* value) const
{
  return IsMV(d_map, value) != 0;
}



boost::any dal::CSFMap::max() const
{
  boost::any result;

  switch(useTypeId()) {
    case TI_UINT1: {
      result = getExtremeImpl<UINT1>(d_map, RgetMaxVal);
      break;
    }
    case TI_INT4: {
      result = getExtremeImpl<INT4>(d_map, RgetMaxVal);
      break;
    }
    case TI_REAL4: {
      result = getExtremeImpl<REAL4>(d_map, RgetMaxVal);
      break;
    }
    case TI_REAL8: {
      result = getExtremeImpl<REAL8>(d_map, RgetMaxVal);
      break;
    }
    default: assert(false);
  }

  return result;
}



boost::any dal::CSFMap::min() const
{
  boost::any result;

  switch(useTypeId()) {
    case TI_UINT1: {
      result = getExtremeImpl<UINT1>(d_map, RgetMinVal);
      break;
    }
    case TI_INT4: {
      result = getExtremeImpl<INT4>(d_map, RgetMinVal);
      break;
    }
    case TI_REAL4: {
      result = getExtremeImpl<REAL4>(d_map, RgetMinVal);
      break;
    }
    case TI_REAL8: {
      result = getExtremeImpl<REAL8>(d_map, RgetMinVal);
      break;
    }
    default: assert(false);
  }

  return result;
}



//! wrapper around Rmalloc
/*!
 * \returns value that must be deleted with free()
 * \exception std::bad_alloc in case of failure.
 */
void*   dal::CSFMap::malloc(size_t nrCells) const
{
  void *m = Rmalloc(d_map,nrCells);
  if (!m)
    throw std::bad_alloc();
  return m;
}

/*!
 * set extremes to \a min and \a max and disable the CSF
 * MM_KEEPTRACK mode
 */
void dal::CSFMap::setExtremes(
         boost::any min,
         boost::any max)
{
  switch(useTypeId()) {
    case TI_UINT1:
      return setExtremesImpl<UINT1>(d_map,min,max);
    case TI_INT4:
      return setExtremesImpl<INT4>(d_map,min,max);
    case TI_REAL4:
      return setExtremesImpl<REAL4>(d_map,min,max);
    case TI_REAL8:
      return setExtremesImpl<REAL8>(d_map,min,max);
    default: assert(false);
  }
}



namespace dal {

bool CSFMap::hasLegend() const
{
  return MattributeAvail(d_map, ATTR_ID_LEGEND_V1) ||
         MattributeAvail(d_map, ATTR_ID_LEGEND_V2);
}



//! Returns the number of legend entries plus 1 for the name of the legend.
/*!
  \return    Number of legend entries plus 1, or 0 if there's no legend.
  \warning   The number of legend entries does not correspond with the
             number of classes in the raster. It is possible to have
             three entries and 6 classes, for example.
*/
size_t CSFMap::nrLegendEntries() const
{
  return MgetNrLegendEntries(d_map);
}



Table CSFMap::legend() const
{
  Table legend;

  if(hasLegend()) {
    size_t nrEntries = nrLegendEntries();
    assert(nrEntries > 0);
    boost::scoped_array<CSF_LEGEND> csfLegend(new CSF_LEGEND[nrEntries]);

    if(MgetLegend(d_map, csfLegend.get()) == 0) {
      throwCannotReadLegend(d_path.string());
    }

    std::vector<TypeId> typeIds;
    typeIds.push_back(TI_INT4);
    typeIds.push_back(TI_STRING);
    legend.init(typeIds);
    legend.createCols();
    legend.resize(nrEntries - 1);
    legend.setTitle(csfLegend[0].descr);

    for(size_t i = 1; i < nrEntries; ++i) {
      legend.col<INT4>(0)[i - 1] = static_cast<INT4>(csfLegend[i].nr);
      legend.col<std::string>(1)[i - 1] = csfLegend[i].descr;
    }
  }

  return legend;
}

} // namespace dal



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



