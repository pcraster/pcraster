#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DAL_BINARYTABLEDRIVER
#include "dal_BinaryTableDriver.h"
#define INCLUDED_DAL_BINARYTABLEDRIVER
#endif

#ifndef INCLUDED_DISCR_RASTERDATA
#include "discr_rasterdata.h"
#define INCLUDED_DISCR_RASTERDATA
#endif

#ifndef INCLUDED_FUNC_ASSIGN
#include "func_assign.h"
#define INCLUDED_FUNC_ASSIGN
#endif

// Module headers.



namespace raster {

template<typename T>
void writeBinary(
         discr::RasterData<T> const& data,
         std::string const& name)
{
  dal::Table table;
  dal::Array<T>& array(table.appendCol<T>());
  array.resize(data.raster()->nrCells());
  func::assign(data.cells(), array.elements(), data.raster()->nrCells());

  dal::BinaryTableDriver driver;
  static_cast<dal::TableDriver&>(driver).write(table, name);
}

template
void writeBinary(
         discr::RasterData<UINT1> const&,
         std::string const&);
template
void writeBinary(
         discr::RasterData<INT4> const&,
         std::string const&);
template
void writeBinary(
         discr::RasterData<REAL4> const&,
         std::string const&);

} // namespace raster

