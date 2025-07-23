#include "stddefx.h"
#include "dal_BinaryTableDriver.h"
#include "discr_rasterdata.h"
#include "func_assign.h"



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

