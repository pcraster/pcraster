#include <pybind11/pybind11.h>

#include "stddefx.h"
#include "com_exception.h"
#include "com_tune.h"
#include "dal_Exception.h"
#include "dal_Raster.h"
#include "dal_RasterDal.h"
#include "dal_RasterDriver.h"
#include "calc_datastorageid.h"
#include "calc_exception.h"
#include "calc_findsymbol.h"
#include "calc_globallibdefs.h"
#include "calc_map2csf.h"
#include "calc_nonspatial.h"
#include "calc_posexception.h"
#include "calc_runtimeengine.h"
#include "calc_spatial.h"
#include "Globals.h"
#include "numpy_conversion.h"
#include "pickle.h"
#include "ppu_exception.h"
#include "pcraster_version.h"

#ifndef INCLUDED_BOOST_VERSION
#include <boost/version.hpp>
#define INCLUDED_BOOST_VERSION
#endif

#if BOOST_VERSION > 105800
#include <boost/test/tools/floating_point_comparison.hpp>
#else
#include <boost/test/floating_point_comparison.hpp>
#endif

#include <boost/format.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/math/special_functions/fpclassify.hpp>
#include <boost/math/special_functions/sign.hpp>

#include <limits>

template<
    typename T>
struct ValueTraits
{
    inline static bool is_nan(
        T value)
    {
        return (boost::math::isnan)(value);
    }

    inline static bool nan_is_negative(
        T value)
    {
        return boost::math::signbit(value) != 0;
    }

    inline static bool nan_is_quiet(
        T /* value */)
    {
        // TODO Howto test this, portable.
        return true;
    }

    inline static bool is_negative_quiet_nan(
        T value)
    {
        return is_nan(value) && nan_is_negative(value) && nan_is_quiet(value);
    }
};


static void mem_init_hook(void);
// static void *mem_malloc_hook(size_t, const void *);
// static void *(*glibc_malloc)(size_t, const void *);
void (*__malloc_initialize_hook)(void) = mem_init_hook;

static void mem_init_hook(void)
{
  com::tune();
}



namespace calc {
 // opaque ptr's only, no .h needed
  class ObjectLink {};
  class Operator   {};
}



namespace pcraster {
namespace python {


//! Check an input pointer to be non 0
/*!
 *  None objects are passed as 0
 */
void checkNotNullPointer(void const* ptr)
{
  if (! ptr) {
    throw com::Exception("None value not allowed");
  }
}


//! Reads a field from a file.
calc::Field* readField(
         std::string const& name)
{
  // Open the raster.
  boost::shared_ptr<dal::Raster> raster;
  dal::RasterDriver* driver;
  boost::tie(raster, driver) = globals.rasterDal().open(name);

  if(!raster) {
    throw com::Exception((boost::format("Raster %1%: can not be opened")
         % name).str());
  }

  // Determine value scale of data, and type to use for storing values.
  CSF_VS valueScale = VS_NOTDETERMINED;

  // Support non-PCRaster rasters too.
  if(raster->properties().hasValue(DAL_CSF_VALUESCALE)) {
    valueScale = raster->properties().value<CSF_VS>(DAL_CSF_VALUESCALE);
  }
  else {
    valueScale = dal::typeIdToValueScale(raster->typeId());
  }

  // Configure raster before reading.
  // Always read as UINT1, INT4 or REAL4.
  dal::TypeId useTypeId = dal::TI_NR_TYPES;

  switch(valueScale) {
    case VS_BOOLEAN:
    case VS_LDD: {
      useTypeId = dal::TI_UINT1;
      break;
    }
    case VS_CLASSIFIED:
      // CSF version 1
      valueScale = VS_NOMINAL;
      // fall through
    case VS_NOMINAL:
    case VS_ORDINAL: {
      useTypeId = dal::TI_INT4;
      break;
    }
    case VS_CONTINUOUS:
      // CSF version 1
      valueScale = VS_SCALAR;
      // fall through
    case VS_SCALAR:
    case VS_DIRECTION: {
      useTypeId = dal::TI_REAL4;
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  raster->setTypeId(useTypeId);

  assert(driver);
  driver->read(*raster, name);

  calc::Spatial* spatial = 0;

  switch(raster->typeId()) {
    case dal::TI_UINT1: {
      spatial = new calc::Spatial(calc::csfVs2vs(valueScale),
         raster->cells<UINT1>(), raster->nrCells());
      break;
    }
    case dal::TI_INT4: {
      spatial = new calc::Spatial(calc::csfVs2vs(valueScale),
         raster->cells<INT4>(), raster->nrCells());
      break;
    }
    case dal::TI_REAL4: {
      spatial = new calc::Spatial(calc::csfVs2vs(valueScale),
         raster->cells<REAL4>(), raster->nrCells());
      break;
    }
    default: {
      PRECOND(false);
      break;
    }
  }

  POSTCOND(spatial);

  if(!globals.cloneSpace().valid()) {
    // \todo angle
    geo::Projection projection = geo::IllegalProjection;
    if(raster->properties().hasValue(DAL_CSF_PROJECTION)){
      if(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION) == 0){
        projection = geo::YIncrT2B;
      }
      else{
        projection = geo::YIncrB2T;
      }
    }

    geo::RasterSpace cloneSpace(raster->nrRows(), raster->nrCols(),
         raster->cellSize(), raster->west(), raster->north(), projection);
    globals.setCloneSpace(cloneSpace);
  }

  POSTCOND(globals.cloneSpace().valid());

  return spatial;
}


// row >= 1, <= nrRows, col >= 1, <= nrCols
pybind11::object readFieldCell(
         std::string const& filename,
         int row,
         int col)
{
  boost::shared_ptr<dal::Raster> raster;
  dal::RasterDriver* driver;
  boost::tie(raster, driver) = globals.rasterDal().open(filename);

  if(!raster) {
    throw com::Exception((boost::format("Raster %1%: can not be opened")
         % filename).str());
  }
  POSTCOND(raster);
  assert(driver);

  dal::RasterDimensions rasterDim(raster->nrRows(), raster->nrCols(), raster->cellSize(), raster->west(), raster->north());
  dal::Dimension dim(dal::Space, dal::RegularDiscretisation, rasterDim);
  dal::DataSpace space(dim);

  double x = 0.0;
  double y = 0.0;
  row -= 1;
  col -= 1;

  rasterDim.coordinates(static_cast<double>(row), static_cast<double>(col), x, y);
  dal::DataSpaceAddress address(space.address());
  address.setCoordinate(0, dal::SpatialCoordinate(x, y));

  pybind11::object tuple;

  switch(raster->typeId()) {
    case dal::TI_UINT1: {
      UINT1 v;
      driver->read(&v, dal::TI_UINT1, filename, space, address);
      tuple = pybind11::make_tuple(static_cast<float>(v), !static_cast<int>(pcr::isMV(v)));
      break;
    }
    case dal::TI_INT4: {
      INT4 v;
      driver->read(&v, dal::TI_INT4, filename, space, address);
      tuple = pybind11::make_tuple(static_cast<float>(v), !static_cast<int>(pcr::isMV(v)));
      break;
    }
    case dal::TI_REAL4: {
      REAL4 v;
      driver->read(&v, dal::TI_REAL4, filename, space, address);
      tuple = pybind11::make_tuple(static_cast<float>(v), !static_cast<int>(pcr::isMV(v)));
      break;
    }
    default: {
      PRECOND(false);
      break;
    }
  }

  return tuple;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  This function realy should be called writeField, but then boost.Python cannot
  choose between de non-template version (which it should select) and this
  template version.
*/
template<typename T>
void writeFieldTemplate(
         calc::Field* field,
         std::string const& filename)
{
  DEVELOP_PRECOND(globals.rasterDal().hasDriverByName("CSF"));
  dal::RasterDriver* driver(globals.rasterDal().driverByName("CSF"));

  // PRECOND(field->isSpatial());

  dal::Raster raster(
         globals.cloneSpace().nrRows(), globals.cloneSpace().nrCols(),
         globals.cloneSpace().cellSize(),
         globals.cloneSpace().west(), globals.cloneSpace().north(),
         dal::TypeTraits<T>::typeId);
  raster.properties().setValue<REAL8>(DAL_CSF_ANGLE,
         globals.cloneSpace().angle());
  raster.properties().setValue<CSF_VS>(DAL_CSF_VALUESCALE,
         calc::vs2CsfVs(field->vs()));
  raster.properties().setValue<CSF_PT>(DAL_CSF_PROJECTION,
         globals.cloneSpace().projection() == geo::YIncrT2B
             ? PT_YINCT2B : PT_YDECT2B);

  if(field->isSpatial()) {
    // Borrow the data from the field, cells are for use only.
    raster.transfer<T>(const_cast<T*>(field->src_t<T>()),
         dal::Raster::DoNotTakeOwnerShip);
  }
  else {
    raster.createCells();

    if(field->isMV()) {
      raster.setAllMV();
    }
    else {
      raster.fill<T>(field->src_t<T>()[0]);
    }
  }

  driver->write(raster, filename);
}



void writeField(
         calc::Field* field,
         std::string const& filename)
{
  checkNotNullPointer(field);

  switch(field->cr()) {
    case CR_UINT1: {
      writeFieldTemplate<UINT1>(field, filename);
      break;
    }
    case CR_INT4: {
      writeFieldTemplate<INT4>(field, filename);
      break;
    }
    case CR_REAL4: {
      writeFieldTemplate<REAL4>(field, filename);
      break;
    }
    default: {
      PRECOND(false);
      break;
    }
  }
}



void writeFilename(
         std::string const& inputFilename,
         std::string const& outputFilename)
{
  writeField(readField(inputFilename), outputFilename);
}



calc::Field* newScalarField()
{
  calc::Spatial* field = new calc::Spatial(
         VS_S, calc::CRI_f, globals.cloneSpace().nrCells());
  REAL4* cells = static_cast<REAL4*>(field->dest());

  for(size_t i = 0; i < field->nrValues(); ++i) {
    pcr::setMV(cells[i]);
  }

  return field;
}



calc::Field* newNonSpatialFloatField(
         double value)
{
  return new calc::NonSpatial(VS_SD, value);
}



calc::Field* newNonSpatialIntegralField(
         long int value)
{
  // nooit never ever int->fouble->int doen,
  // kan met
  //   template<typename CR>
  //     NonSpatial(VS vs, const CR& value);
  //     CW=UINT1,INT4 of REAL4
  //     maar dan moet je de VS subset wel weten.
  // Als deze functie alleen gebruikt in operations.py
  //  dan heeft de operator al context type informatie voor de VS

  // ok, manual heeft een workaround, maar je ziet hoe subtiel de bug is.
  // CW welke manual waar?

  // ERROR: value in is different from value out.
  calc::Field* result = new calc::NonSpatial(vsOfNumber(value),
         static_cast<double>(value));

  // double newValue;
  // bool isValid = result->getCell(newValue, 0);

  // std::cout << INT4(newValue) << '\t' << isValid << std::endl;

  return result;
}



pybind11::tuple fieldGetCellIndex(
         calc::Field const* field,
         size_t index)
{
 checkNotNullPointer(field);

 if(field->isSpatial()){
    if((index < 1) || (index > globals.cloneSpace().nrCells() )){
      throw com::Exception(
      (boost::format("cellvalue index %1% out of range [1,%2%]") %
      index % globals.cloneSpace().nrCells()).str());
    }
  }
  --index;

  pybind11::tuple tuple;
  double value = 0;
  bool isValid = field->getCell(value, static_cast<size_t>(index));

  switch(field->vs()) {
    case VS_B: {
      tuple = pybind11::make_tuple(
         static_cast<bool>(value), isValid);
      break;
    }
    case VS_L:
    case VS_N:
    case VS_O: {
      tuple = pybind11::make_tuple(
         static_cast<int>(value), isValid);
      break;
    }
    case VS_S:
    case VS_D: {
      // std::cout << (double(0.4) == double(value)) << std::endl;
      // std::cout << (float(0.4) == float(value)) << std::endl;
      // --> std::cout << (double(float(0.4)) == value) << std::endl;
      // TODO pak een selectie van bytes om de float te maken?
      tuple = pybind11::make_tuple(
         static_cast<float>(value), isValid);
      break;
    }
    default: {
      PRECOND(false);
      break;
    }
  }

  return tuple;
}



pybind11::tuple fieldGetCellRowCol(
         calc::Field const* field,
         size_t row,
         size_t col)
{
  checkNotNullPointer(field);
  if(field->isSpatial()){
    if((row < 1) || (row > globals.cloneSpace().nrRows() )){
      throw com::Exception(
      (boost::format("cellvalue row index %1% out of range [1,%2%]") %
      row% globals.cloneSpace().nrRows()).str());
    }
    if((col < 1) || (col > globals.cloneSpace().nrCols() )){
      throw com::Exception(
      (boost::format("cellvalue column index %1% out of range [1,%2%]")
      % col % globals.cloneSpace().nrCols()).str());
    }
  }
  --row;
  --col;

  return fieldGetCellIndex(
         field, row * globals.cloneSpace().nrCols() + col + 1);
}



pybind11::tuple cellvalue_by_index(
         calc::Field const* field,
         size_t index)
{
  checkNotNullPointer(field);

  if(field->isSpatial()){
    if(index >= globals.cloneSpace().nrCells()){
      std::ostringstream errMsg;
      errMsg << "cellvalue index '"
             << index
             << "' out of range [0, "
             << globals.cloneSpace().nrCells() - 1
             << "]";
      throw std::invalid_argument(errMsg.str());
    }
  }

  pybind11::tuple tuple;
  double value = 0;
  bool isValid = field->getCell(value, static_cast<size_t>(index));

  switch(field->vs()) {
    case VS_B: {
      tuple = pybind11::make_tuple(
         static_cast<bool>(value), isValid);
      break;
    }
    case VS_L:
    case VS_N:
    case VS_O: {
      tuple = pybind11::make_tuple(
         static_cast<int>(value), isValid);
      break;
    }
    case VS_S:
    case VS_D: {
      tuple = pybind11::make_tuple(
         static_cast<float>(value), isValid);
      break;
    }
    default: {
      PRECOND(false);
      break;
    }
  }

  return tuple;
}


pybind11::tuple cellvalue_by_indices(
         calc::Field const* field,
         size_t row,
         size_t col)
{
  checkNotNullPointer(field);
  if(field->isSpatial()){
    if(row >= globals.cloneSpace().nrRows()){
      std::ostringstream errMsg;
      errMsg << "cellvalue row index '"
             << row
             << "' out of range [0, "
             << globals.cloneSpace().nrRows() - 1
             << "]";
      throw std::invalid_argument(errMsg.str());
    }
    if(col >= globals.cloneSpace().nrCols()){
      std::ostringstream errMsg;
      errMsg << "cellvalue column index '"
             << col
             << "' out of range [0, "
             << globals.cloneSpace().nrCols() - 1
             << "]";
      throw std::invalid_argument(errMsg.str());
    }
  }

  return cellvalue_by_index(
         field, row * globals.cloneSpace().nrCols() + col);
}


pybind11::tuple cellvalue_by_coordinates(
         calc::Field const* field,
         double xcoordinate,
         double ycoordinate)
{
  checkNotNullPointer(field);

  if(!field->isSpatial()) {
    throw std::invalid_argument("Not implemented for non-spatial arguments");
  }

  if(globals.cloneSpace().projection() == geo::YIncrT2B) {
    throw std::invalid_argument("Not implemented for projection type 'YIncrT2B'");
  }

  double west = globals.cloneSpace().west();
  double north = globals.cloneSpace().north();
  size_t nr_rows = globals.cloneSpace().nrRows();
  size_t nr_cols = globals.cloneSpace().nrCols();
  double cellsize = globals.cloneSpace().cellSize();
  double east = west + nr_cols * cellsize;
  double south = north - nr_rows * cellsize;

  if((xcoordinate < west) || (xcoordinate > east)){
    std::ostringstream errMsg;
    errMsg << "xcoordinate '"
           << xcoordinate
           << "' out of range ["
           << west
           << ", "
           << east
           << "]";
    throw std::invalid_argument(errMsg.str());
  }

  if((ycoordinate > north) || (ycoordinate < south)){
    std::ostringstream errMsg;
    errMsg << "ycoordinate '"
           << ycoordinate
           << "' out of range ["
           << north
           << ", "
           << south
           << "]";
    throw std::invalid_argument(errMsg.str());
  }

  double xCol = (xcoordinate - west) / cellsize;
  double yRow = (north - ycoordinate) / cellsize;

  size_t row = std::floor(yRow);
  size_t col = std::floor(xCol);

  return cellvalue_by_index(field, row * nr_cols + col);
}


/*
boost::python::tuple nonSpatial2Number(calc::Field const* field)
{
  if(field->isSpatial()) {
    throw com::Exception("Raster: is not a non-spatial");
  }

  return fieldGetCell(field, 0);
}
*/



/*
* tests two floats map for equality
*/
calc::Field* closeAtTolerance(calc::Field const * result,
          calc::Field const * validated)
{
  checkNotNullPointer(result);
  checkNotNullPointer(validated);

  size_t nrCells = globals.cloneSpace().nrCells();

  calc::Spatial* field = new calc::Spatial(VS_B, calc::CRI_1, nrCells);
  UINT1* cells = static_cast<UINT1*>(field->dest());

  for(size_t i = 0; i < nrCells; ++i) {
    cells[i] = 0;
  }

#if BOOST_VERSION > 105800
  boost::math::fpc::close_at_tolerance<REAL4> tester(
         boost::math::fpc::fpc_detail::fraction_tolerance<REAL4>(REAL4(1e-4)),
         boost::math::fpc::FPC_STRONG);
#else
  boost::test_tools::close_at_tolerance<REAL4> tester(
         boost::test_tools::fraction_tolerance_t<REAL4>(REAL4(1e-4)),
         boost::test_tools::FPC_STRONG);
#endif
  for(size_t i = 0; i < nrCells; ++i) {
    double validatedValue;
    validated->getCell(validatedValue, static_cast<size_t>(i));
    double resultValue;
    result->getCell(resultValue, static_cast<size_t>(i));

    if(!pcr::isMV(validatedValue)){
      if(!pcr::isMV(resultValue)){
        if(tester(static_cast<REAL4>(resultValue),static_cast<REAL4>(validatedValue))){
          cells[i] = 1;
        }
      }
    }
    else{
      if(pcr::isMV(resultValue)){
        cells[i] = 1;
      }
    }
  }

  return field;
}



void setGlobalOption(
         std::string const& option) {
  if(!calc::parseGlobalFlag(option)) {
    throw com::Exception((boost::format("Global option %1%: not supported")
         % option).str());
  }
}



// Overloads.
geo::LinearLoc (geo::RasterDim::*convert)(size_t, size_t) const =
         &geo::RasterDim::convert;


// to-Python converter for PCR_VS already registered; second conversion method ignored.



// void setCloneSpace(
//          geo::RasterSpace const& raster)
// {
//   globals.setCloneSpace(raster);
// }

void setCloneSpaceFromFilename(
         std::string const& filename)
{
  boost::shared_ptr<dal::Raster> raster(globals.rasterDal().read(filename));
  assert(raster);

  geo::Projection projection = geo::IllegalProjection;

  if(raster->properties().value<CSF_PT>(DAL_CSF_PROJECTION) == 0) {
    projection = geo::YIncrT2B;
  }
  else {
    projection = geo::YIncrB2T;
  }

  // \todo what about angle?

  geo::RasterSpace space(raster->nrRows(), raster->nrCols(),
         raster->cellSize(), raster->west(), raster->north(), projection);
  globals.setCloneSpace(space);
}


void setCloneSpaceFromValues(
    int nrRows,
    int nrCols,
    double cellSize,
    double west,
    double north)
{
  int max_row_col = std::numeric_limits<int>::max();

  if( (nrRows < 1) || (nrRows > max_row_col) ) {
    std::ostringstream errMsg;
    errMsg << "Number of rows '"
           << nrRows
           << "' out of range [1, (2 ^ 31) - 1]";
    throw std::invalid_argument(errMsg.str());
  }

  if( (nrCols < 1) || (nrCols > max_row_col) ) {
    std::ostringstream errMsg;
    errMsg << "Number of columns '"
           << nrCols
           << "' out of range [1, (2 ^ 31) - 1]";
    throw std::invalid_argument(errMsg.str());
  }

  if(cellSize <= 0.0) {
    std::ostringstream errMsg;
    errMsg << "Cell size '"
           << cellSize
           << "' must be larger than 0";
    throw std::invalid_argument(errMsg.str());

  }

  geo::RasterSpace space(nrRows, nrCols, cellSize, west, north, geo::YIncrB2T);
  globals.setCloneSpace(space);
}


void setRandomSeed(
         unsigned int seed)
{
  globals.setRandomSeed(seed);
}

//! Returns the clone raster space object.
geo::RasterSpace const& cloneSpace()
{
  return globals.cloneSpace();
}

//! Returns the runtime environment object.
calc::RunTimeEngine& rte()
{
  return globals.rte();
}

//! Reset globals variable to initial state.
void initGlobals()
{
  globals.init();
}


pybind11::object copyField(
          calc::Field const & /* field */)
{
  throw com::Exception("Shallow copy of PCRaster objects not supported");
}


calc::Field* deepCopyField(
          calc::Field const & field,
          pybind11::dict /* memo */)
{
  calc::Field* spatial = nullptr;

  if(field.isSpatial() == true){
    spatial = new calc::Spatial(field.vs(), field.cri(), globals.cloneSpace().nrRows() * globals.cloneSpace().nrCols());
    spatial->beMemCpyDest(field.src());
  }
  else{
    spatial = new calc::NonSpatial(field.vs());
    spatial->beMemCpyDest(field.src());
  }

  return spatial;
}

} // namespace python
} // namespace pcraster


PYBIND11_MODULE(_pcraster, module)
{
  using namespace pybind11;
  namespace pp = pcraster::python;

#ifndef NDEBUG
  {
      // For performance reasons it is useful to exactly know what value
      // is used to signal scalar missing values. All our code uses macro's
      // or functions to check for missing valueness, hiding the actual
      // special value used behind the scenes. When converting to numpy or
      // passing buffers to opencl, it is useful to be able to assume that
      // the missing value in the scalar case is a NaN, so we don't need
      // to test each value for missing valueness, but just get on with it.
      REAL4 value;
      pcr::setMV(value);
      assert(ValueTraits<REAL4>::is_negative_quiet_nan(value));
  }
#endif

  pybind11::register_exception_translator([](std::exception_ptr p) {
    try {
      if (p) std::rethrow_exception(p);
    }
    catch (dal::Exception const& exception) {
      PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
    }
    catch (calc::PosException const& exception) {
      PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
    }
    catch (calc::Exception const& exception) {
      PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
    }
    catch (pp::PyUtilsException const& exception) {
      PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
    }
    catch (com::Exception const& exception) {
      PyErr_SetString(PyExc_RuntimeError, exception.messages().c_str());
    }
  });

  // disables the C++ signatures in docstrings
  //pybind11::options options;
  //options.disable_function_signatures();


  #include "operations.inc"

  module.def("_initGlobals", &pp::initGlobals);

  module.def("clone", &pp::cloneSpace, return_value_policy::reference, "Returns the clone RasterSpace object");

  module.def("_rte", &pp::rte, return_value_policy::reference);

  module.def("setclone", &pp::setCloneSpaceFromFilename, R"(
   Set the clone properties from an existing raster.

   map -- Filename of clone map.
    )"
  );

  module.def("setclone", &pp::setCloneSpaceFromValues, R"(
   Set the clone using clone properties.

   nrRows -- Number of rows.

   nrCols -- Number of columns.

   cellSize -- Cell size.

   west -- Coordinate of west side of raster.

   north -- Coordinate of north side of raster.
    )"
  );

  module.def("setrandomseed", &pp::setRandomSeed, R"(
   Set the random seed.

   seed -- An integer value >= 0. If the seed is 0 then the seed is taken
           from the current time.
     )",
      arg("seed")
  );

  class_<geo::RasterSpace>(module, "RasterSpace")
    .def("nrRows", &geo::RasterSpace::nrRows, "Returns number of rows")
    .def("nrCols", &geo::RasterSpace::nrCols, "Returns number of columns")
    .def("north", &geo::RasterSpace::north, "Returns north coordinate")
    .def("west", &geo::RasterSpace::west, "Returns west coordinate")
    .def("cellSize", &geo::RasterSpace::cellSize, "Returns cell size")
    .def("convert", pp::convert)
    ;

  // The shared_ptr argument is here, so functions can return in smart Field
  // pointers to pass/share ownership of the Field instance.
  class_<calc::Field, std::shared_ptr<calc::Field>>(module, "Field")
    .def("isSpatial", &calc::Field::isSpatial)
    .def("_setCell", &calc::Field::setCell)
    .def("dataType", &calc::Field::vs)
    .def("__deepcopy__", pp::deepCopyField)
    .def("__copy__", pp::copyField)
    //.def("__init__", boost::python::make_constructor(&pp::initField))
    .def(pybind11::pickle(
        [](const calc::Field &field) {
          return pp::getstate(field);
        }
        ,
        [](pybind11::tuple state) {
          return pp::setstate(state);
        }
    ))
    ;

  // implicitly_convertible<discr::RasterData<REAL4>, calc::Spatial>();
  class_<calc::DataStorageId>(module, "DataStorageId")
      .def(init<std::string const&>())
      ;

  class_<calc::ObjectLink>(module, "ObjectLink");

  class_<calc::RunTimeEngine>(module, "RunTimeEngine")
    // the push method's have a check in PCRasterModelEngine for passing in 0/None
    //  such that type error messages are nice
    .def(init<geo::RasterSpace const&>())
    .def("pushField",         &calc::RunTimeEngine::pushField)
    .def("pushObjectLink",    &calc::RunTimeEngine::pushObjectLink)
    .def("pushDataStorageId", &calc::RunTimeEngine::pushDataStorageId)
    .def("releasePopField",       &calc::RunTimeEngine::releasePopField,
         return_value_policy::automatic)
    .def("releasePopObjectLink",  &calc::RunTimeEngine::releasePopObjectLink,
         return_value_policy::automatic)
    .def("setNrTimeSteps", &calc::RunTimeEngine::setNrTimeSteps)
    .def("setCurrentTimeStep", &calc::RunTimeEngine::setCurrentTimeStep)
    .def("checkAndExec", &calc::RunTimeEngine::checkAndExec)
    ;

  enum_<PCR_VS>(module, "VALUESCALE")
    .value("Boolean", VS_B)
    .value("Nominal", VS_N)
    .value("Ordinal", VS_O)
    .value("Scalar", VS_S)
    .value("Directional", VS_D)
    .value("Ldd", VS_L)
    .export_values()
    ;

// #ifdef DEBUG_DEVELOP
//   enum_<PCR_VS>("PCR_VS")
//     .value("VS_B", VS_B)
//     .value("VS_N", VS_N)
//     .value("VS_O", VS_O)
//     .value("VS_S", VS_S)
//     .value("VS_D", VS_D)
//     .value("VS_L", VS_L)
//
//     // .value("VS_BNOSDL", VS_BNOSDL)
//     // .value("VS_FIELD", VS_FIELD)
//     // .value("VS_BNO", VS_BNO)
//     // .value("VS_BNOL", VS_BNOL)
//     // .value("VS_NO", VS_NO)
//     // .value("VS_NOSDL", VS_NOSDL)
//     // .value("VS_OS", VS_OS)
//     // // .value("VS_SV", VS_SV)
//     // .value("VS_SD", VS_SD)
//     // .value("VS_BL", VS_BL)
//     ;
// #endif

  class_<calc::Operator>(module, "Operator");

  module.def("_loadCalcLib", &calc::loadCalcLib);
  module.def("_major2op", &calc::major2op,
    return_value_policy::reference);
  module.def("_opName2op", &calc::opName2op,
    return_value_policy::reference);


  module.def("readFieldCell", pp::readFieldCell);
  module.def("_newScalarField", pp::newScalarField,
         return_value_policy::automatic);
  module.def("_newNonSpatialField", pp::newNonSpatialFloatField,
         return_value_policy::automatic);
  module.def("_newNonSpatialField", pp::newNonSpatialIntegralField,
         return_value_policy::automatic);
  module.def("_closeAtTolerance", pp::closeAtTolerance,
         return_value_policy::automatic);


  // User functions. -----------------------------------------------------------
  // TODO  check bottom of com_tune.cc for an automatic hook in malloc lib so
  //       no explicit user call is needed and a good book reference
  // def("tune", com::tune,
  // "Tune model execution\n\n"
  // "Calling this funtion greatly improves memory management.\n"
  // "Drawback not very nice if small RAM and many processes.\n");

  module.def("report", pp::writeFilename, R"(
   Write data from a file to a file.

   filename -- Filename of data you want to open and write.
   filename -- Filename to use.
    )"
  );

  module.def("report", pp::writeField, R"(
   Write a map to a file.

   map -- Map you want to write.
   filename -- Filename to use.
    )"
  );

  module.def("readmap", pp::readField,
         return_value_policy::automatic, R"(
  Read a map.

  filename -- Filename of a map to read.
    )"
  );

  module.def("setglobaloption", pp::setGlobalOption, R"(
   Set the global option. The option argument must not contain the leading
   dashes as used on the command line of pcrcalc.

   Python example:
     setglobaloption("unitcell")

   The pcrcalc equivalent:
     pcrcalc --unitcell -f model.mod
    )"
  );

  module.def("pcr2numpy", pp::field_to_array);
  module.def("numpy2pcr", pp::array_to_field,
    return_value_policy::automatic);
  module.def("pcr_as_numpy", pp::field_as_array);

  module.def("cellvalue", pp::fieldGetCellIndex, R"(
   Return a cell value from a map.

   map -- Map you want to query.

   index -- Linear index of a cell in the map, ranging from
            [1, number-of-cells].

   Returns a tuple with two elements: the first is the cell value, the second
   is a boolean value which shows whether the first element, is valid or not.
   If the second element is False, the cell contains a missing value.

   See also: cellvalue(map, row, col)
    )",
    arg("map"), arg("index")
  );

  module.def("cellvalue", pp::fieldGetCellRowCol, R"(
   Return a cell value from a map.

   map -- Map you want to query.

   row -- Row index of a cell in the map, ranging from [1, number-of-rows].

   col -- Col index of a cell in the map, ranging from [1, number-of-cols].

   Returns a tuple with two elements: the first is the cell value,
   the second is a boolean value which shows whether the first element,
   is valid or not.
   If the second element is False, the cell contains a missing value.

   See also: cellvalue(map, index)
    )",
    arg("map"), arg("row"), arg("col")
  );

  module.def("cellvalue_by_index", pp::cellvalue_by_index, R"(
   Return a cell value from a map.

   map -- Map you want to query.

   index -- Linear index of a cell in the map, ranging from
            [0, number-of-cells].

   Returns a tuple with two elements: the first is the cell value, the second
   is a boolean value which shows whether the first element, is valid or not.
   If the second element is False, the cell contains a missing value.

.. versionadded:: 4.3
    )",
    arg("map"), arg("index")
  );


  module.def("cellvalue_by_indices", pp::cellvalue_by_indices, R"(
   Return a cell value from a map.

   map -- Map you want to query.

   row -- Row index of a cell in the map, ranging from [0, number-of-rows].

   col -- Col index of a cell in the map, ranging from [0, number-of-cols].

   Returns a tuple with two elements: the first is the cell value, the second
   is a boolean value which shows whether the first element, is valid or not.
   If the second element is False, the cell contains a missing value.

.. versionadded:: 4.3
    )",
    arg("map"), arg("row"), arg("col")
  );


  module.def("cellvalue_by_coordinates", pp::cellvalue_by_coordinates, R"(
   Return a cell value from a map.

   map -- Map you want to query.

   xcoordinate -- x coordinate of the point.

   ycoordinate -- y coordinate of the point.

   Returns a tuple with two elements: the first is the cell value, the second
   is a boolean value which shows whether the first element, is valid or not.
   If the second element is False, the cell contains a missing value.

   Note that no check on coordinate reference systems is performed.

.. versionadded:: 4.3
    )",
    arg("map"), arg("xcoordinate"), arg("ycoordinate")
  );

  module.def("version_tuple", [] () {
    return pybind11::make_tuple(PCRASTER_VERSION_MAJOR, PCRASTER_VERSION_MINOR, PCRASTER_VERSION_PATCH);
    },
    R"(
   Returns the PCRaster version as tuple (major, minor, patch)

.. versionadded:: 4.3
    )"
  );
}
