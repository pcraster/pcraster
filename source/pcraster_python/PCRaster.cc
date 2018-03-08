// This header first!
#include <boost/python.hpp>

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
#include "docstrings.h"
#include "numpy_conversion.h"
#include "pickle.h"
#include "ppu_exception.h"

#ifndef INCLUDED_BOOST_VERSION
#include <boost/version.hpp>
#define INCLUDED_BOOST_VERSION
#endif

#if BOOST_VERSION > 105800
#include <boost/test/tools/floating_point_comparison.hpp>
#else
#include <boost/test/floating_point_comparison.hpp>
#endif

#if BOOST_VERSION < 106500
#include <numpy/arrayobject.h>
#endif



#include <boost/format.hpp>
#include <boost/python/docstring_options.hpp>
#include <boost/shared_ptr.hpp>


#if _MSC_VER == 1900
  // Workaround wrt Boost Python and VS2015v3
  namespace boost
  {
    template <>
    calc::Field const volatile * get_pointer(class calc::Field const volatile *f)
    {
      return f;
    }

    template <>
    geo::RasterSpace const volatile * get_pointer<class geo::RasterSpace const volatile>
      (class geo::RasterSpace const volatile *r)
    {
      return r;
    }
  }
#endif


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

//! Translates dal::Exception to Python RuntimeError exception.
void translator1(dal::Exception const& exception) {
  PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
}

//! Translates com::Exception to Python RuntimeError exception.
void translator2(com::Exception const& exception) {
  PyErr_SetString(PyExc_RuntimeError, exception.messages().c_str());
}

//! Translates calc::PosException to Python RuntimeError exception.
void translator3(calc::PosException const& exception) {
  PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
}

//! Translates calc::Exception to Python RuntimeError exception.
void translator4(calc::Exception const& exception) {
  PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
}

//! Translates calc::PosException to Python RuntimeError exception.
void translator5(PyUtilsException const& exception) {
  PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
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
boost::python::object readFieldCell(
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

  boost::python::object tuple;

  switch(raster->typeId()) {
    case dal::TI_UINT1: {
      UINT1 v;
      driver->read(&v, dal::TI_UINT1, filename, space, address);
      tuple = boost::python::make_tuple(static_cast<float>(v), !static_cast<int>(pcr::isMV(v)));
      break;
    }
    case dal::TI_INT4: {
      INT4 v;
      driver->read(&v, dal::TI_INT4, filename, space, address);
      tuple = boost::python::make_tuple(static_cast<float>(v), !static_cast<int>(pcr::isMV(v)));
      break;
    }
    case dal::TI_REAL4: {
      REAL4 v;
      driver->read(&v, dal::TI_REAL4, filename, space, address);
      tuple = boost::python::make_tuple(static_cast<float>(v), !static_cast<int>(pcr::isMV(v)));
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



boost::python::tuple fieldGetCellIndex(
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

  boost::python::tuple tuple;
  double value = 0;
  bool isValid = field->getCell(value, static_cast<size_t>(index));

  switch(field->vs()) {
    case VS_B: {
      tuple = boost::python::make_tuple<bool, bool>(
         static_cast<bool>(value), isValid);
      break;
    }
    case VS_L:
    case VS_N:
    case VS_O: {
      tuple = boost::python::make_tuple<int, bool>(
         static_cast<int>(value), isValid);
      break;
    }
    case VS_S:
    case VS_D: {
      // std::cout << (double(0.4) == double(value)) << std::endl;
      // std::cout << (float(0.4) == float(value)) << std::endl;
      // --> std::cout << (double(float(0.4)) == value) << std::endl;
      // TODO pak een selectie van bytes om de float te maken?
      tuple = boost::python::make_tuple<float, bool>(
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



boost::python::tuple fieldGetCellRowCol(
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


template<class T>
inline PyObject * managingPyObject(
         T *p)
{
  return typename boost::python::manage_new_object::apply<T *>::type()(p);
}


boost::python::object copyField(
          calc::Field const & /* field */)
{
  throw com::Exception("Shallow copy of PCRaster objects not supported");
}


boost::python::object deepCopyField(
          calc::Field const & field,
          boost::python::dict /* memo */)
{
  calc::Field* spatial = 0;

  if(field.isSpatial() == true){
    spatial = new calc::Spatial(field.vs(), field.cri(), globals.cloneSpace().nrRows() * globals.cloneSpace().nrCols());
    spatial->beMemCpyDest(field.src());
  }
  else{
    spatial = new calc::NonSpatial(field.vs());
    spatial->beMemCpyDest(field.src());
  }

  return boost::python::object(boost::python::detail::new_reference(managingPyObject(spatial)));
}

} // namespace python
} // namespace pcraster



BOOST_PYTHON_MODULE(_pcraster)
{
  using namespace boost::python;
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

  register_exception_translator<dal::Exception>(&pp::translator1);
  register_exception_translator<com::Exception>(&pp::translator2);
  register_exception_translator<calc::PosException>(&pp::translator3);
  register_exception_translator<calc::Exception>(&pp::translator4);
  register_exception_translator<pcraster::python::PyUtilsException>(&pp::translator5);

  // disables the C++ signatures in docstrings
  docstring_options doc_options(true, true, false);

  /// init_numpy();

#if BOOST_VERSION < 106500
  boost::python::numeric::array::set_module_and_type("numpy", "ndarray");
#else
  boost::python::numpy::initialize();
#endif


  #include "operations.inc"

  def("_initGlobals", &pp::initGlobals);

  def("clone", &pp::cloneSpace,
    return_value_policy<reference_existing_object>());

  def("_rte", &pp::rte,
    return_value_policy<reference_existing_object>());

  def("setclone", &pp::setCloneSpaceFromFilename,
  "Set the clone properties from an existing raster.\n"
  "\n"
  "map -- Filename of clone map.\n");

  def("setclone", &pp::setCloneSpaceFromValues,
  "Set the clone using clone properties.\n"
  "\n"
  "nrRows -- Number of rows.\n"
  "nrCols -- Number of columns.\n"
  "cellSize -- Cell size.\n"
  "west -- Coordinate of west side of raster.\n"
  "north -- Coordinate of north side of raster.\n");

  def("setrandomseed", &pp::setRandomSeed,
  "Set the random seed.\n"
  "\n"
  "seed -- An integer value >= 0. If the seed is 0 then the seed is taken\n"
  "        from the current time.\n", args("seed"));

  class_<geo::RasterSpace>("RasterSpace")
    .def("nrRows", &geo::RasterSpace::nrRows)
    .def("nrCols", &geo::RasterSpace::nrCols)
    .def("north", &geo::RasterSpace::north)
    .def("west", &geo::RasterSpace::west)
    .def("cellSize", &geo::RasterSpace::cellSize)
    .def("convert", pp::convert)
    ;

  // The shared_ptr argument is here, so functions can return in smart Field
  // pointers to pass/share ownership of the Field instance.
  class_<calc::Field, boost::shared_ptr<calc::Field>, boost::noncopyable>(
         "Field", no_init)
    .def("isSpatial", &calc::Field::isSpatial)
    .def("setCell", &calc::Field::setCell)
    .def("dataType", &calc::Field::vs)
    .def("__deepcopy__", pp::deepCopyField)
    .def("__copy__", pp::copyField)
    .def("__init__", boost::python::make_constructor(&pp::initField))
    .def_pickle(pp::field_pickle_suite())
    ;
  // implicitly_convertible<discr::RasterData<REAL4>, calc::Spatial>();
  class_<calc::DataStorageId, boost::noncopyable>("DataStorageId", init<std::string const&>())
    ;
  class_<calc::ObjectLink, boost::noncopyable>("ObjectLink", no_init)
    ;

  class_<calc::RunTimeEngine, boost::noncopyable>("RunTimeEngine", init<geo::RasterSpace const&>())
    // the push method's have a check in PCRasterModelEngine for passing in 0/None
    //  such that type error messages are nice
    .def("pushField",         &calc::RunTimeEngine::pushField)
    .def("pushObjectLink",    &calc::RunTimeEngine::pushObjectLink)
    .def("pushDataStorageId", &calc::RunTimeEngine::pushDataStorageId)

    .def("releasePopField",       &calc::RunTimeEngine::releasePopField,
         return_value_policy<manage_new_object>())
    .def("releasePopObjectLink",  &calc::RunTimeEngine::releasePopObjectLink,
         return_value_policy<manage_new_object>())

    .def("setNrTimeSteps", &calc::RunTimeEngine::setNrTimeSteps)
    .def("setCurrentTimeStep", &calc::RunTimeEngine::setCurrentTimeStep)
    .def("checkAndExec", &calc::RunTimeEngine::checkAndExec)
    ;

  enum_<PCR_VS>("VALUESCALE")
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

  class_<calc::Operator, boost::noncopyable>("Operator", no_init)
    ;

  def("loadCalcLib", &calc::loadCalcLib);
  def("_major2op", &calc::major2op,
    return_value_policy<reference_existing_object>());
  // def("opName2op", calc::opName2op, opName2opOverloads(),
  //   return_value_policy<reference_existing_object>());
  def("_opName2op", &calc::opName2op,
    return_value_policy<reference_existing_object>());


  def("readFieldCell", pp::readFieldCell);
  def("newScalarField", pp::newScalarField,
         return_value_policy<manage_new_object>());
  def("newNonSpatialField", pp::newNonSpatialFloatField,
         return_value_policy<manage_new_object>());
  def("newNonSpatialField", pp::newNonSpatialIntegralField,
         return_value_policy<manage_new_object>());
  def("_closeAtTolerance", pp::closeAtTolerance,
         return_value_policy<manage_new_object>());


  // User functions. -----------------------------------------------------------
  // TODO  check bottom of com_tune.cc for an automatic hook in malloc lib so
  //       no explicit user call is needed and a good book reference
  // def("tune", com::tune,
  // "Tune model execution\n\n"
  // "Calling this funtion greatly improves memory management.\n"
  // "Drawback not very nice if small RAM and many processes.\n");

  def("report", pp::writeFilename,
  "Write data from a file to a file.\n"
  "\n"
  "filename -- Filename of data you want to open and write.\n"
  "filename -- Filename to use.\n");

  def("report", pp::writeField,
  "Write a map to a file.\n"
  "\n"
  "map -- Map you want to write.\n"
  "filename -- Filename to use.\n");

  def("readmap", pp::readField,
         return_value_policy<manage_new_object>(),
  "Read a map.\n"
  "\n"
  "filename -- Filename of a map to read.\n");

  def("cellvalue", pp::fieldGetCellIndex, cellvalue_idx_doc.c_str(),
    args("map", "index")
  );

  def("cellvalue", pp::fieldGetCellRowCol, cellvalue_rc_doc.c_str(),
    args("map", "row", "col")
  );

  def("setglobaloption", pp::setGlobalOption,
  "Set the global option. The option argument must not contain the leading\n"
  "dashes as used on the command line of pcrcalc.\n"
  "\n"
  "Python example:\n"
  "  setglobaloption(\"unitcell\")\n"
  "\n"
  "The pcrcalc equivalent:\n"
  "  pcrcalc --unitcell -f model.mod\n"
  );

  def("pcr2numpy", pcraster::python::field_to_array);
  def("numpy2pcr", pcraster::python::array_to_field,
    return_value_policy<manage_new_object>());
  def("pcr_as_numpy", pcraster::python::field_as_array);
}
