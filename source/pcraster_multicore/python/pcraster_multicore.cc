#include <boost/python.hpp>
#include <boost/python/docstring_options.hpp>
#include "pcraster_multicore/python/execution_policy.h"
#include "pcraster_multicore/python/local/local.h"
#include "pcraster_multicore/python/focal/focal.h"
#include "pcraster_multicore/python/total/total.h"
#include "pcraster_multicore/python/local/utils.h"
#include "pcraster_multicore/python/type_conversion/type_conversion.h"

namespace bp = boost::python;
namespace pmcpy = pcraster_multicore::python;



BOOST_PYTHON_MODULE(_pcraster_multicore){

  // disables the C++ signatures in docstrings
  bp::docstring_options doc_options(true, false);

  bp::def("set_nr_cpus", &pmcpy::set_nr_cpus,
    "Set the number of CPUs to be used in the PCRaster multicore algorithms");

  bp::def("_newNonSpatialScalar", &pmcpy::newNonSpatialScalar,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("_newNonSpatialNominal", &pmcpy::newNonSpatialNominal,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("_newNonSpatialBoolean", &pmcpy::newNonSpatialBoolean,
    bp::return_value_policy<bp::manage_new_object>());

  // Local operations
  bp::def("_and", &pmcpy::_and,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("_or", &pmcpy::_or,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("_xor", &pmcpy::_xor,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("_not", &pmcpy::_not,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("defined", &pmcpy::defined,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("ifthen", &pmcpy::ifthen,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("ifthenelse", &pmcpy::ifthenelse,
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("cover", &pmcpy::cover,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("maximum", &pmcpy::maximum,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("minimum", &pmcpy::minimum,
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("boolean", &pmcpy::boolean,
   bp::return_value_policy<bp::manage_new_object>());
  bp::def("nominal", &pmcpy::nominal,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("ordinal", &pmcpy::ordinal,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("scalar", &pmcpy::scalar,
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("equal", &pmcpy::equal,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("unequal", &pmcpy::unequal,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("less", &pmcpy::less,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("less_equal", &pmcpy::less_equal,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("greater", &pmcpy::greater,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("greater_equal", &pmcpy::greater_equal,
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("add", &pmcpy::add,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("sub", &pmcpy::sub,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("mul", &pmcpy::mul,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("div", &pmcpy::div,
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("sqrt", &pmcpy::sqrt,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("abs", &pmcpy::abs,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("power", &pmcpy::power,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("sqr", &pmcpy::sqr,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("fac", &pmcpy::fac,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("ln", &pmcpy::ln,
   bp::return_value_policy<bp::manage_new_object>());
  bp::def("log10", &pmcpy::log10,
   bp::return_value_policy<bp::manage_new_object>());
  bp::def("rounddown", &pmcpy::rounddown,
   bp::return_value_policy<bp::manage_new_object>());
  bp::def("roundup", &pmcpy::roundup,
   bp::return_value_policy<bp::manage_new_object>());
  bp::def("roundoff", &pmcpy::roundoff,
   bp::return_value_policy<bp::manage_new_object>());

  bp::def("cos", &pmcpy::cos,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("sin", &pmcpy::sin,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("tan", &pmcpy::tan,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("acos", &pmcpy::acos,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("asin", &pmcpy::asin,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("atan", &pmcpy::atan,
    bp::return_value_policy<bp::manage_new_object>());


  // Focal operations
  bp::def("slope", &pmcpy::slope,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("window4total", &pmcpy::window4total,
    bp::return_value_policy<bp::manage_new_object>());


  // Operations on full map extent
  bp::def("mapmaximum", &pmcpy::mapmaximum,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("mapminimum", &pmcpy::mapminimum,
    bp::return_value_policy<bp::manage_new_object>());
}
