#include <boost/python.hpp>
#include <boost/python/docstring_options.hpp>
#include "pcraster_multicore/python/execution_policy.h"
#include "pcraster_multicore/python/local/local.h"
#include "pcraster_multicore/python/focal/focal.h"
#include "pcraster_multicore/python/total/total.h"
#include "pcraster_multicore/python/local/utils.h"
#include "pcraster_multicore/python/type_conversion/type_conversion.h"
#include "dal_Exception.h"
#include "com_exception.h"
#include "fern/core/exception.h"


#if _MSC_VER == 1900
  #include "geo_rasterspace.h"
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


namespace pcraster_multicore {
namespace python {

//! Translates dal::Exception to Python RuntimeError exception.
void translator1(dal::Exception const& exception) {
  PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
}

//! Translates fern::Exception to Python RuntimeError exception.
void translator2(fern::Exception const& exception) {
  PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
}

//! Translates com::Exception to Python RuntimeError exception.
void translator3(com::Exception const& exception) {
  PyErr_SetString(PyExc_RuntimeError, exception.messages().c_str());
}

} // namespace
} // namespace


namespace bp = boost::python;
namespace pmcpy = pcraster_multicore::python;

BOOST_PYTHON_MODULE(_pcraster_multicore){

  bp::register_exception_translator<dal::Exception>(&pmcpy::translator1);
  bp::register_exception_translator<fern::Exception>(&pmcpy::translator2);
  bp::register_exception_translator<com::Exception>(&pmcpy::translator3);

  // show user defined docstrings and Python signatures, disable the C++ signatures
  bp::docstring_options doc_options(true, true, false);


  try {
      // Upon module initialization the execution policy must be constructed, not sooner
      pmcpy::construct_execution_policy();

      // Upon module finalization, the execution policy must be destructed, not later
      bp::import("atexit").attr("register")(
          bp::make_function(&pmcpy::destruct_execution_policy));
  }
  catch(bp::error_already_set&) {
      pmcpy::destruct_execution_policy();
      throw;
  }


  bp::def("set_nr_worker_threads", &pmcpy::set_nr_worker_threads,
    "Set the number of worker threads to be used in the PCRaster multicore algorithms",
    bp::args("nr_threads"));

  bp::def("nr_worker_threads", &pmcpy::nr_worker_threads,
    "Returns the number of worker threads currently used in the PCRaster multicore algorithms");

  bp::def("_newNonSpatialScalar", &pmcpy::newNonSpatialScalar,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("_newNonSpatialNominal", &pmcpy::newNonSpatialNominal,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("_newNonSpatialBoolean", &pmcpy::newNonSpatialBoolean,
    bp::return_value_policy<bp::manage_new_object>());
  bp::def("_unittrue", &pmcpy::global_option_unittrue);


  // Local operations
  bp::def("_and", &pmcpy::_and,
    "Cell-wise boolean and operation. Equivalent to 'expression1 & expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("_or", &pmcpy::_or,
    "Cell-wise boolean or operation. Equivalent to 'expression1 | expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("_xor", &pmcpy::_xor,
    "Cell-wise boolean xor operation. Equivalent to 'expression1 ^ expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("_not", &pmcpy::_not,
    "Cell-wise boolean not operation. Equivalent to '~expression'.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());


  bp::def("defined", &pmcpy::defined,
    "Cell-wise, returns whether the cell value on expression is a missing value or not.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("cover", &pmcpy::cover,
    "Cell-wise, substitutes missing values with values taken from one or more expressions",
    bp::args("expression*"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("ifthen", &pmcpy::ifthen,
    "Cell-wise, for True values on condition expression1 is assigned, missing value otherwise.",
    bp::args("condition", "expression1"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("ifthenelse", &pmcpy::ifthenelse,
    "Cell-wise, for True values on condition expression1 is assigned, expression2 otherwise.",
    bp::args("condition", "expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());


  bp::def("maximum", &pmcpy::maximum,
    "Cell-wise maximum value of multiple expressions.",
    bp::args("expression*"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("minimum", &pmcpy::minimum,
    "Cell-wise minimum value of multiple expressions.",
    bp::args("expression*"),
    bp::return_value_policy<bp::manage_new_object>());


  bp::def("boolean", &pmcpy::boolean,
    "Cell-wise conversion of expression to boolean data type.",
    bp::args("expression"),
   bp::return_value_policy<bp::manage_new_object>());

  bp::def("nominal", &pmcpy::nominal,
    "Cell-wise conversion of expression to nominal data type.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("ordinal", &pmcpy::ordinal,
    "Cell-wise conversion of expression to ordinal data type.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("scalar", &pmcpy::scalar,
    "Cell-wise conversion of expression to scalar data type.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());


  bp::def("equal", &pmcpy::equal,
    "Cell-wise relational equal-to operation. Equivalent to 'expression1 == expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("unequal", &pmcpy::unequal,
    "Cell-wise relational not-equal-to  operation. Equivalent to 'expression1 != expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("less", &pmcpy::less,
    "Cell-wise relational less-than operation. Equivalent to 'expression1 < expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("less_equal", &pmcpy::less_equal,
    "Cell-wise relational less-than-or-equal-to operation. Equivalent to 'expression1 <= expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("greater", &pmcpy::greater,
    "Cell-wise relational greater-than operation. Equivalent to 'expression1 < expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("greater_equal", &pmcpy::greater_equal,
    "Cell-wise relational greater-than-or-equal-to operation. Equivalent to 'expression1 >= expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());


  bp::def("add", &pmcpy::add,
    "Add arguments cell-wise. Equivalent to 'expression1 + expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("sub", &pmcpy::sub,
    "Subtract arguments cell-wise. Equivalent to 'expression1 - expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("mul", &pmcpy::mul,
    "Multiply arguments cell-wise. Equivalent to 'expression1 * expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("div", &pmcpy::div,
    "Divide arguments cell-wise. Equivalent to 'expression1 / expression2'.",
    bp::args("expression1", "expression2"),
    bp::return_value_policy<bp::manage_new_object>());


  bp::def("sqrt", &pmcpy::sqrt,
    "Cell-wise square root of an expression.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("abs", &pmcpy::abs,
    "Cell-wise absolute value of an expression.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("power", &pmcpy::power,
    "Cell-wise base raised to the power exponent. Equivalent to 'base ** exponent'.",
    bp::args("base", "exponent"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("sqr", &pmcpy::sqr,
    "Cell-wise square of an expression.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("fac", &pmcpy::fac,
    "Cell-wise factorial of a natural positive number of an expression.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("ln", &pmcpy::ln,
    "Cell-wise natural logarithm of an expression.",
    bp::args("expression"),
   bp::return_value_policy<bp::manage_new_object>());

  bp::def("log10", &pmcpy::log10,
    "Cell-wise base 10 logarithm of an expression.",
    bp::args("expression"),
   bp::return_value_policy<bp::manage_new_object>());

  bp::def("rounddown", &pmcpy::rounddown,
    "Cell-wise the value of an expression is rounded downwards.",
    bp::args("expression"),
   bp::return_value_policy<bp::manage_new_object>());

  bp::def("roundup", &pmcpy::roundup,
    "Cell-wise the value of an expression is rounded upwards.",
    bp::args("expression"),
   bp::return_value_policy<bp::manage_new_object>());

  bp::def("roundoff", &pmcpy::roundoff,
    "Cell-wise the value of an expression is rounded off.",
    bp::args("expression"),
   bp::return_value_policy<bp::manage_new_object>());


  bp::def("cos", &pmcpy::cos,
    "Cell-wise cosine.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("sin", &pmcpy::sin,
    "Cell-wise sine.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("tan", &pmcpy::tan,
    "Cell-wise tangent.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("acos", &pmcpy::acos,
    "Cell-wise inverse cosine.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("asin", &pmcpy::asin,
    "Cell-wise inverse sine.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("atan", &pmcpy::atan,
    "Cell-wise inverse tangent.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());


  // Focal operations
  bp::def("slope", &pmcpy::slope,
    "Cell-wise slope on basis of the elevation 'dem'.",
    bp::args("dem"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("window4total", &pmcpy::window4total,
    "Cell-wise, sums the values of the four cells which lie above, below, left and right of the cell.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("windowtotal", &pmcpy::windowtotal,
    "Cell-wise, sums the values of a square window defined by windowlength.",
    bp::args("expression", "windowlength"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("windowaverage", &pmcpy::windowaverage,
    "Cell-wise, averages the values of a square window defined by windowlength.",
    bp::args("expression", "windowlength"),
    bp::return_value_policy<bp::manage_new_object>());


  // Operations on full map extent
  bp::def("mapmaximum", &pmcpy::mapmaximum,
    "Maximum cell value of the expression.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());

  bp::def("mapminimum", &pmcpy::mapminimum,
    "Minimum cell value of the expression.",
    bp::args("expression"),
    bp::return_value_policy<bp::manage_new_object>());
}
