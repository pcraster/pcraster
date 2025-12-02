#include "pcraster_multicore/python/execution_policy.h"
#include "pcraster_multicore/python/local/local.h"
#include "pcraster_multicore/python/focal/focal.h"
#include "pcraster_multicore/python/total/total.h"
#include "pcraster_multicore/python/local/utils.h"
#include "pcraster_multicore/python/type_conversion/type_conversion.h"
#include "dal_Exception.h"
#include "com_exception.h"
#include "fern/core/exception.h"

#include <pybind11/pybind11.h>


namespace py = pybind11;
namespace pmcpy = pcraster_multicore::python;

PYBIND11_MODULE(_pcraster_multicore, module){


  pybind11::register_exception_translator([](std::exception_ptr p) {
    try {
      if (p) {
        std::rethrow_exception(p);
      }
    }
    catch (dal::Exception const& exception) {
      PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
    }
    catch (fern::Exception const& exception) {
      PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
    }
    catch (com::Exception const& exception) {
      PyErr_SetString(PyExc_RuntimeError, exception.messages().c_str());
    }
  });

  // show user defined docstrings and Python signatures, disable the C++ signatures
  py::options options;
  options.disable_function_signatures();


  try {
      // Upon module initialization the execution policy must be constructed, not sooner
      pmcpy::construct_execution_policy();

      // Upon module finalization, the execution policy must be destructed, not later
      py::module::import("atexit").attr("register")(
          py::cpp_function(&pmcpy::destruct_execution_policy));
  }
  catch(py::error_already_set&) {
      pmcpy::destruct_execution_policy();
      throw;
  }


  module.def("set_nr_worker_threads", &pmcpy::set_nr_worker_threads,
    "Set the number of worker threads to be used in the PCRaster multicore algorithms",
    py::arg("nr_threads"));

  module.def("nr_worker_threads", &pmcpy::nr_worker_threads,
    "Returns the number of worker threads currently used in the PCRaster multicore algorithms");

  module.def("_newNonSpatialScalar", &pmcpy::newNonSpatialScalar);
  module.def("_newNonSpatialNominal", &pmcpy::newNonSpatialNominal);
  module.def("_newNonSpatialBoolean", &pmcpy::newNonSpatialBoolean);
  module.def("_unittrue", &pmcpy::global_option_unittrue);


  // Local operations
  module.def("_and", &pmcpy::_and,
    "Cell-wise boolean and operation. Equivalent to 'expression1 & expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("_or", &pmcpy::_or,
    "Cell-wise boolean or operation. Equivalent to 'expression1 | expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("_xor", &pmcpy::_xor,
    "Cell-wise boolean xor operation. Equivalent to 'expression1 ^ expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("_not", &pmcpy::_not,
    "Cell-wise boolean not operation. Equivalent to '~expression'.",
    py::arg("expression")
    );


  module.def("defined", &pmcpy::defined,
    "Cell-wise, returns whether the cell value on expression is a missing value or not.",
    py::arg("expression")
    );

  module.def("cover", &pmcpy::cover,
    "Cell-wise, substitutes missing values with values taken from one or more expressions",
    py::arg("expression*")
    );

  module.def("ifthen", &pmcpy::ifthen,
    "Cell-wise, for True values on condition expression1 is assigned, missing value otherwise.",
    py::arg("condition"), py::arg("expression1")
    );

  module.def("ifthenelse", &pmcpy::ifthenelse,
    "Cell-wise, for True values on condition expression1 is assigned, expression2 otherwise.",
    py::arg("condition"), py::arg("expression1"), py::arg("expression2")
    );


  module.def("maximum", &pmcpy::maximum,
    "Cell-wise maximum value of multiple expressions.",
    py::arg("expression*")
    );

  module.def("minimum", &pmcpy::minimum,
    "Cell-wise minimum value of multiple expressions.",
    py::arg("expression*")
    );


  module.def("boolean", &pmcpy::boolean,
    "Cell-wise conversion of expression to boolean data type.",
    py::arg("expression")
   );

  module.def("nominal", &pmcpy::nominal,
    "Cell-wise conversion of expression to nominal data type.",
    py::arg("expression")
    );

  module.def("ordinal", &pmcpy::ordinal,
    "Cell-wise conversion of expression to ordinal data type.",
    py::arg("expression")
    );

  module.def("scalar", &pmcpy::scalar,
    "Cell-wise conversion of expression to scalar data type.",
    py::arg("expression")
    );


  module.def("equal", &pmcpy::equal,
    "Cell-wise relational equal-to operation. Equivalent to 'expression1 == expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("unequal", &pmcpy::unequal,
    "Cell-wise relational not-equal-to  operation. Equivalent to 'expression1 != expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("less", &pmcpy::less,
    "Cell-wise relational less-than operation. Equivalent to 'expression1 < expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("less_equal", &pmcpy::less_equal,
    "Cell-wise relational less-than-or-equal-to operation. Equivalent to 'expression1 <= expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("greater", &pmcpy::greater,
    "Cell-wise relational greater-than operation. Equivalent to 'expression1 < expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("greater_equal", &pmcpy::greater_equal,
    "Cell-wise relational greater-than-or-equal-to operation. Equivalent to 'expression1 >= expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );


  module.def("add", &pmcpy::add,
    "Add arguments cell-wise. Equivalent to 'expression1 + expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("sub", &pmcpy::sub,
    "Subtract arguments cell-wise. Equivalent to 'expression1 - expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("mul", &pmcpy::mul,
    "Multiply arguments cell-wise. Equivalent to 'expression1 * expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );

  module.def("div", &pmcpy::div,
    "Divide arguments cell-wise. Equivalent to 'expression1 / expression2'.",
    py::arg("expression1"), py::arg("expression2")
    );


  module.def("sqrt", &pmcpy::sqrt,
    "Cell-wise square root of an expression.",
    py::arg("expression")
    );

  module.def("abs", &pmcpy::abs,
    "Cell-wise absolute value of an expression.",
    py::arg("expression")
    );

  module.def("power", &pmcpy::power,
    "Cell-wise base raised to the power exponent. Equivalent to 'base ** exponent'.",
    py::arg("base"), py::arg("exponent")
    );

  module.def("sqr", &pmcpy::sqr,
    "Cell-wise square of an expression.",
    py::arg("expression")
    );

  module.def("fac", &pmcpy::fac,
    "Cell-wise factorial of a natural positive number of an expression.",
    py::arg("expression")
    );

  module.def("ln", &pmcpy::ln,
    "Cell-wise natural logarithm of an expression.",
    py::arg("expression")
   );

  module.def("log10", &pmcpy::log10,
    "Cell-wise base 10 logarithm of an expression.",
    py::arg("expression")
   );

  module.def("rounddown", &pmcpy::rounddown,
    "Cell-wise the value of an expression is rounded downwards.",
    py::arg("expression")
   );

  module.def("roundup", &pmcpy::roundup,
    "Cell-wise the value of an expression is rounded upwards.",
    py::arg("expression")
   );

  module.def("roundoff", &pmcpy::roundoff,
    "Cell-wise the value of an expression is rounded off.",
    py::arg("expression")
   );


  module.def("cos", &pmcpy::cos,
    "Cell-wise cosine.",
    py::arg("expression")
    );

  module.def("sin", &pmcpy::sin,
    "Cell-wise sine.",
    py::arg("expression")
    );

  module.def("tan", &pmcpy::tan,
    "Cell-wise tangent.",
    py::arg("expression")
    );

  module.def("acos", &pmcpy::acos,
    "Cell-wise inverse cosine.",
    py::arg("expression")
    );

  module.def("asin", &pmcpy::asin,
    "Cell-wise inverse sine.",
    py::arg("expression")
    );

  module.def("atan", &pmcpy::atan,
    "Cell-wise inverse tangent.",
    py::arg("expression")
    );


  // Focal operations
  module.def("slope", &pmcpy::slope,
    "Cell-wise slope on basis of the elevation 'dem'.",
    py::arg("dem")
    );

  module.def("window4total", &pmcpy::window4total,
    "Cell-wise, sums the values of the four cells which lie above, below, left and right of the cell.",
    py::arg("expression")
    );

  module.def("windowtotal", &pmcpy::windowtotal,
    "Cell-wise, sums the values of a square window defined by windowlength.",
    py::arg("expression"), py::arg("windowlength")
    );

  module.def("windowaverage", &pmcpy::windowaverage,
    "Cell-wise, averages the values of a square window defined by windowlength.",
    py::arg("expression"), py::arg("windowlength")
    );


  // Operations on full map extent
  module.def("mapmaximum", &pmcpy::mapmaximum,
    "Maximum cell value of the expression.",
    py::arg("expression")
    );

  module.def("mapminimum", &pmcpy::mapminimum,
    "Minimum cell value of the expression.",
    py::arg("expression")
    );
}
