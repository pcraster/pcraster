#ifndef INCLUDED_PYTHON_PICKLE
#define INCLUDED_PYTHON_PICKLE

#include <pybind11/pybind11.h>
#include "calc_field.h"



namespace pcraster::python {

  pybind11::tuple       getstate       (calc::Field const & field);

  calc::Field*          setstate       (pybind11::tuple const & state);

} // namespace pcraster::python


#endif