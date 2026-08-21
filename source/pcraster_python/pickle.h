#ifndef INCLUDED_PYTHON_PICKLE
#define INCLUDED_PYTHON_PICKLE

#include <nanobind/nanobind.h>
#include <nanobind/stl/tuple.h>

#include "calc_field.h"



namespace pcraster::python {

  nanobind::tuple       getstate       (calc::Field const & field);

  void         setstate       (calc::Field* field, nanobind::tuple const & state);

} // namespace pcraster::python


#endif