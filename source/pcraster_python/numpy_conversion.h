#pragma once

#include "calc_vs.h"

#include <pybind11/pybind11.h>
#include <pybind11/numpy.h>


namespace calc {
    class Field;
}
namespace geo {
    class RasterSpace;
}


namespace pcraster {
namespace python {



pybind11::array    field_to_array     (geo::RasterSpace const& space,
                                       calc::Field const* field,
                                       double const missing_value);

calc::Field*       array_to_field     (geo::RasterSpace const& space,
                                       VS const value_scale,
                                       pybind11::array const& array,
                                       double const missing_value);

pybind11::array    field_as_array      (geo::RasterSpace const& space,
                                        PyObject* field_object);

} // namespace python
} // namespace pcraster
