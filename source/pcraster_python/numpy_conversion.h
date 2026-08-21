#ifndef INCLUDED_PYTHON_NUMPY_CONVERSION
#define INCLUDED_PYTHON_NUMPY_CONVERSION

#include "calc_vs.h"

#include <nanobind/nanobind.h>
#include <nanobind/ndarray.h>


namespace calc {
    class Field;
}
namespace geo {
    class RasterSpace;
}



namespace pcraster::python {



nanobind::ndarray<nanobind::numpy>    field_to_array     (geo::RasterSpace const& space,
                                       calc::Field const* field,
                                       double const missing_value);

calc::Field*       array_to_field     (geo::RasterSpace const& space,
                                       VS const value_scale,
                                       nanobind::ndarray<nanobind::numpy> const& array,
                                       double missing_value);

nanobind::ndarray<nanobind::numpy>    field_as_array     (geo::RasterSpace const& space,
                                       nanobind::object* field_object);

} // namespace pcraster::python


#endif