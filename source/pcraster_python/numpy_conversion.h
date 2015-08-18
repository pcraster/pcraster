#pragma once
#include <boost/python.hpp>
#include <boost/python/numeric.hpp>
#include "calc_vs.h"


namespace calc {
    class Field;
}
namespace geo {
    class RasterSpace;
}


namespace pcraster {
namespace python {

bool               init_numpy          ();

boost::python::numeric::array field_to_array(
                                        geo::RasterSpace const& space,
                                        calc::Field const* field,
                                        double const missing_value);

calc::Field*       array_to_field      (geo::RasterSpace const& space,
                                        VS const value_scale,
                                        boost::python::numeric::array const& array,
                                        double const missing_value);

boost::python::numeric::array field_as_array(
                                        geo::RasterSpace const& space,
                                        PyObject* field_object);

} // namespace python
} // namespace pcraster
