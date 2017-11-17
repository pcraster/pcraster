#pragma once
#include <boost/python.hpp>
#if BOOST_VERSION < 106500
#include <boost/python/numeric.hpp>
#else
#include <boost/python/numpy.hpp>
#endif
#include "calc_vs.h"


namespace calc {
    class Field;
}
namespace geo {
    class RasterSpace;
}


namespace pcraster {
namespace python {

#if BOOST_VERSION < 106500
  typedef typename boost::python::numeric::array bpn_array;
#else
  typedef typename boost::python::numpy::ndarray bpn_array;
#endif


#if BOOST_VERSION < 106500
    #if PY_MAJOR_VERSION >= 3
    #define DEFINE_INIT_NUMPY()  \
    static void* init_numpy()    \
    {                            \
        import_array();          \
        return NULL;             \
    }
    #else
    #define DEFINE_INIT_NUMPY()  \
    static void init_numpy()     \
    {                            \
        import_array();          \
    }
    #endif
#else
    #define DEFINE_INIT_NUMPY()  \
    static void init_numpy() {}
#endif


bpn_array          field_to_array     (geo::RasterSpace const& space,
                                       calc::Field const* field,
                                       double const missing_value);

calc::Field*       array_to_field     (geo::RasterSpace const& space,
                                       VS const value_scale,
                                       bpn_array const& array,
                                       double const missing_value);

bpn_array          field_as_array      (geo::RasterSpace const& space,
                                        PyObject* field_object);

} // namespace python
} // namespace pcraster
