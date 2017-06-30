#pragma once
#include <boost/python.hpp>
#include "calc_field.h"


namespace pcraster {
namespace python {

  calc::Field* initField(int value_scale,
         int cri,
         size_t nr_rows,
         size_t nr_cols,
         double north,
         double west,
         double cell_size,
         int projection,
         int version);

  struct field_pickle_suite : boost::python::pickle_suite {

    static
    boost::python::tuple getinitargs   (calc::Field const & w);

    static
    boost::python::tuple getstate      (calc::Field const & w);

    static
    void           setstate            (calc::Field & w,
                                        boost::python::tuple state);

  };

} // namespace python
} // namespace pcraster
