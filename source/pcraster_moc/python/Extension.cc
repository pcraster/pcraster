#include "calc_spatial.h"
#include "Moc.h"

#include <nanobind/nanobind.h>
#include <nanobind/stl/shared_ptr.h>


NB_MODULE(_pcraster_moc, module) {
  namespace nb = nanobind;
  namespace mp = moc::python;

  nb::class_<mp::Moc>(module, "initialise")
    .def(nb::init<geo::RasterSpace const&, double, UINT4, calc::Field const*,
              calc::Field const*, calc::Field const*>())
    .def("transport", &mp::Moc::transport)
    .def("adjust", &mp::Moc::adjust)
    ;
}

