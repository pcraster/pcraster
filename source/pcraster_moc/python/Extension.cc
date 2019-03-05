// External headers.

// Project headers.
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

// Module headers.
#ifndef INCLUDED_MOC
#include "Moc.h"
#define INCLUDED_MOC
#endif



PYBIND11_MODULE(_pcraster_moc, module) {
  namespace py = pybind11;
  namespace mp = moc::python;

  py::class_<mp::Moc>(module, "initialise")
    .def(py::init<geo::RasterSpace const&, double, UINT4, calc::Field const*,
              calc::Field const*, calc::Field const*>())
    .def("transport", &mp::Moc::transport)
    .def("adjust", &mp::Moc::adjust)
    ;
}

