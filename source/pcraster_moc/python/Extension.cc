#include "calc_spatial.h"
#include "Moc.h"



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

