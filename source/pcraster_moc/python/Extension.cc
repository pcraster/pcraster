#include "calc_spatial.h"
#include "Moc.h"



PYBIND11_MODULE(_pcraster_moc, module) {
  namespace pb = pybind11;
  namespace mp = moc::python;

  pb::class_<mp::Moc, pb::smart_holder>(module, "initialise")
    .def(pb::init<geo::RasterSpace const&, double, UINT4, calc::Field const*,
              calc::Field const*, calc::Field const*>())
    .def("transport", &mp::Moc::transport)
    .def("adjust", &mp::Moc::adjust)
    ;
}

