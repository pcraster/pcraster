#include <pybind11/pybind11.h>

#include "calc_field.h"

#include "Mldd.h"



PYBIND11_MODULE(_pcraster_mldd, module){
  namespace mp = mldd::python;

  pybind11::class_<mp::Mldd>(module, "initialise")
    .def(pybind11::init<geo::RasterSpace const&>())
    .def("setDem", &mp::Mldd::setDem)
    .def("getDem", &mp::Mldd::getDem)
    .def("addStream", &mp::Mldd::addStream)
    .def("setStream", &mp::Mldd::setStream)
    .def("removeStream", &mp::Mldd::removeStream)
    .def("getStream", &mp::Mldd::getStream)
    .def("getWeight", &mp::Mldd::getWeight)
    .def("upstream", &mp::Mldd::upstream)
    .def("accuflux", &mp::Mldd::accuflux)
    .def("diffuse", &mp::Mldd::diffuse)
    ;
}
