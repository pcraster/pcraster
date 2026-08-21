#include <nanobind/nanobind.h>
#include <nanobind/stl/shared_ptr.h>

#include "calc_field.h"

#include "Mldd.h"



NB_MODULE(_pcraster_mldd, module){
  namespace nb = nanobind;
  namespace mp = mldd::python;

  nb::class_<mp::Mldd>(module, "initialise")
    .def(nb::init<geo::RasterSpace const&>())
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
