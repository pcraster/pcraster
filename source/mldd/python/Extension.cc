#include <boost/python.hpp>

#include "calc_field.h"

#include "Mldd.h"



BOOST_PYTHON_MODULE(_pcraster_mldd){
  namespace bp = boost::python;
  namespace mp = mldd::python;

  bp::class_<mp::Mldd, boost::noncopyable>(
         "initialise",
         bp::init<geo::RasterSpace const&>())
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
