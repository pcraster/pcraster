// External headers.
#ifndef INCLUDED_BOOST_PYTHON
#include <boost/python.hpp>
#define INCLUDED_BOOST_PYTHON
#endif

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



BOOST_PYTHON_MODULE(_pcraster_moc) {
  namespace bp = boost::python;
  namespace mp = moc::python;

  bp::class_<mp::Moc, boost::noncopyable>(
         "initialise",
         bp::init<geo::RasterSpace const&, double, UINT4, calc::Field const*,
              calc::Field const*, calc::Field const*>())
    .def("transport", &mp::Moc::transport)
    .def("adjust", &mp::Moc::adjust)
    ;
}

