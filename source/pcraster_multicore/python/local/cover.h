#pragma once


namespace pybind11 {
  class list;
}

namespace calc {
  class Field;
}




namespace pcraster_multicore::python {


calc::Field* cover(pybind11::list const& arguments);


} // namespace pcraster_multicore::python


