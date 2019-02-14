#pragma once


namespace pybind11 {
  class list;
}

namespace calc {
  class Field;
}



namespace pcraster_multicore {
namespace python {


calc::Field* cover(pybind11::list const& arguments);


} // namespace python
} // namespace pcraster_multicore

