#pragma once
#include <cstddef>

namespace calc {
  class Field;
}



namespace pcraster_multicore::python {


calc::Field* windowtotal(
         calc::Field * field,
         size_t radius);


calc::Field* windowaverage(
         calc::Field * field,
         size_t radius);


} // namespace pcraster_multicore::python


