#pragma once
#include <cstddef>

namespace calc {
  class Field;
}


namespace pcraster_multicore {
namespace python {


calc::Field* windowtotal(
         calc::Field * field,
         size_t radius);


calc::Field* windowaverage(
         calc::Field * field,
         size_t radius);


} // namespace python
} // namespace pcraster_multicore

