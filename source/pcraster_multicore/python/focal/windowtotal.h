#pragma once


namespace calc {
  class Field;
}


namespace pcraster_multicore {
namespace python {


calc::Field* windowtotal(
         calc::Field * field,
         int window_length);


} // namespace python
} // namespace pcraster_multicore

