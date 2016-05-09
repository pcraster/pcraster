#pragma once


namespace calc {
  class Field;
}


namespace pcraster_multicore {
namespace python {


calc::Field* _or(
         calc::Field* field_a,
         calc::Field* field_b);


} // namespace python
} // namespace pcraster_multicore

