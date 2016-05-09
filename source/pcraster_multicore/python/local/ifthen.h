#pragma once


namespace calc {
  class Field;
}


namespace pcraster_multicore {
namespace python {


calc::Field* ifthen(
         calc::Field* condition,
         calc::Field* field_a);


} // namespace python
} // namespace pcraster_multicore

