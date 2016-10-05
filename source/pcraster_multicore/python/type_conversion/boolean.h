#pragma once


namespace calc {
  class Field;
}


namespace pcraster_multicore {
namespace python {


calc::Field* boolean(
         calc::Field* field);


calc::Field* safe_boolean(
         calc::Field* field);


} // namespace python
} // namespace pcraster_multicore

