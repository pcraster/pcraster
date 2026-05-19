#pragma once


namespace calc {
  class Field;
}



namespace pcraster_multicore::python {


calc::Field* ifthen(
         calc::Field* condition,
         calc::Field* field_a);


} // namespace pcraster_multicore::python


