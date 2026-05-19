#pragma once


namespace calc {
  class Field;
}



namespace pcraster_multicore::python {


calc::Field* less(
         calc::Field* field_a,
         calc::Field* field_b);


} // namespace pcraster_multicore::python


