#pragma once


namespace calc {
  class Field;
}



namespace pcraster_multicore::python {


calc::Field* boolean(
         calc::Field* field);


calc::Field* safe_boolean(
         calc::Field* field);


} // namespace pcraster_multicore::python


