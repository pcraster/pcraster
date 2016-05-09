#pragma once

#include "calc_spatial.h"

#include "fern/algorithm/policy/execution_policy.h"
namespace calc {
  class Field;
}

namespace multicore_field{
  template<class T>
  class Spatial;
  template<class T>
  class Nonspatial;
}


namespace pcraster_multicore {
namespace python {

calc::Field* mul_number_number(
         const multicore_field::Nonspatial<REAL4>* arg1,
         const multicore_field::Nonspatial<REAL4>* arg2,
         multicore_field::Nonspatial<REAL4>* res);

calc::Field* mul_field_number(
         fern::algorithm::ExecutionPolicy epol,
         const multicore_field::Spatial<REAL4>* arg1,
         const multicore_field::Nonspatial<REAL4>* arg2,
         multicore_field::Spatial<REAL4>* res);

calc::Field* mul(
         calc::Field* field_a,
         calc::Field* field_b);



} // namespace python
} // namespace pcraster_multicore

