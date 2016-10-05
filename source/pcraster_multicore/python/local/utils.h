#pragma once
#include <cassert>
#include "calc_spatial.h"
#include "pcrdatatype.h"

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


size_t nr_rows();
size_t nr_cols();
size_t nr_cells();

bool boolean_valuescale(const calc::Field& aField);
bool ldd_valuescale(const calc::Field& aField);
bool nominal_valuescale(const calc::Field& aField);
bool ordinal_valuescale(const calc::Field& aField);
bool scalar_valuescale(const calc::Field& aField);
bool directional_valuescale(const calc::Field& aField);




void assert_boolean_valuescale(const calc::Field& aField, const std::string& msg);
//void assert_ldd_valuescale(const calc::Field& aField, const std::string& msg);
void assert_nominal_valuescale(const calc::Field& aField, const std::string& msg);
void assert_ordinal_valuescale(const calc::Field& aField, const std::string& msg);
void assert_scalar_valuescale(const calc::Field& aField, const std::string& msg);
//void assert_directional_valuescale(const calc::Field& aField, const std::string& msg);


calc::Field* degrees_to_radians(const multicore_field::Nonspatial<REAL4>* aField,  multicore_field::Nonspatial<REAL4>* result);

calc::Field* degrees_to_radians(const multicore_field::Spatial<REAL4>* aField,  multicore_field::Spatial<REAL4>* result);



bool global_option_degrees();
bool global_option_unittrue();

void assert_equal_valuescale(const calc::Field& field_a, const calc::Field& field_b, const std::string& msg);

/// simple test if location attributes of field match the clone ones
void assert_equal_location_attributes(const calc::Field& field_a);

calc::Field* to_scalar(calc::Field* nonspatial);
calc::Field* to_boolean(calc::Field* nonspatial);
calc::Field* to_ordinal(calc::Field* nonspatial);

} // namespace python
} // namespace pcraster_multicore

