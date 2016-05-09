#include "pcraster_multicore/python/local/fac.h"
#include "pcraster_multicore/python/local/utils.h"

// PCRaster
#include "calc_spatial.h"
#include "calc_nonspatial.h"


// Field wrapper
#include "pcraster_multicore/wrapper/datatype_customization_points/multicore_spatial.h"
#include "pcraster_multicore/wrapper/datatype_traits/multicore_spatial.h"
#include "pcraster_multicore/wrapper/argument_traits/multicore_spatial.h"

#include "pcraster_multicore/wrapper/datatype_customization_points/multicore_nonspatial.h"
#include "pcraster_multicore/wrapper/datatype_traits/multicore_nonspatial.h"
#include "pcraster_multicore/wrapper/argument_traits/multicore_nonspatial.h"

#include "pcraster_multicore/python/execution_policy.h"
#include "pcraster_multicore/python/local/utils.h"


// Fern
#include "fern/algorithm/policy/policies.h"
#include "fern/algorithm/algebra/elementary/factorial.h"



namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {

} // namespace detail


calc::Field* fac(
         calc::Field* field){

  assert_equal_location_attributes(*field);
  assert_scalar_valuescale(*field, "argument");

  if(field->isSpatial() == false){
    throw std::runtime_error("argument is non-spatial, only spatial is allowed\n");
  }

  calc::Field* field_result = nullptr;

  const multicore_field::Spatial<REAL4> arg(field);

  field_result = new calc::Spatial(VS_S, calc::CRI_f, nr_cells());
  multicore_field::Spatial<REAL4> result(field_result);

  fa::ExecutionPolicy epol = execution_policy();

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<REAL4>>;
  InputNoDataPolicy input_no_data_policy{{arg}};

  SpatialSetNoData<REAL4> output_no_data_policy(result);

  fa::algebra::factorial<fa::factorial::OutOfDomainPolicy, fa::factorial::OutOfRangePolicy>
    (input_no_data_policy, output_no_data_policy, epol, arg, result);


  return result.getField();
}


} // namespace python
} // namespace pcraster_multicore

