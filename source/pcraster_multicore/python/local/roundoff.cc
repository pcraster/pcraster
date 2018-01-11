#include "pcraster_multicore/python/local/roundoff.h"

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
#include "pcraster_multicore/python/local/policies.h"

// Fern
#include "fern/algorithm/policy/policies.h"
#include "fern/algorithm/algebra/elementary/round.h"


namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {


calc::Field* roundoff(
         calc::Field* field){

  calc::Field* field_result = nullptr;

  if(field->isSpatial() == false){
    fa::SequentialExecutionPolicy sequential;

    const multicore_field::Nonspatial<REAL4> arg(field);

    field_result = new calc::NonSpatial(VS_S);
    multicore_field::Nonspatial<REAL4> result(field_result);

    using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<REAL4>>;
    InputNoDataPolicy input_no_data_policy{{arg}};

    NonspatialSetNoData<REAL4> output_no_data_policy(result);

    fa::algebra::round(input_no_data_policy,
      output_no_data_policy, sequential, arg, result);

    return result.getField();
  }
  else{
    const multicore_field::Spatial<REAL4> arg(field);

    field_result = new calc::Spatial(VS_S, calc::CRI_f, nr_cells());
    multicore_field::Spatial<REAL4> result(field_result);

    fa::ExecutionPolicy epol = execution_policy();

    using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<REAL4>>;
    InputNoDataPolicy input_no_data_policy{{arg}};

    SpatialSetNoData<REAL4> output_no_data_policy(result);

    fa::algebra::round(input_no_data_policy,
      output_no_data_policy, epol, arg, result);

    return result.getField();
  }
}




} // namespace detail



calc::Field* roundoff(
         calc::Field* field){

  assert_equal_location_attributes(*field);

  if(scalar_valuescale(*field)){
    return detail::roundoff(field);
  }
  else{
    throw std::runtime_error("argument type must be 'scalar'");
  }
}


} // namespace python
} // namespace pcraster_multicore
