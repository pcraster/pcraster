#include "pcraster_multicore/python/local/and.h"

// PCRaster
#include "pcraster_model_engine/calc_spatial.h"
#include "pcraster_model_engine/calc_nonspatial.h"

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
#include "fern/algorithm/algebra/boole/and.h"




namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {

calc::Field* and_number_number(
         const multicore_field::Nonspatial<UINT1>* arg1,
         const multicore_field::Nonspatial<UINT1>* arg2,
         multicore_field::Nonspatial<UINT1>* res){

  fa::SequentialExecutionPolicy sequential;

  using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<UINT1>,
        NonspatialDetectNoData<UINT1>>;
  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};

  NonspatialSetNoData<UINT1> output_no_data_policy(*res);

  fa::algebra::and_(input_no_data_policy,
    output_no_data_policy, sequential, *arg1, *arg2, *res);

  return res->getField();
}


calc::Field* and_field_field(
         fa::ExecutionPolicy epol,
         const multicore_field::Spatial<UINT1>* arg1,
         const multicore_field::Spatial<UINT1>* arg2,
         multicore_field::Spatial<UINT1>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<UINT1>,
        SpatialDetectNoData<UINT1>>;
  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};

  SpatialSetNoData<UINT1> output_no_data_policy(*res);

  fa::algebra::and_(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}


calc::Field* and_number_field(
         fern::algorithm::ExecutionPolicy epol,
         const multicore_field::Nonspatial<UINT1>* arg1,
         const multicore_field::Spatial<UINT1>* arg2,
         multicore_field::Spatial<UINT1>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<UINT1>,
    SpatialDetectNoData<UINT1>>;

  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};
  SpatialSetNoData<UINT1> output_no_data_policy(*res);

  fa::algebra::and_(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}

} // namespace detail


calc::Field* _and(
         calc::Field* field_a,
         calc::Field* field_b){

  assert_equal_location_attributes(*field_a);
  assert_equal_location_attributes(*field_b);
  assert_boolean_valuescale(*field_a, "left operand");
  assert_boolean_valuescale(*field_b, "right operand");

  calc::Field* res_field = nullptr;

  if((field_a->isSpatial() == false) && (field_b->isSpatial() == false)){
    const multicore_field::Nonspatial<UINT1> arg1(field_a);
    const multicore_field::Nonspatial<UINT1> arg2(field_b);
    res_field = new calc::NonSpatial(VS_B);
    multicore_field::Nonspatial<UINT1> res(res_field);

    return detail::and_number_number(&arg1, &arg2, &res);
  }

  res_field = new calc::Spatial(VS_B, calc::CRI_1, nr_cells());
  multicore_field::Spatial<UINT1> res(res_field);

  fa::ExecutionPolicy epol = execution_policy();

  if(field_b->isSpatial() == false){
    const multicore_field::Spatial<UINT1> arg1(field_a);
    const multicore_field::Nonspatial<UINT1> arg2(field_b);
    return detail::and_number_field(epol, &arg2, &arg1, &res);
  }
  else if(field_a->isSpatial() == false){
    const multicore_field::Nonspatial<UINT1> arg1(field_a);
    const multicore_field::Spatial<UINT1> arg2(field_b);
    return detail::and_number_field(epol, &arg1, &arg2, &res);
  }
  else{
    const multicore_field::Spatial<UINT1> arg1(field_a);
    const multicore_field::Spatial<UINT1> arg2(field_b);
    return detail::and_field_field(epol, &arg1, &arg2, &res);
  }
}


} // namespace python
} // namespace pcraster_multicore

