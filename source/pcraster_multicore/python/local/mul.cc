#include "pcraster_multicore/python/local/mul.h"

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
#include "fern/algorithm/algebra/elementary/multiply.h"


namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {





calc::Field* mul_field_field(
         fa::ExecutionPolicy epol,
         const multicore_field::Spatial<REAL4>* arg1,
         const multicore_field::Spatial<REAL4>* arg2,
         multicore_field::Spatial<REAL4>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<REAL4>,
        SpatialDetectNoData<REAL4>>;
  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};

  SpatialSetNoData<REAL4> output_no_data_policy(*res);

  fa::algebra::multiply<fa::multiply::OutOfRangePolicy>(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}





} // namespace detail


calc::Field* mul_number_number(
         const multicore_field::Nonspatial<REAL4>* arg1,
         const multicore_field::Nonspatial<REAL4>* arg2,
         multicore_field::Nonspatial<REAL4>* res){

  fa::SequentialExecutionPolicy sequential;

  using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<REAL4>,
        NonspatialDetectNoData<REAL4>>;
  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};

  NonspatialSetNoData<REAL4> output_no_data_policy(*res);

  fa::algebra::multiply<fa::multiply::OutOfRangePolicy>(input_no_data_policy,
    output_no_data_policy, sequential, *arg1, *arg2, *res);

  return res->getField();
}

calc::Field* mul_field_number(
         fern::algorithm::ExecutionPolicy epol,
         const multicore_field::Spatial<REAL4>* arg1,
         const multicore_field::Nonspatial<REAL4>* arg2,
         multicore_field::Spatial<REAL4>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<REAL4>,
        NonspatialDetectNoData<REAL4>>;

  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};
  SpatialSetNoData<REAL4> output_no_data_policy(*res);

  fa::algebra::multiply<fa::multiply::OutOfRangePolicy>(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}


calc::Field* mul(
         calc::Field* field_a,
         calc::Field* field_b){

  // type casting of nonspatials
  if(field_a->isSpatial() == false){
    field_a = to_scalar(field_a);
  }
  if(field_b->isSpatial() == false){
    field_b = to_scalar(field_b);
  }

  assert_equal_location_attributes(*field_a);
  assert_equal_location_attributes(*field_b);
  assert_scalar_valuescale(*field_a, "left operand");
  assert_scalar_valuescale(*field_b, "right operand");

  calc::Field* res_field = nullptr;

  if((field_a->isSpatial() == false) && (field_b->isSpatial() == false)){
    const multicore_field::Nonspatial<REAL4> arg1(field_a);
    const multicore_field::Nonspatial<REAL4> arg2(field_b);
    res_field = new calc::NonSpatial(VS_S);
    multicore_field::Nonspatial<REAL4> res(res_field);

    return mul_number_number(&arg1, &arg2, &res);
  }

  res_field = new calc::Spatial(VS_S, calc::CRI_f, nr_cells());
  multicore_field::Spatial<REAL4> res(res_field);

  fa::ExecutionPolicy epol = execution_policy();

  if(field_b->isSpatial() == false){
    const multicore_field::Spatial<REAL4> arg1(field_a);
    const multicore_field::Nonspatial<REAL4> arg2(field_b);
    return mul_field_number(epol, &arg1, &arg2, &res);
  }
  else if(field_a->isSpatial() == false){
    const multicore_field::Nonspatial<REAL4> arg1(field_a);
    const multicore_field::Spatial<REAL4> arg2(field_b);
    return mul_field_number(epol, &arg2, &arg1, &res);
  }
  else{
    const multicore_field::Spatial<REAL4> arg1(field_a);
    const multicore_field::Spatial<REAL4> arg2(field_b);
    return detail::mul_field_field(epol, &arg1, &arg2, &res);
  }
}


} // namespace python
} // namespace pcraster_multicore

