#include "pcraster_multicore/python/local/ifthen.h"

// PCRaster
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "calc_cr.h"

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
#include <fern/algorithm/core/if.h>


namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {


template<class T>
calc::Field* ifthen_number_number(
         const multicore_field::Nonspatial<UINT1>* arg1,
         const multicore_field::Nonspatial<T>* arg2,
         multicore_field::Nonspatial<T>* res){

  fa::SequentialExecutionPolicy sequential;

  using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<UINT1>,
        NonspatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};

  NonspatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, sequential, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* ifthen_field_field(
         fa::ExecutionPolicy epol,
         const multicore_field::Spatial<UINT1>* arg1,
         const multicore_field::Spatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<UINT1>,
        SpatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};

  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* ifthen_number_field(
         fern::algorithm::ExecutionPolicy epol,
         const multicore_field::Nonspatial<UINT1>* arg1,
         const multicore_field::Spatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<UINT1>,
    SpatialDetectNoData<T>>;

  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};
  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* ifthen_field_number(
         fern::algorithm::ExecutionPolicy epol,
         const multicore_field::Spatial<UINT1>* arg1,
         const multicore_field::Nonspatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<UINT1>,
    NonspatialDetectNoData<T>>;

  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};
  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* ifthen(
         calc::Field* field_a,
         calc::Field* field_b,
         VS result_vs,
         calc::CRIndex result_cri){

  calc::Field* res_field = nullptr;

  if((field_a->isSpatial() == false) && (field_b->isSpatial() == false)){
    const multicore_field::Nonspatial<UINT1> arg1(field_a);
    const multicore_field::Nonspatial<T> arg2(field_b);
    res_field = new calc::NonSpatial(result_vs);
    multicore_field::Nonspatial<T> res(res_field);
    return detail::ifthen_number_number(&arg1, &arg2, &res);
  }

  res_field = new calc::Spatial(result_vs, result_cri, nr_cells());
  multicore_field::Spatial<T> res(res_field);

  fa::ExecutionPolicy epol = execution_policy();

  if(field_b->isSpatial() == false){
    const multicore_field::Spatial<UINT1> arg1(field_a);
    const multicore_field::Nonspatial<T> arg2(field_b);
    return detail::ifthen_field_number<T>(epol, &arg1, &arg2, &res);
  }
  else if(field_a->isSpatial() == false){
    const multicore_field::Nonspatial<UINT1> arg1(field_a);
    const multicore_field::Spatial<T> arg2(field_b);
    return detail::ifthen_number_field<T>(epol, &arg1, &arg2, &res);
  }
  else{
    const multicore_field::Spatial<UINT1> arg1(field_a);
    const multicore_field::Spatial<T> arg2(field_b);
    return detail::ifthen_field_field<T>(epol, &arg1, &arg2, &res);
  }
}


} // namespace detail


calc::Field* ifthen(
         calc::Field* condition,
         calc::Field* field_a){

  assert_equal_location_attributes(*condition);
  assert_equal_location_attributes(*field_a);
  assert_boolean_valuescale(*condition, "first argument");

  VS result_vs = field_a->vs();
  calc::CRIndex result_cri = field_a->cri();

  if(boolean_valuescale(*field_a) || ldd_valuescale(*field_a)){
    return detail::ifthen<UINT1>(condition, field_a, result_vs, result_cri);
  }
  else if(nominal_valuescale(*field_a) || ordinal_valuescale(*field_a)){
    return detail::ifthen<INT4>(condition, field_a, result_vs, result_cri);
  }
  else if(scalar_valuescale(*field_a) || directional_valuescale(*field_a)){
    return detail::ifthen<REAL4>(condition, field_a, result_vs, result_cri);
  }

  assert(false);
  return nullptr;
}


} // namespace python
} // namespace pcraster_multicore

