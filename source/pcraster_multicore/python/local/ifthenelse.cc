#include "pcraster_multicore/python/local/ifthenelse.h"

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
calc::Field* ifthenelse_number_n_n(
         const multicore_field::Nonspatial<UINT1>* condition,
         const multicore_field::Nonspatial<T>* arg1,
         const multicore_field::Nonspatial<T>* arg2,
         multicore_field::Nonspatial<T>* res){

  fa::SequentialExecutionPolicy sequential;

  using InputNoDataPolicy = fa::InputNoDataPolicies<
        NonspatialDetectNoData<UINT1>,
        NonspatialDetectNoData<T>,
        NonspatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*condition}, {*arg1}, {*arg2}};

  NonspatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, sequential, *condition, *arg1, *arg2, *res);

  return res->getField();
}

template<class T>
calc::Field* ifthenelse_number_n_s(
         const multicore_field::Nonspatial<UINT1>* condition,
         const multicore_field::Nonspatial<T>* arg1,
         const multicore_field::Spatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  fa::SequentialExecutionPolicy sequential;

  using InputNoDataPolicy = fa::InputNoDataPolicies<
        NonspatialDetectNoData<UINT1>,
        NonspatialDetectNoData<T>,
        SpatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*condition}, {*arg1}, {*arg2}};

  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, sequential, *condition, *arg1, *arg2, *res);

  return res->getField();
}

template<class T>
calc::Field* ifthenelse_number_s_n(
         const multicore_field::Nonspatial<UINT1>* condition,
         const multicore_field::Spatial<T>* arg1,
         const multicore_field::Nonspatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  fa::SequentialExecutionPolicy sequential;

  using InputNoDataPolicy = fa::InputNoDataPolicies<
         NonspatialDetectNoData<UINT1>,
         SpatialDetectNoData<T>,
         NonspatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*condition}, {*arg1}, {*arg2}};

  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, sequential, *condition, *arg1, *arg2, *res);

  return res->getField();
}

template<class T>
calc::Field* ifthenelse_number_s_s(
         const multicore_field::Nonspatial<UINT1>* condition,
         const multicore_field::Spatial<T>* arg1,
         const multicore_field::Spatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  fa::SequentialExecutionPolicy sequential;

  using InputNoDataPolicy = fa::InputNoDataPolicies<
         NonspatialDetectNoData<UINT1>,
         SpatialDetectNoData<T>,
         SpatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*condition}, {*arg1}, {*arg2}};

  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, sequential, *condition, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* ifthenelse_field_n_n(
         fa::ExecutionPolicy epol,
         const multicore_field::Spatial<UINT1>* condition,
         const multicore_field::Nonspatial<T>* arg1,
         const multicore_field::Nonspatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<
         SpatialDetectNoData<UINT1>,
         NonspatialDetectNoData<T>,
         NonspatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*condition}, {*arg1}, {*arg2}};

  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, epol, *condition, *arg1, *arg2, *res);

  return res->getField();
}

template<class T>
calc::Field* ifthenelse_field_s_n(
         fa::ExecutionPolicy epol,
         const multicore_field::Spatial<UINT1>* condition,
         const multicore_field::Spatial<T>* arg1,
         const multicore_field::Nonspatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<
         SpatialDetectNoData<UINT1>,
         SpatialDetectNoData<T>,
         NonspatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*condition}, {*arg1}, {*arg2}};

  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, epol, *condition, *arg1, *arg2, *res);

  return res->getField();
}

template<class T>
calc::Field* ifthenelse_field_n_s(
         fa::ExecutionPolicy epol,
         const multicore_field::Spatial<UINT1>* condition,
         const multicore_field::Nonspatial<T>* arg1,
         const multicore_field::Spatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<
         SpatialDetectNoData<UINT1>,
         NonspatialDetectNoData<T>,
         SpatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*condition}, {*arg1}, {*arg2}};

  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, epol, *condition, *arg1, *arg2, *res);

  return res->getField();
}

template<class T>
calc::Field* ifthenelse_field_s_s(
         fa::ExecutionPolicy epol,
         const multicore_field::Spatial<UINT1>* condition,
         const multicore_field::Spatial<T>* arg1,
         const multicore_field::Spatial<T>* arg2,
         multicore_field::Spatial<T>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<
         SpatialDetectNoData<UINT1>,
         SpatialDetectNoData<T>,
         SpatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*condition}, {*arg1}, {*arg2}};

  SpatialSetNoData<T> output_no_data_policy(*res);

  fa::core::if_(input_no_data_policy,
    output_no_data_policy, epol, *condition, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* ifthenelse(
         calc::Field* condition,
         calc::Field* field_a,
         calc::Field* field_b,
         VS result_vs,
         calc::CRIndex result_cri){

  calc::Field* res_field = nullptr;

  if(condition->isSpatial()){
    const multicore_field::Spatial<UINT1> arg1(condition);
    res_field = new calc::Spatial(result_vs, result_cri, nr_cells());
    multicore_field::Spatial<T> res(res_field);
    fa::ExecutionPolicy epol = execution_policy();

    if((field_a->isSpatial()) && (field_b->isSpatial())){
      const multicore_field::Spatial<T> arg2(field_a);
      const multicore_field::Spatial<T> arg3(field_b);
      return detail::ifthenelse_field_s_s(epol, &arg1, &arg2, &arg3, &res);
    }
    else if(field_a->isSpatial()){
      const multicore_field::Spatial<T> arg2(field_a);
      const multicore_field::Nonspatial<T> arg3(field_b);
      return detail::ifthenelse_field_s_n(epol, &arg1, &arg2, &arg3, &res);

    }
    else if(field_b->isSpatial()){
      const multicore_field::Nonspatial<T> arg2(field_a);
      const multicore_field::Spatial<T> arg3(field_b);
      return detail::ifthenelse_field_n_s(epol, &arg1, &arg2, &arg3, &res);
    }
    else{
      const multicore_field::Nonspatial<T> arg2(field_a);
      const multicore_field::Nonspatial<T> arg3(field_b);
      return detail::ifthenelse_field_n_n(epol, &arg1, &arg2, &arg3, &res);
    }
  }
  else if(condition->isSpatial() == false){
    const multicore_field::Nonspatial<UINT1> arg1(condition);

    if((field_a->isSpatial()) && (field_b->isSpatial())){
      const multicore_field::Spatial<T> arg2(field_a);
      const multicore_field::Spatial<T> arg3(field_b);
      res_field = new calc::Spatial(result_vs, result_cri, nr_cells());
      multicore_field::Spatial<T> res(res_field);
      return detail::ifthenelse_number_s_s(&arg1, &arg2, &arg3, &res);
    }
    else if(field_a->isSpatial()){
      const multicore_field::Spatial<T> arg2(field_a);
      const multicore_field::Nonspatial<T> arg3(field_b);
      res_field = new calc::Spatial(result_vs, result_cri, nr_cells());
      multicore_field::Spatial<T> res(res_field);
      return detail::ifthenelse_number_s_n(&arg1, &arg2, &arg3, &res);

    }
    else if(field_b->isSpatial()){
      const multicore_field::Nonspatial<T> arg2(field_a);
      const multicore_field::Spatial<T> arg3(field_b);
      res_field = new calc::Spatial(result_vs, result_cri, nr_cells());
      multicore_field::Spatial<T> res(res_field);
      return detail::ifthenelse_number_n_s(&arg1, &arg2, &arg3, &res);
    }
    else{
      const multicore_field::Nonspatial<T> arg2(field_a);
      const multicore_field::Nonspatial<T> arg3(field_b);
      res_field = new calc::NonSpatial(result_vs);
      multicore_field::Nonspatial<T> res(res_field);
      return detail::ifthenelse_number_n_n(&arg1, &arg2, &arg3, &res);
    }
  }
  else{
    throw std::runtime_error("internal error: unable to perform operation");
  }
}


} // namespace detail


calc::Field* ifthenelse(
         calc::Field* condition,
         calc::Field* field_a,
         calc::Field* field_b){

  // type casting of nonspatials
  // in case of int we can cast to scalar iff other argument is scalar
  if(field_a->isSpatial() == false){
    if(scalar_valuescale(*field_b)){
      field_a = to_scalar(field_a);
    }
  }
  if(field_b->isSpatial() == false){
    if(scalar_valuescale(*field_a)){
      field_b = to_scalar(field_b);
    }
  }

  assert_equal_location_attributes(*condition);
  assert_equal_location_attributes(*field_a);
  assert_equal_location_attributes(*field_b);
  assert_boolean_valuescale(*condition, "first argument");
  assert_equal_valuescale(*field_a, *field_b, "one argument");

  VS result_vs = field_a->vs();
  calc::CRIndex result_cri = field_a->cri();

  if(boolean_valuescale(*field_a) || ldd_valuescale(*field_a)){
    return detail::ifthenelse<UINT1>(condition, field_a, field_b, result_vs, result_cri);
  }
  else if(nominal_valuescale(*field_a) || ordinal_valuescale(*field_a)){
    return detail::ifthenelse<INT4>(condition, field_a, field_b, result_vs, result_cri);
  }
  else if(scalar_valuescale(*field_a) || directional_valuescale(*field_a)){
    return detail::ifthenelse<REAL4>(condition, field_a, field_b, result_vs, result_cri);
  }

  assert(false);
  return nullptr;
}


} // namespace python
} // namespace pcraster_multicore

