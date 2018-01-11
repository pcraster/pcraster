#include "pcraster_multicore/python/local/greater.h"

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
#include <fern/algorithm/algebra/elementary/greater.h>


namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {


template<class T>
calc::Field* greater_number_number(
         const multicore_field::Nonspatial<T>* arg1,
         const multicore_field::Nonspatial<T>* arg2,
         multicore_field::Nonspatial<UINT1>* res){

  fa::SequentialExecutionPolicy sequential;

  using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<T>,
        NonspatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};

  NonspatialSetNoData<UINT1> output_no_data_policy(*res);

  fa::algebra::greater(input_no_data_policy,
    output_no_data_policy, sequential, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* greater_field_field(
         fa::ExecutionPolicy epol,
         const multicore_field::Spatial<T>* arg1,
         const multicore_field::Spatial<T>* arg2,
         multicore_field::Spatial<UINT1>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<T>,
        SpatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};

  SpatialSetNoData<UINT1> output_no_data_policy(*res);

  fa::algebra::greater(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* greater_number_field(
         fern::algorithm::ExecutionPolicy epol,
         const multicore_field::Nonspatial<T>* arg1,
         const multicore_field::Spatial<T>* arg2,
         multicore_field::Spatial<UINT1>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<T>,
    SpatialDetectNoData<T>>;

  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};
  SpatialSetNoData<UINT1> output_no_data_policy(*res);

  fa::algebra::greater(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* greater_field_number(
         fern::algorithm::ExecutionPolicy epol,
         const multicore_field::Spatial<T>* arg1,
         const multicore_field::Nonspatial<T>* arg2,
         multicore_field::Spatial<UINT1>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<T>,
    NonspatialDetectNoData<T>>;

  InputNoDataPolicy input_no_data_policy{{*arg1},{*arg2}};
  SpatialSetNoData<UINT1> output_no_data_policy(*res);

  fa::algebra::greater(input_no_data_policy,
    output_no_data_policy, epol, *arg1, *arg2, *res);

  return res->getField();
}


template<class T>
calc::Field* greater(
         calc::Field* field_a,
         calc::Field* field_b){

  calc::Field* res_field = nullptr;

  if((field_a->isSpatial() == false) && (field_b->isSpatial() == false)){
    const multicore_field::Nonspatial<T> arg1(field_a);
    const multicore_field::Nonspatial<T> arg2(field_b);
    res_field = new calc::NonSpatial(VS_B);
    multicore_field::Nonspatial<UINT1> res(res_field);

    return detail::greater_number_number(&arg1, &arg2, &res);
  }


  res_field = new calc::Spatial(VS_B, calc::CRI_1, nr_cells());
  multicore_field::Spatial<UINT1> res(res_field);

  fa::ExecutionPolicy epol = execution_policy();

  if(field_b->isSpatial() == false){
    const multicore_field::Spatial<T> arg1(field_a);
    const multicore_field::Nonspatial<T> arg2(field_b);

    return detail::greater_field_number<T>(epol, &arg1, &arg2, &res);
  }
  else if(field_a->isSpatial() == false){
    const multicore_field::Nonspatial<T> arg1(field_a);
    const multicore_field::Spatial<T> arg2(field_b);

    return detail::greater_number_field<T>(epol, &arg1, &arg2, &res);
  }
  else{
    const multicore_field::Spatial<T> arg1(field_a);
    const multicore_field::Spatial<T> arg2(field_b);

    return detail::greater_field_field<T>(epol, &arg1, &arg2, &res);
  }
}


} // namespace detail


calc::Field* greater(
         calc::Field* field_a,
         calc::Field* field_b){

  assert_equal_location_attributes(*field_a);
  assert_equal_location_attributes(*field_b);
  //assert_equal_valuescale(*field_a, *field_b, "one operand");

  if(ordinal_valuescale(*field_a) == true){
    assert_ordinal_valuescale(*field_b, " second argument");
    return detail::greater<INT4>(field_a, field_b);
  }
  else if(scalar_valuescale(*field_a) == true){
    if(field_b->isSpatial() == false){
      field_b = to_scalar(field_b);
    }
    assert_scalar_valuescale(*field_b, " second argument");
    return detail::greater<REAL4>(field_a, field_b);
  }
  else{
    std::stringstream msg{};
    msg << "left operand is of type '" << field_b->vs() << "', legal type is either 'ordinal' or 'scalar'\n";
    throw std::runtime_error(msg.str());
  }
}


} // namespace python
} // namespace pcraster_multicore
