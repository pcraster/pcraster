#include "pcraster_multicore/python/local/sin.h"

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
#include "fern/algorithm/trigonometry/sin.h"


namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {


calc::Field* sin_scalar(
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

    if(global_option_degrees()){
      // fist convert input to radians...
      multicore_field::Nonspatial<REAL4> deg_rad(degrees_to_radians(&arg, &result));

      fa::trigonometry::sin<fa::sin::OutOfDomainPolicy>(input_no_data_policy,
        output_no_data_policy, sequential, deg_rad, result);
    }
    else{
      fa::trigonometry::sin<fa::sin::OutOfDomainPolicy>(input_no_data_policy,
        output_no_data_policy, sequential, arg, result);
    }
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

    if(global_option_degrees()){
      // fist convert input to radians...
      multicore_field::Spatial<REAL4> deg_rad(degrees_to_radians(&arg, &result));

      fa::trigonometry::sin<fa::sin::OutOfDomainPolicy>(input_no_data_policy,
        output_no_data_policy, epol, deg_rad, result);
    }
    else{
      fa::trigonometry::sin<fa::sin::OutOfDomainPolicy>(input_no_data_policy,
        output_no_data_policy, epol, arg, result);
    }

    return result.getField();
  }
}


calc::Field* sin_directional(
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

    if(global_option_degrees()){
      // fist convert input to radians...
      multicore_field::Nonspatial<REAL4> deg_rad(degrees_to_radians(&arg, &result));

      fa::trigonometry::sin<OutOfDirectionalDomainPolicy>(input_no_data_policy,
        output_no_data_policy, sequential, deg_rad, result);
    }
    else{
      fa::trigonometry::sin<OutOfDirectionalDomainPolicy>(input_no_data_policy,
        output_no_data_policy, sequential, arg, result);
    }
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

    if(global_option_degrees()){
      // fist convert input to radians...
      //multicore_field::Spatial<REAL4> deg_rad(degrees_to_radians(&arg, &result));

      //fa::trigonometry::sin<OutOfDirectionalDomainPolicy>(input_no_data_policy,
      //  output_no_data_policy, epol, deg_rad, result);

      // no conversion only due to match the PCRaster behaviour...
      fa::trigonometry::sin<OutOfDirectionalDomainPolicy>(input_no_data_policy,
        output_no_data_policy, epol, arg, result);
    }
    else{
      fa::trigonometry::sin<OutOfDirectionalDomainPolicy>(input_no_data_policy,
        output_no_data_policy, epol, arg, result);
    }

    return result.getField();
  }
}


} // namespace detail



calc::Field* sin(
         calc::Field* field){

  assert_equal_location_attributes(*field);

  if(scalar_valuescale(*field)){
    return detail::sin_scalar(field);
  }
  else if(directional_valuescale(*field)){
    return detail::sin_directional(field);
  }
  else{
    throw std::runtime_error("argument type must be either 'scalar' or 'directional' \n");
  }
}


} // namespace python
} // namespace pcraster_multicore

