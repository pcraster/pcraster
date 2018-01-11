#include "pcraster_multicore/python/type_conversion/ordinal.h"

#include <cstdlib>

// PCRaster
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "pcrtypes.h"

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
#include "fern/algorithm/core/unary_local_operation.h"



namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {


template<
    typename Value>
struct Algorithm
{

    inline void operator()(
        Value const& value,
        INT4& result) const
    {
        result = static_cast<INT4>(value);
    }

};


template<
    typename InputNoDataPolicy,
    typename OutputNoDataPolicy,
    typename ExecutionPolicy,
    typename Value,
    typename Result
>
void cast_to_ordinal(
    InputNoDataPolicy const& input_no_data_policy,
    OutputNoDataPolicy& output_no_data_policy,
    ExecutionPolicy& execution_policy,
    Value const& value,
    Result& result)
{
    fa::unary_local_operation<Algorithm,
        fa::unary::DiscardDomainErrors, fa::unary::DiscardRangeErrors>(
            input_no_data_policy, output_no_data_policy,
            execution_policy,
            value, result);
}


} // namespace detail



calc::Field* ordinal(
         calc::Field* field){

  assert_equal_location_attributes(*field);

  if(ordinal_valuescale(*field)){
    calc::Field* res_field = field->createClone();
    return res_field;
  }

  if(directional_valuescale(*field)){
    throw std::runtime_error("operation not implemented for type 'directional'");
  }

  calc::Field* res_field = nullptr;

  CSF_CR cell_representation = field->cr();

  if(field->isSpatial() == false){
    fa::SequentialExecutionPolicy sequential;

    res_field = new calc::NonSpatial(VS_O);
    multicore_field::Nonspatial<INT4> result(res_field);
    NonspatialSetNoData<INT4> output_no_data_policy(result);

    switch(cell_representation) {
      case CR_UINT1:{
        const multicore_field::Nonspatial<UINT1> arg(field);

        using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<UINT1>>;
        InputNoDataPolicy input_no_data_policy{{arg}};

        detail::cast_to_ordinal(input_no_data_policy, output_no_data_policy, sequential, arg, result);
        break;
      }
      case CR_INT4:{
        const multicore_field::Nonspatial<INT4> arg(field);

        using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<INT4>>;
        InputNoDataPolicy input_no_data_policy{{arg}};

        detail::cast_to_ordinal(input_no_data_policy, output_no_data_policy, sequential, arg, result);
        break;
      }
      case CR_REAL4:{
        const multicore_field::Nonspatial<REAL4> arg(field);

        using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<REAL4>>;
        InputNoDataPolicy input_no_data_policy{{arg}};

        detail::cast_to_ordinal(input_no_data_policy, output_no_data_policy, sequential, arg, result);
        break;
      }
      default: {
        throw std::runtime_error("internal error: unable to perform nonspatial operation");
      }
    }

    return result.getField();
  }

  res_field = new calc::Spatial(VS_O, calc::CRI_4, nr_cells());
  multicore_field::Spatial<INT4> result(res_field);
  SpatialSetNoData<INT4> output_no_data_policy(result);


  switch(cell_representation) {
    case CR_UINT1:{
      fa::ExecutionPolicy epol = execution_policy();
      const multicore_field::Spatial<UINT1> arg(field);

      using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<UINT1>>;
      InputNoDataPolicy input_no_data_policy{{arg}};

      detail::cast_to_ordinal(input_no_data_policy, output_no_data_policy, epol, arg, result);
      break;
    }
    case CR_INT4:{
      fa::ExecutionPolicy epol = execution_policy();
      const multicore_field::Spatial<INT4> arg(field);

      using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<INT4>>;
      InputNoDataPolicy input_no_data_policy{{arg}};

      detail::cast_to_ordinal(input_no_data_policy, output_no_data_policy, epol, arg, result);
      break;
    }
    case CR_REAL4:{
      fa::ExecutionPolicy epol = execution_policy();
      const multicore_field::Spatial<REAL4> arg(field);

      using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<REAL4>>;
      InputNoDataPolicy input_no_data_policy{{arg}};

      detail::cast_to_ordinal(input_no_data_policy, output_no_data_policy, epol, arg, result);
      break;
    }
    default: {
      throw std::runtime_error("internal error: unable to perform operation");
    }
  }

  return result.getField();
}


} // namespace python
} // namespace pcraster_multicore

