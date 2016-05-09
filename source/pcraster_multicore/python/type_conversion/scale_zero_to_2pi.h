#pragma once

#include "fern/algorithm/policy/policies.h"
#include "fern/algorithm/core/unary_local_operation.h"


namespace calc {
  class Field;
}


namespace pcraster_multicore {
namespace python {
namespace detail {


template<
    typename Value>
struct Algorithm
{

    template<
        typename Result>
    inline void operator()(
        Value const& value,
        Result& result) const
    {
        if(value >= 0){
          result = value;
        }
        else{
          result = 2.0 * M_PI + value; // i.e. + (-value)
        }
    }

};


template<
    typename InputNoDataPolicy,
    typename OutputNoDataPolicy,
    typename ExecutionPolicy,
    typename Value,
    typename Result
>
void scale_zero_to_2pi(
    InputNoDataPolicy const& input_no_data_policy,
    OutputNoDataPolicy& output_no_data_policy,
    ExecutionPolicy& execution_policy,
    Value const& value,
    Result& result)
{
    fern::algorithm::unary_local_operation<Algorithm,
        fern::algorithm::unary::DiscardDomainErrors, fern::algorithm::unary::DiscardRangeErrors>(
            input_no_data_policy, output_no_data_policy,
            execution_policy,
            value, result);
}

} // namespace detail



template<
    typename InputNoDataPolicy,
    typename OutputNoDataPolicy,
    typename ExecutionPolicy,
    typename Value,
    typename Result
>
void scale_zero_to_2pi(
    InputNoDataPolicy const& input_no_data_policy,
    OutputNoDataPolicy& output_no_data_policy,
    ExecutionPolicy& execution_policy,
    Value const& value,
    Result& result){

  detail::scale_zero_to_2pi(input_no_data_policy,
        output_no_data_policy, execution_policy, value, result);
}


} // namespace python
} // namespace pcraster_multicore

