#include "pcraster_multicore/python/focal/windowtotal.h"

// PCRaster
#include "calc_spatial.h"

// Field wrapper
#include "pcraster_multicore/wrapper/datatype_customization_points/multicore_spatial.h"
#include "pcraster_multicore/wrapper/datatype_traits/multicore_spatial.h"
#include "pcraster_multicore/wrapper/argument_customization_points/multicore_spatial.h"
#include "pcraster_multicore/wrapper/argument_traits/multicore_spatial.h"

#include "pcraster_multicore/python/execution_policy.h"
#include "pcraster_multicore/python/local/utils.h"

// Fern
#include <fern/algorithm/convolution/neighborhood/kernel.h>
#include "fern/algorithm/convolution/convolve.h"
#include "fern/algorithm/convolution/policies.h"



namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {


template<typename DivideOrNot>
calc::Field* windowtot_avg(
         calc::Field * field,
         size_t radius){

  if(field->isSpatial() == false){
    throw std::runtime_error("argument is non-spatial, only spatial is allowed\n");
  }

  assert_equal_location_attributes(*field);
  assert_scalar_valuescale(*field, "argument");

  const multicore_field::Spatial<REAL4> arg(field);

  calc::Spatial* field_result = new calc::Spatial(VS_S, calc::CRI_f, nr_cells());
  multicore_field::Spatial<REAL4> result(field_result);

  fa::ExecutionPolicy epol = execution_policy();

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<REAL4>>;
  InputNoDataPolicy input_no_data_policy{{arg}};

  SpatialSetNoData<REAL4> output_no_data_policy(result);

  fern::Kernel<REAL4> runtime_kernel(radius, static_cast<REAL4>(1.0));

  fa::convolution::convolve<
    fa::convolve::SkipNoData,
    DivideOrNot,
    fa::convolve::SkipOutOfImage,
    fa::convolve::ReplaceNoDataFocusElement,
    fa::convolve::OutOfRangePolicy>(
      input_no_data_policy, output_no_data_policy,
      epol, arg, runtime_kernel, result);

  return field_result;
}

} // namespace detail


calc::Field* windowtotal(
         calc::Field * field,
         size_t radius) {
    return detail::windowtot_avg<fa::convolve::DontDivideByWeights>(field, radius);

}

calc::Field* windowaverage(
         calc::Field * field,
         size_t radius) {

    return detail::windowtot_avg<fa::convolve::DivideByWeights>(field, radius);
}


} // namespace python
} // namespace pcraster_multicore
