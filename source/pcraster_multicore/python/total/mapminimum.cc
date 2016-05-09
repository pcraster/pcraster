#include "pcraster_multicore/python/total/mapminimum.h"

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
#include "fern/algorithm/statistic/unary_min.h"

namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {



template<class T>
calc::Field* mapminimum(
         calc::Field* field,
         VS result_vs){

  const multicore_field::Spatial<T> arg1(field);
  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{arg1}};

  calc::Field* res_field = nullptr;

  res_field = new calc::NonSpatial(result_vs);
  multicore_field::Nonspatial<T> res(res_field);

  NonspatialSetNoData<T> output_no_data_policy(res);

  fa::ExecutionPolicy epol = execution_policy();

  fa::statistic::unary_min(input_no_data_policy, output_no_data_policy, epol, arg1, res);

  return res.getField();
}


} // namespace detail


calc::Field* mapminimum(
         calc::Field* field){

  assert_equal_location_attributes(*field);

  if(field->isSpatial() == false){
    throw std::runtime_error("argument is non-spatial, only spatial allowed\n");
  }

  VS result_vs = field->vs();

  if(scalar_valuescale(*field) == true){
    return detail::mapminimum<REAL4>(field, result_vs);
  }
  else if(ordinal_valuescale(*field) == true){
    return detail::mapminimum<INT4>(field, result_vs);
  }
  else{
    std::stringstream msg{};
    msg << "argument is of type '" << field->vs() << "', legal type is either 'ordinal' or 'scalar'\n";
    throw std::runtime_error(msg.str());
  }
}


} // namespace python
} // namespace pcraster_multicore
