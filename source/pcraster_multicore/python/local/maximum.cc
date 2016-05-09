#include "pcraster_multicore/python/local/maximum.h"

#include <boost/python.hpp>

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
#include "fern/algorithm/statistic/binary_max.h"


namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {


template<class T>
void maximum_nonspatial(
         fa::ExecutionPolicy& epol,
         multicore_field::Spatial<T>& res,
         multicore_field::Nonspatial<T> const& arg2) {

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<T>,
        NonspatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{res}, {arg2}};

  SpatialSetNoData<T> output_no_data_policy(res);

  fa::statistic::binary_max(input_no_data_policy,
    output_no_data_policy, epol, res, arg2, res);
}


template<class T>
void maximum_spatial(
         fa::ExecutionPolicy& epol,
         multicore_field::Spatial<T>& res,
         multicore_field::Spatial<T> const& arg2){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<T>,
        SpatialDetectNoData<T>>;

  InputNoDataPolicy input_no_data_policy{{res}, {arg2}};

  SpatialSetNoData<T> output_no_data_policy(res);

  fa::statistic::binary_max(input_no_data_policy,
    output_no_data_policy, epol, res, arg2, res);
}


template<class T>
calc::Field* maximum(std::vector<calc::Field*> const&  field_arguments){

  calc::Field* res_field = (field_arguments.at(0))->createClone();

  multicore_field::Spatial<T> res(res_field);

  fa::ExecutionPolicy epol = execution_policy();

  for(size_t idx = 1; idx < field_arguments.size(); ++idx){
    // we assume that the first field is always spatial
    // i.e. spatial argument, or spatial result of previous maximum
    if(field_arguments.at(idx)->isSpatial() == true){
      const multicore_field::Spatial<T> arg2(field_arguments.at(idx));
      maximum_spatial<T>(epol, res, arg2);
    }
    else{
      const multicore_field::Nonspatial<T> arg2(field_arguments.at(idx));
      maximum_nonspatial<T>(epol, res, arg2);

    }
  }
  return res.getField();
}


} // namespace detail



calc::Field* maximum(boost::python::list const& arguments){

  size_t nr_args = boost::python::len(arguments);

  if(nr_args == 0){
    throw std::runtime_error("at least 1 argument required, 0 given\n");
  }

  if(nr_args == 1){
    return (static_cast<calc::Field*>(boost::python::extract<calc::Field*>(arguments[0])))->createClone();
  }

  std::vector<calc::Field *> field_arguments;

  for(size_t idx = 0; idx < nr_args; ++idx){
    field_arguments.push_back(boost::python::extract<calc::Field*>(arguments[idx]));
    // arguments must have same extent as clone
    assert_equal_location_attributes(*field_arguments.at(idx));
  }

  if(directional_valuescale(*field_arguments.at(0))){
    throw std::runtime_error("operation not implemented for type 'directional'");
  }
  if(ldd_valuescale(*field_arguments.at(0))){
    throw std::runtime_error("operation not implemented for type 'ldd'");
  }
  if(nominal_valuescale(*field_arguments.at(0))){
    throw std::runtime_error("operation not implemented for type 'nominal'");
  }
  if(boolean_valuescale(*field_arguments.at(0))){
    throw std::runtime_error("operation not implemented for type 'boolean'");
  }

  // implement this when requested
  // note: there are some checks later on that require the first argument
  // being spatial, this needs to be adapted
  if(field_arguments.at(0)->isSpatial() == false){
    throw std::runtime_error("non-spatial result type is not implemented for this operation");
  }

  // first argument will determine result type
  // all remaining arguments need to have that type as well...
  for(size_t idx = 0; idx < nr_args - 1; ++idx){
    std::stringstream msg{};
    msg << "argument " << idx + 2; // + 2 due to second argument tested, and human indexing
    assert_equal_valuescale(*field_arguments.at(idx), *field_arguments.at(idx + 1), msg.str());
  }

  if(ordinal_valuescale(*field_arguments.at(0))){
    return detail::maximum<INT4>(field_arguments);
  }
  else if(scalar_valuescale(*field_arguments.at(0))){
    return detail::maximum<REAL4>(field_arguments);
  }

  assert(false);
  return nullptr;
}

} // namespace python
} // namespace pcraster_multicore

