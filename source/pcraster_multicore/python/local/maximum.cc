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
#include "pcraster_multicore/python/type_conversion/scalar.h"

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

  // Casting of POD to a PCRaster data type
  // To determine the intended datatype, we check the spatial arguments
  // as POD are just mapped to nominal or scalars in the Python wrapper.
  // Depending on the spatial data types, we cast int where appropriate
  bool is_ordinal = false;
  bool is_scalar = false;
  bool all_nonspatial = true;

  for(size_t idx = 0; idx < nr_args; ++idx){
    field_arguments.push_back(boost::python::extract<calc::Field*>(arguments[idx]));
    // arguments must have same extent as clone
    assert_equal_location_attributes(*field_arguments.at(idx));

    // Just test the spatials
    if(field_arguments.at(idx)->isSpatial() == true){
      if(ordinal_valuescale(*field_arguments.at(idx))){
        is_ordinal = true;
        all_nonspatial = false;
      }
      else if(scalar_valuescale(*field_arguments.at(idx))) {
        is_scalar = true;
        all_nonspatial = false;
      }

      if(directional_valuescale(*field_arguments.at(idx))){
        throw std::runtime_error("argument " + std::to_string(idx + 1) + " is of type 'directional'; allowed type is 'scalar' or 'ordinal'");
      }
      if(ldd_valuescale(*field_arguments.at(idx))){
        throw std::runtime_error("argument " + std::to_string(idx + 1) + " is of type 'ldd'; allowed type is 'scalar' or 'ordinal'");
      }
      if(nominal_valuescale(*field_arguments.at(idx))){
        throw std::runtime_error("argument " + std::to_string(idx + 1) + " is of type 'nominal'; allowed type is 'scalar' or 'ordinal'");
      }
      if(boolean_valuescale(*field_arguments.at(idx))){
        throw std::runtime_error("argument " + std::to_string(idx + 1) + " is of type 'boolean'; allowed type is 'scalar' or 'ordinal'");
      }
    }
  }

  // In case someone runs into this: modify the wrapper and forward this to the PCRaster max
  if(all_nonspatial == true){
    throw std::runtime_error("non-spatial result type is not implemented; use at least one spatial argument");
  }

  if(is_ordinal == true){
    for(size_t idx = 0; idx < nr_args; ++idx){
      // Data type mixing is not allowed
      if(scalar_valuescale(*field_arguments.at(idx))){
        throw std::runtime_error("argument " + std::to_string(idx + 1) + " is of type 'scalar'; allowed type is 'ordinal'");
       }
    }
    return detail::maximum<INT4>(field_arguments);
  }
  else if(is_scalar == true){
    for(size_t idx = 0; idx < nr_args; ++idx){
      // Data type mixing is not allowed
      if(ordinal_valuescale(*field_arguments.at(idx))){
        throw std::runtime_error("argument " + std::to_string(idx + 1) + " is of type 'ordinal'; allowed type is 'scalar'");
      }
      // In case we got an integer (nominal) as argument we can cast to scalar
      if(nominal_valuescale(*field_arguments.at(idx))){
        field_arguments.at(idx) = to_scalar(field_arguments.at(idx));
      }
    }

    return detail::maximum<REAL4>(field_arguments);
  }

  assert(false);
  return nullptr;
}

} // namespace python
} // namespace pcraster_multicore

