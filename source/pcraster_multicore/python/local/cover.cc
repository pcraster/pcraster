#include "pcraster_multicore/python/local/cover.h"

#include <boost/python.hpp>

// PCRaster
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "calc_vs.h"


// Field wrapper
#include "pcraster_multicore/wrapper/datatype_customization_points/multicore_spatial.h"
#include "pcraster_multicore/wrapper/datatype_traits/multicore_spatial.h"
#include "pcraster_multicore/wrapper/argument_traits/multicore_spatial.h"

#include "pcraster_multicore/wrapper/datatype_customization_points/multicore_nonspatial.h"
#include "pcraster_multicore/wrapper/datatype_traits/multicore_nonspatial.h"
#include "pcraster_multicore/wrapper/argument_traits/multicore_nonspatial.h"

#include "pcraster_multicore/python/execution_policy.h"
#include "pcraster_multicore/python/local/utils.h"
#include "pcraster_multicore/python/type_conversion/boolean.h"

// Fern
#include "fern/algorithm/policy/policies.h"
#include "fern/algorithm/core/cover.h"


namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {


template<class T>
void cover_nonspatial(
         fa::ExecutionPolicy& epol,
         multicore_field::Spatial<T>& res,
         multicore_field::Nonspatial<T> const& arg2) {

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<T>,
        NonspatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{res}, {arg2}};

  SpatialSetNoData<T> output_no_data_policy(res);

  fa::core::cover(input_no_data_policy,
    output_no_data_policy, epol, res, arg2, res);
}


template<class T>
void cover_spatial(
         fa::ExecutionPolicy& epol,
         multicore_field::Spatial<T>& res,
         multicore_field::Spatial<T> const& arg2){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<T>,
        SpatialDetectNoData<T>>;

  InputNoDataPolicy input_no_data_policy{{res}, {arg2}};

  SpatialSetNoData<T> output_no_data_policy(res);

  fa::core::cover(input_no_data_policy,
    output_no_data_policy, epol, res, arg2, res);
}


template<class T>
calc::Field* cover(std::vector<calc::Field*> const&  field_arguments){

  calc::Field* res_field = (field_arguments.at(0))->createClone();
  multicore_field::Spatial<T> res(res_field);

  fa::ExecutionPolicy epol = execution_policy();

  for(size_t idx = 1; idx < field_arguments.size(); ++idx){
    // we assume that the first field is always spatial
    // i.e. spatial argument, or spatial result of previous cover
    if(field_arguments.at(idx)->isSpatial() == true){
      const multicore_field::Spatial<T> arg2(field_arguments.at(idx));
      cover_spatial<T>(epol, res, arg2);
    }
    else{
      const multicore_field::Nonspatial<T> arg2(field_arguments.at(idx));
      cover_nonspatial<T>(epol, res, arg2);
    }
  }
  return res.getField();
}


calc::Field* spatial_safe_bool(calc::Field* argument){
  double value = 0;
  argument->getCell(value, 0);

  INT4 nominal_val = static_cast<INT4>(value);

  if(!(0 <= nominal_val && nominal_val <= 1)){
    throw std::runtime_error("value '" + std::to_string(nominal_val) + "' is of type nominal or ordinal, must be boolean");

  }

  UINT1 cellvalue = nominal_val != static_cast<UINT1>(0) ? 1 : 0;

  calc::Field* res_field = new calc::Spatial(VS_B, calc::CRI_1, nr_cells());

  UINT1* cells = static_cast<UINT1*>(res_field->dest());
  std::memset(cells, cellvalue, nr_cells());

  return res_field;

}


calc::Field* spatial_int4(calc::Field* argument){
  double value = 0;
  argument->getCell(value, 0);

  INT4 cellvalue = static_cast<INT4>(value);

  calc::Field* res_field = new calc::Spatial(VS_O, calc::CRI_4, nr_cells());

  for(size_t idx = 0; idx < nr_cells(); ++idx){
    res_field->setCell(cellvalue, idx);
  }

  return res_field;

}


} // namespace detail



calc::Field* cover(boost::python::list const& arguments){

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

  bool is_boolean = false;
  bool is_nominal = false;
  bool is_ordinal = false;
  bool is_scalar = false;

  // First spatial argument will determine the return datatype
  for(size_t idx = 0; idx < nr_args; ++idx){
    if(field_arguments.at(idx)->isSpatial() == true && boolean_valuescale(*field_arguments.at(idx))){
      is_boolean = true;
      break;
    }
    if(field_arguments.at(idx)->isSpatial() == true && nominal_valuescale(*field_arguments.at(idx))){
      is_nominal = true;
      break;
    }
    if(field_arguments.at(idx)->isSpatial() == true && ordinal_valuescale(*field_arguments.at(idx))){
      is_ordinal = true;
      break;
    }
    if(field_arguments.at(idx)->isSpatial() == true && scalar_valuescale(*field_arguments.at(idx))){
      is_scalar = true;
      break;
    }
  }

  if(is_scalar) {
    // cast nonspatial nominals (integers) to scalar
    for(size_t idx = 0; idx < nr_args; ++idx){
      if(field_arguments.at(idx)->isSpatial() == false){
        if(nominal_valuescale(*field_arguments.at(idx))){
          field_arguments.at(idx) = to_scalar(field_arguments.at(idx));
        }
      }
      assert_scalar_valuescale(*field_arguments.at(idx), "argument nr " + std::to_string(idx + 1));
    }
    return detail::cover<REAL4>(field_arguments);
  }
  else if(is_nominal){
    if(field_arguments.at(0)->isSpatial() == false){
      field_arguments.at(0) = detail::spatial_int4(field_arguments.at(0));
    }
    for(size_t idx = 0; idx < nr_args; ++idx){
      assert_nominal_valuescale(*field_arguments.at(idx), "argument nr " + std::to_string(idx + 1));
    }
    return detail::cover<INT4>(field_arguments);
  }
  else if(is_ordinal) {
    // If first argument is nonspatial nominal: cast to spatial ordinal
    if(field_arguments.at(0)->isSpatial() == false){
      if(nominal_valuescale(*field_arguments.at(0))){
          field_arguments.at(0) = detail::spatial_int4(field_arguments.at(0));
      }
    }
    for(size_t idx = 0; idx < nr_args; ++idx){
      // Nominal nonspatials (integers) can be casted to ordinals
      if(field_arguments.at(idx)->isSpatial() == false){
        if(nominal_valuescale(*field_arguments.at(idx))){
          field_arguments.at(idx) = to_ordinal(field_arguments.at(idx));
        }
      }
      assert_ordinal_valuescale(*field_arguments.at(idx), "argument nr " + std::to_string(idx + 1));
    }
    return detail::cover<INT4>(field_arguments);
  }
  else if(is_boolean) {
    // If first argument is nonspatial nominal (either 0 or 1): cast to boolean spatial
    if(field_arguments.at(0)->isSpatial() == false){
      if(nominal_valuescale(*field_arguments.at(0))){
          field_arguments.at(0) = detail::spatial_safe_bool(field_arguments.at(0));
      }
    }
    // Cast nonspatial nominal arguments to boolean where possible
    for(size_t idx = 0; idx < nr_args; ++idx){
      if(field_arguments.at(idx)->isSpatial() == false){
        // only 0 and 1 may be casted to boolean
        if(nominal_valuescale(*field_arguments.at(idx))){
          field_arguments.at(idx) = safe_boolean(field_arguments.at(idx));
        }
      }
      assert_boolean_valuescale(*field_arguments.at(idx), "argument nr " + std::to_string(idx + 1));
    }
    return detail::cover<UINT1>(field_arguments);
  }
  else if(directional_valuescale(*field_arguments.at(0))){
    // directional should be covered by wrapper
    // just bail out to be sure
    throw std::runtime_error("internal error: unable to perform operation");
  }

  assert(false);
  return nullptr;
}


} // namespace python
} // namespace pcraster_multicore

