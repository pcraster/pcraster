#include "pcraster_multicore/python/local/defined.h"

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
#include "fern/core/data_customization_point/scalar.h"
#include "fern/algorithm/policy/policies.h"
#include "fern/algorithm/algebra/boole/defined.h"

namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {
namespace detail {


template<class T>
calc::Field* defined_number(
         const multicore_field::Nonspatial<T>* arg1,
         multicore_field::Nonspatial<UINT1>* res){

  fa::SequentialExecutionPolicy sequential;

  using InputNoDataPolicy = fa::InputNoDataPolicies<NonspatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*arg1}};

  fa::algebra::defined(input_no_data_policy, sequential, *res);

  return res->getField();
}


template<class T>
calc::Field* defined_spatial(
         fa::ExecutionPolicy& epol,
         const multicore_field::Spatial<T>* arg1,
         multicore_field::Spatial<UINT1>* res){

  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<T>>;
  InputNoDataPolicy input_no_data_policy{{*arg1}};

  fa::algebra::defined(input_no_data_policy, epol, *res);

  return res->getField();
}


} // namespace detail


calc::Field* defined(
         calc::Field* field_a){

  assert_equal_location_attributes(*field_a);

  calc::Field* res_field = nullptr;

  CSF_CR cell_representation = field_a->cr();

  if(field_a->isSpatial() == false){
    res_field = new calc::NonSpatial(VS_B);
    multicore_field::Nonspatial<UINT1> res(res_field);

    switch(cell_representation) {
      case CR_UINT1:{
        const multicore_field::Nonspatial<UINT1> arg(field_a);
        return detail::defined_number<UINT1>(&arg, &res);
      }
      case CR_INT4:{
        const multicore_field::Nonspatial<INT4> arg(field_a);
        return detail::defined_number<INT4>(&arg, &res);
      }
      case CR_REAL4:{
        const multicore_field::Nonspatial<REAL4> arg(field_a);
        return detail::defined_number<REAL4>(&arg, &res);
      }
      default: {
        throw std::runtime_error("internal error: unable to perform operation");
      }
    }
  }

  res_field = new calc::Spatial(VS_B, calc::CRI_1, nr_cells());
  multicore_field::Spatial<UINT1> res(res_field);

  fa::ExecutionPolicy epol = execution_policy();

  switch(cell_representation) {
    case CR_UINT1:{
      const multicore_field::Spatial<UINT1> arg(field_a);
      return detail::defined_spatial<UINT1>(epol, &arg, &res);
    }
    case CR_INT4:{
      const multicore_field::Spatial<INT4> arg(field_a);
      return detail::defined_spatial<INT4>(epol, &arg, &res);
    }
    case CR_REAL4:{
      const multicore_field::Spatial<REAL4> arg(field_a);
      return detail::defined_spatial<REAL4>(epol, &arg, &res);
    }
    default: {
      throw std::runtime_error("internal error: unable to perform operation");
      break;
    }
  }
}


} // namespace python
} // namespace pcraster_multicore

