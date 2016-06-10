#include "pcraster_multicore/python/focal/slope.h"

#include <stdexcept>

// PCRaster
#include "calc_spatial.h"
#include "Globals.h"
#include "appargs.h"


// Field wrapper
#include "pcraster_multicore/wrapper/argument_customization_points/multicore_spatial.h"
#include "pcraster_multicore/wrapper/datatype_customization_points/multicore_spatial.h"
#include "pcraster_multicore/wrapper/datatype_traits/multicore_spatial.h"
#include "pcraster_multicore/wrapper/argument_traits/multicore_spatial.h"

#include "pcraster_multicore/python/execution_policy.h"
#include "pcraster_multicore/python/local/utils.h"

// Fern
#include "fern/algorithm/policy/policies.h"
#include "fern/algorithm/space/focal/slope.h"



namespace fa = fern::algorithm;


namespace pcraster_multicore {
namespace python {


calc::Field* slope(
         calc::Field * field_dem){

  assert_equal_location_attributes(*field_dem);

  if(appUnitTrue == false){
    throw std::runtime_error("not implemented for global option 'unitcell'\n");
  }

  if(field_dem->isSpatial() == false){
    throw std::runtime_error("argument is non-spatial, only spatial is allowed\n");
  }

  if(boolean_valuescale(*field_dem)){
    throw std::runtime_error("argument nr. 1 of function 'slope': type is boolean, legal type is scalar\n");
  }

  calc::Spatial* field_result = new calc::Spatial(VS_S, calc::CRI_f, nr_cells());
  multicore_field::Spatial<REAL4> result(field_result);

  fa::ExecutionPolicy epol = execution_policy();

  const multicore_field::Spatial<REAL4> arg(field_dem);
  using InputNoDataPolicy = fa::InputNoDataPolicies<SpatialDetectNoData<REAL4>>;
  InputNoDataPolicy input_no_data_policy{{arg}};

  SpatialSetNoData<REAL4> output_no_data_policy(result);

  fa::space::slope<fa::slope::OutOfRangePolicy>(
        input_no_data_policy, output_no_data_policy,
        epol, arg, result);

  return result.getField();
}


} // namespace python
} // namespace pcraster_multicore

