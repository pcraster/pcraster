#include "pcraster_multicore/python/type_conversion/nonspatial_field.h"


// PCRaster
#include "calc_nonspatial.h"
#include "pcrtypes.h"

// Field wrapper
#include "pcraster_multicore/wrapper/datatype_customization_points/multicore_nonspatial.h"
#include "pcraster_multicore/wrapper/datatype_traits/multicore_nonspatial.h"
#include "pcraster_multicore/wrapper/argument_traits/multicore_nonspatial.h"



namespace pcraster_multicore {
namespace python {


// the PCRaster newnonspatialfield return with multiple valuescales based on the input value
// that will break the value scale checking in the multicore algorithms
// just make our own ones
calc::Field* newNonSpatialScalar(const double value){
  return new calc::NonSpatial(VS_S, static_cast<REAL4>(value));
}

calc::Field* newNonSpatialNominal(const int value){
  return new calc::NonSpatial(VS_N, static_cast<INT4>(value));
}

calc::Field* newNonSpatialBoolean(const bool value){
  return new calc::NonSpatial(VS_B, static_cast<UINT1>(value));
}


} // namespace python
} // namespace pcraster_multicore

