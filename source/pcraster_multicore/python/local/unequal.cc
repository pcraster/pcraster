#include "pcraster_multicore/python/local/unequal.h"


// PCRaster
#include "calc_spatial.h"
#include "calc_nonspatial.h"


// Field wrapper
#include "pcraster_multicore/python/local/utils.h"
#include "pcraster_multicore/python/local/not.h"
#include "pcraster_multicore/python/local/equal.h"



namespace pcraster_multicore {
namespace python {


calc::Field* unequal(
         calc::Field* field_a,
         calc::Field* field_b){

  calc::Field* eq = equal(field_a, field_b);
  calc::Field* res = _not(eq);
  delete eq;

  return res;
}

} // namespace python
} // namespace pcraster_multicore

