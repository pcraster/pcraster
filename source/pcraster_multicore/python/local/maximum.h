#pragma once

#include <vector>



namespace calc {
  class Field;
}




namespace pcraster_multicore::python {


calc::Field* maximum(std::vector<calc::Field*> const& arguments);


} // namespace pcraster_multicore::python


