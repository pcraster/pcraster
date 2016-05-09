#pragma once

#include "pcraster_multicore/wrapper/multicore_spatial.h"
#include "fern/algorithm/core/argument_customization_point.h"

namespace fern {
namespace algorithm {

template<typename T>
inline multicore_field::Spatial<T>& mask(
    multicore_field::Spatial<T>& argument){
    return argument;
}

} // namespace algorithm
} // namespace fern
