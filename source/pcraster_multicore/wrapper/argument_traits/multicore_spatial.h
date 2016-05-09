#pragma once

#include "fern/algorithm/core/argument_traits.h"
#include "pcraster_multicore/wrapper/multicore_spatial_input_policy.h"
#include "pcraster_multicore/wrapper/multicore_spatial_output_policy.h"
#include "pcraster_multicore/wrapper/multicore_spatial.h"

namespace fern {
namespace algorithm {

template<class T>
struct ArgumentTraits<multicore_field::Spatial<T>>
{

    using Mask = multicore_field::Spatial<T>;

    using InputNoDataPolicy = SpatialDetectNoData<T>;

    using OutputNoDataPolicy = SpatialSetNoData<T>;

};


} // namespace algorithm
} // namespace fern
