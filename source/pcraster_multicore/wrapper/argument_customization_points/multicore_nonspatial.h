#pragma once

#include "fern/algorithm/core/argument_customization_point.h"

namespace fern {
namespace algorithm {

template<typename T>
inline multicore_field::Nonspatial<T>& mask(
    multicore_field::Nonspatial<T>& argument)
{
    return argument;
}

} // namespace algorithm
} // namespace fern
