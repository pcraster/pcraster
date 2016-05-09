#pragma once

#include <cassert>
#include "calc_spatial.h"


namespace pcraster_multicore {
namespace python {


template<
    typename Value>
class OutOfDirectionalDomainPolicy
{

public:

    inline static bool within_domain(
        Value const& value)
    {
      return std::isfinite(value) && 0.0 <= value && value < 360.0;
    }

};


template<
    typename Value>
class OutOfLogDomainPolicy
{

public:

    inline static bool within_domain(
        Value const& value)
    {
        return value > Value(0);
    }

};


} // namespace python
} // namespace pcraster_multicore

