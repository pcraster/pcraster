#pragma once

#include <cassert>
#include <cmath>
#include "calc_spatial.h"
#include "fern/core/type_traits.h"


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
class OutOfTangensDirectionalDomainPolicy
{

public:

  inline static bool within_domain(
        Value const& value)
  {

    if(!std::isfinite(value)) {
      return false;
    }

    if(value < 0.0 || value >= 360.0){
      return false;
    }

    Value remainder = std::remainder(value, fern::half_pi<Value>());

    if(remainder != Value(0)){
      // Value is not divisable by a whole number of times 0.5 * pi.
      return true;
    }
    else{
      // if whole number it must not be odd
      Value quotient = value / fern::half_pi<Value>();

      return int64_t(quotient) % 2 == 0;
    }
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




template<
    typename Base,
    typename Exponent>
class OutOfPowerDomainPolicy
{

public:

    inline static bool within_domain(
        Base const& base,
        Exponent const& exponent)
    {

        if(base == Base(0) && exponent <= Exponent(0)) {
            return false;
        }
        else if(base < Base(0)) {
            Base integral, fractional;
            fractional = std::modf(exponent, &integral);

            if(fractional != Base(0)) {
                return false;
            }

//             if(integral < Base(0)) {
//                 return false;
//             }
        }

        return true;
    }

};












} // namespace python
} // namespace pcraster_multicore

