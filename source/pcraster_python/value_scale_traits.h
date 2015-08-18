#pragma once
#include <boost/cstdint.hpp>
#include <boost/integer_traits.hpp>
#include "calc_cr.h"


namespace pcraster {

template<
    VS value_scale>
struct ValueScaleTraits
{
};


template<>
struct ValueScaleTraits<VS_B>
{
    static std::string const name;
    static calc::CRIndex const cell_representation_index = calc::CRI_1;
    typedef boost::uint8_t Type;
    static Type const minimum = 0;
    static Type const maximum = 1;
};


template<>
struct ValueScaleTraits<VS_L>
{
    static std::string const name;
    static calc::CRIndex const cell_representation_index = calc::CRI_1;
    typedef boost::uint8_t Type;
    static Type const minimum = 1;
    static Type const maximum = 9;
};


template<>
struct ValueScaleTraits<VS_N>
{
    static std::string const name;
    static calc::CRIndex const cell_representation_index = calc::CRI_4;
    typedef boost::int32_t Type;
    static Type const minimum = boost::integer_traits<Type>::const_min + 1;
    static Type const maximum = boost::integer_traits<Type>::const_max;
};


template<>
struct ValueScaleTraits<VS_O>
{
    static std::string const name;
    static calc::CRIndex const cell_representation_index = calc::CRI_4;
    typedef boost::int32_t Type;
    static Type const minimum = boost::integer_traits<Type>::const_min + 1;
    static Type const maximum = boost::integer_traits<Type>::const_max;
};


template<>
struct ValueScaleTraits<VS_S>
{
    static std::string const name;
    static calc::CRIndex const cell_representation_index = calc::CRI_f;
    typedef float Type;
    static Type const minimum;
    static Type const maximum;
};


template<>
struct ValueScaleTraits<VS_D>
{
    static std::string const name;
    static calc::CRIndex const cell_representation_index = calc::CRI_f;
    typedef float Type;
    static Type const minimum;
    static Type const maximum;
};

} // namespace pcraster
