#ifndef INCLUDED_PYTHON_VALUE_SCALE_TRAITS
#define INCLUDED_PYTHON_VALUE_SCALE_TRAITS

#include "calc_cr.h"

#include <cstdint>
#include <limits>

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
    typedef std::uint8_t Type;
    static Type const minimum = 0;
    static Type const maximum = 1;
};


template<>
struct ValueScaleTraits<VS_L>
{
    static std::string const name;
    static calc::CRIndex const cell_representation_index = calc::CRI_1;
    typedef std::uint8_t Type;
    static Type const minimum = 1;
    static Type const maximum = 9;
};


template<>
struct ValueScaleTraits<VS_N>
{
    static std::string const name;
    static calc::CRIndex const cell_representation_index = calc::CRI_4;
    typedef std::int32_t Type;
    static Type const minimum = std::numeric_limits<Type>::min() + 1;
    static Type const maximum = std::numeric_limits<Type>::max();
};


template<>
struct ValueScaleTraits<VS_O>
{
    static std::string const name;
    static calc::CRIndex const cell_representation_index = calc::CRI_4;
    typedef std::int32_t Type;
    static Type const minimum = std::numeric_limits<Type>::min() + 1;
    static Type const maximum = std::numeric_limits<Type>::max();
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

#endif