#pragma once
#include "com_csfcell.h"
#include <type_traits>


namespace com {

// In the code below, PCRASTER_MV_PROPAGATES is used to select a more
// efficient branch. This symbol is not defined yet. If necessary, add
// tests to the CMake scripts to make this symbol dependent on the
// platform.

template<
    typename T1,
    typename T2>
inline void inplace_add(
    T1& lhs,
    T2 const rhs)
{
    static_assert(std::is_same<T1, T2>::value, "");
    static_assert(std::is_floating_point<T1>::value, "");
    static_assert(sizeof(T1) == 4 || sizeof(T2) == 8, "");

// #ifdef PCRASTER_MV_PROPAGATES
//     lhs += rhs;
// #else
    if(!pcr::isMV(lhs)) {
        if(!pcr::isMV(rhs)) {
            lhs += rhs;
        }
        else {
            pcr::setMV(lhs);
        }
    }
// #endif
}


template<
    typename T1,
    typename T2>
inline void inplace_subtract(
    T1& lhs,
    T2 const rhs)
{
    static_assert(std::is_same<T1, T2>::value, "");
    static_assert(std::is_floating_point<T1>::value, "");
    static_assert(sizeof(T1) == 4 || sizeof(T1) == 8, "");

// #ifdef PCRASTER_MV_PROPAGATES
//     lhs -= rhs;
// #else
    if(!pcr::isMV(lhs)) {
        if(!pcr::isMV(rhs)) {
            lhs -= rhs;
        }
        else {
            pcr::setMV(lhs);
        }
    }
// #endif
}


template<
    typename T1,
    typename T2>
inline T1 add(
    T1 const lhs,
    T2 const rhs)
{
    T1 result = lhs;
    inplace_add(result, rhs);
    return result;
}


template<
    typename T1,
    typename T2>
inline T1 subtract(
    T1 const lhs,
    T2 const rhs)
{
    T1 result = lhs;
    inplace_subtract(result, rhs);
    return result;
}

} // namespace com
