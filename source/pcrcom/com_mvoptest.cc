#define BOOST_TEST_MODULE pcraster com mv_op
#include <boost/test/unit_test.hpp>
#include "com_mvop.h"


// template<typename T>
// static std::string asStr(char const* bits)
// {
//    std::string res;
// 
//    UINT4 *v=(UINT4 *)bits;
//    std::bitset<32> bs(v[0]);
//    res = bs.to_string();
//    if (sizeof(T) == 8) {
//      bs = std::bitset<32>(v[1]);
//      res += bs.to_string();
//    }
//    return res;
// }

// BOOST_CHECK_EQUAL(asStr<T>(fr.debugBits()), std::string("11111111"));
// BOOST_CHECK_EQUAL(asStr<T>(R(f).debugBits()),std::string("11111111"));


template<
    typename T>
void test_assignment()
{
    {
        T argument;
        pcr::setMV(argument);
        auto result = argument;
        BOOST_CHECK(pcr::isMV(result));
    }
}


template<
    typename T>
void test_add()
{
    // 3 + 4
    {
        T argument1 = 3;
        T argument2 = 4;
        auto result = com::add<T>(argument1, argument2);
        BOOST_CHECK(!pcr::isMV(result));
        BOOST_CHECK_EQUAL(result, 7);
    }

    // mv + 4
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument1);
        auto result = com::add<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(result));
    }

    // 3 + mv
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument2);
        auto result = com::add<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(result));
    }

    // mv + mv
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument1);
        pcr::setMV(argument2);
        auto result = com::add<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(result));
    }
}


template<
    typename T>
void test_inplace_add()
{
    // 3 += 4
    {
        T argument1 = 3;
        T argument2 = 4;
        com::inplace_add<T>(argument1, argument2);
        BOOST_CHECK(!pcr::isMV(argument1));
        BOOST_CHECK_EQUAL(argument1, 7);
    }

    // mv += 4
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument1);
        com::inplace_add<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(argument1));
    }

    // 3 += mv
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument2);
        com::inplace_add<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(argument1));
    }

    // mv += mv
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument1);
        pcr::setMV(argument2);
        com::inplace_add<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(argument1));
    }
}


template<
    typename T>
void test_subtract()
{
    // 3 - 4
    {
        T argument1 = 3;
        T argument2 = 4;
        auto result = com::subtract<T>(argument1, argument2);
        BOOST_CHECK(!pcr::isMV(result));
        BOOST_CHECK_EQUAL(result, -1);
    }

    // mv - 4
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument1);
        auto result = com::subtract<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(result));
    }

    // 3 - mv
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument2);
        auto result = com::subtract<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(result));
    }

    // mv - mv
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument1);
        pcr::setMV(argument2);
        auto result = com::subtract<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(result));
    }
}


template<
    typename T>
void test_inplace_subtract()
{
    // 3 -= 4
    {
        T argument1 = 3;
        T argument2 = 4;
        com::inplace_subtract<T>(argument1, argument2);
        BOOST_CHECK(!pcr::isMV(argument1));
        BOOST_CHECK_EQUAL(argument1, -1);
    }

    // mv -= 4
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument1);
        com::inplace_subtract<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(argument1));
    }

    // 3 -= mv
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument2);
        com::inplace_subtract<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(argument1));
    }

    // mv -= mv
    {
        T argument1 = 3;
        T argument2 = 4;
        pcr::setMV(argument1);
        pcr::setMV(argument2);
        com::inplace_subtract<T>(argument1, argument2);
        BOOST_CHECK(pcr::isMV(argument1));
    }
}


template<
    typename T>
void test_operations_()
{
    test_assignment<T>();
    test_add<T>();
    test_inplace_add<T>();
    test_subtract<T>();
    test_inplace_subtract<T>();
}


BOOST_AUTO_TEST_CASE(test_operations)
{
  test_operations_<float>();
  test_operations_<double>();
}
