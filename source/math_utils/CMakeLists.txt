add_library(math_utils INTERFACE)

add_library(pcraster::math_utils ALIAS math_utils)

target_include_directories(math_utils
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        Boost::tuple
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE math_utils
        NAMES
            dev_MathUtilsTest
        LINK_LIBRARIES
            pcraster::math_utils
    )
endif()
