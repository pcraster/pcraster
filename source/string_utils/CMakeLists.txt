add_library(string_utils INTERFACE)

add_library(pcraster::string_utils ALIAS string_utils)

target_include_directories(string_utils
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE string_utils
        NAMES
            dev_ToStringTest
        LINK_LIBRARIES
            pcraster::string_utils
    )
endif()
