add_library(pcrfunc INTERFACE)

add_library(pcraster::pcrfunc ALIAS pcrfunc)

target_include_directories(pcrfunc
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE pcrfunc
        NAMES
            func_assigntest
            func_booleanoperatorstest
            func_setmvtest
            func_summarystatisticstest
        LINK_LIBRARIES
            pcraster::dal
    )
endif()
