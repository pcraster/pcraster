include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
)

# SET(SOURCES
# )
# ADD_LIBRARY(pcrfunc
#   ${SOURCES}
# )


if(PCRASTER_BUILD_TEST)
    SET(TEST_NAMES
        func_assigntest
        func_booleanoperatorstest
        func_setmvtest
        func_summarystatisticstest
    )
    add_unit_tests(
        SCOPE pcrfunc
        NAMES ${TEST_NAMES}
    )
endif()
