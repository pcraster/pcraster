include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
)

# SET(SOURCES
# )
SET(TEST_SOURCES
    func_assigntest.cc
    func_booleanoperatorstest.cc
    func_setmvtest.cc
    func_summarystatisticstest.cc
    func_test.cc
)
# ADD_LIBRARY(pcrfunc
#   ${SOURCES}
# )
ADD_EXECUTABLE(test_pcrfunc
    ${TEST_SOURCES}
)
ADD_CUSTOM_TARGET(run_test_pcrfunc
    COMMAND test_pcrfunc
    DEPENDS test_pcrfunc
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_pcrfunc)
