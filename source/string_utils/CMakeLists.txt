set(TEST_SOURCES
    dev_Test.cc
    dev_ToStringTest.cc
)
add_executable(test_string_utils
    ${TEST_SOURCES}
)
add_test(string_utils ${EXECUTABLE_OUTPUT_PATH}/test_string_utils)
add_custom_target(run_test_string_utils
    test_string_utils
    DEPENDS test_string_utils
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
add_dependencies(tests run_test_string_utils)
