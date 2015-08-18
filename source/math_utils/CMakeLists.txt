set(TEST_SOURCES
    dev_MathUtilsTest.cc
    dev_Test.cc
)
add_executable(test_math_utils
    ${TEST_SOURCES}
  )
add_test(math_utils ${EXECUTABLE_OUTPUT_PATH}/test_math_utils)
add_custom_target(run_test_math_utils
    test_math_utils
    DEPENDS test_math_utils
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
add_dependencies(tests run_test_math_utils)
