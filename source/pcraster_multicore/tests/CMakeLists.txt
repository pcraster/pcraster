CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
    NEWLINE_STYLE UNIX
)

add_test(
    NAME
        python_multicore_unit_tests
    COMMAND
        ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
)

set_tests_properties(python_multicore_unit_tests PROPERTIES
  DEPENDS
      _pcraster_multicore
)

# add path of project's unit test related files to PYTHONPATH
# required to execute this unit test via ctest properly
if(WIN32)
    # argument is a 'cmake' ; separated list of values, therefore escape the environment path separator
    set_tests_properties(python_multicore_unit_tests PROPERTIES
      ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;${EXECUTABLE_OUTPUT_PATH}"
)
else()
    set_tests_properties(python_multicore_unit_tests PROPERTIES
      ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:${EXECUTABLE_OUTPUT_PATH}"
    )
endif()
