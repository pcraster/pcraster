configure_file(
    test_prolog.py.in
    test_prolog.py
    @ONLY
    NEWLINE_STYLE UNIX
)

add_test(
    NAME setup_multicore
    COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
)

set_tests_properties(setup_multicore
    PROPERTIES
        FIXTURES_SETUP FixtureMulticore
)

add_test(
    NAME
        python_multicore_unit_tests
    COMMAND
        ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
)

set_tests_properties(python_multicore_unit_tests
    PROPERTIES
        DEPENDS
            _pcraster_multicore
        FIXTURES_REQUIRED
            FixtureMulticore
)

# add path of project's unit test related files to PYTHONPATH
# required to execute this unit test via ctest properly
if(WIN32)
     set(ENV_PATH $ENV{PATH})
     string(REPLACE ";" "\\;" ENV_PATH "${ENV_PATH}")
     # argument is a 'cmake' ; separated list of values, therefore escape the environment path separator
     set_tests_properties(python_multicore_unit_tests PROPERTIES
       ENVIRONMENT "PATH=${ENV_PATH}\\;${EXECUTABLE_OUTPUT_PATH};PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}\\;${EXECUTABLE_OUTPUT_PATH};PCRASTER_NR_WORKER_THREADS=2"
)
else()
    set_tests_properties(python_multicore_unit_tests PROPERTIES
      ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:${EXECUTABLE_OUTPUT_PATH};PCRASTER_NR_WORKER_THREADS=2"
    )
endif()
