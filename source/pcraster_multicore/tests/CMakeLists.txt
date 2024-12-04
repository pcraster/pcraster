configure_file(
    test_prolog.py.in
    test_prolog.py
    @ONLY
    NEWLINE_STYLE UNIX
)

add_test(
    NAME setup_multicore
    COMMAND ${Python_EXECUTABLE} test_prolog.py
)

set_tests_properties(setup_multicore
    PROPERTIES
        FIXTURES_SETUP
            FixtureMulticore
)

add_test(
    NAME
        python_multicore_unit_tests
    COMMAND
        ${Python_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
)

set_tests_properties(python_multicore_unit_tests
    PROPERTIES
        DEPENDS
            _pcraster_multicore
        FIXTURES_REQUIRED
            FixtureMulticore
)

# Environment variables to execute this unit test via ctest properly
set(test_env
    PATH=path_list_prepend:$<$<PLATFORM_ID:Windows>:$<TARGET_FILE_DIR:pcrcalc>>
    PYTHONPATH=path_list_prepend:${CMAKE_CURRENT_BINARY_DIR}
    PYTHONPATH=path_list_prepend:$<TARGET_FILE_DIR:pcrcalc>
    PCRASTER_NR_WORKER_THREADS=set:2
)

set_tests_properties(python_multicore_unit_tests
    PROPERTIES
        ENVIRONMENT_MODIFICATION
            "${test_env}"
)
